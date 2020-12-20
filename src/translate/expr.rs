use std::collections::HashMap;

use crate::{
    lowering::{
        LExpression, LExpressionData, LGlobal, LLiteral, LMembers, LStatement, LStatementData,
        LVariableContext, LoopId, VariableId,
    },
    typechecker::{TType, Typechecker},
    util::{AResult, Id, TryCollectVec},
};

use super::{pattern::CPattern, CFunction, CFunctionId, Translator};

/// Variable captures when we're pushing a new stack frame. Each index is a pair
/// of an index from the previous stack frame, to an index of the (new) current
/// stack frame.
#[derive(Debug, Copy, Clone)]
pub struct CCaptures<'a>(pub &'a [(CStackId, CStackId)]);

/// Newtype for an index on the stack. It doesn't need to be newtyped, but is
/// just for good (type) measure.
#[derive(Debug, Copy, Clone)]
pub struct CStackId(pub usize);

#[derive(Debug, Copy, Clone)]
pub enum CExpression<'a> {
    Literal(CLiteral<'a>),
    Variable(CStackId),
    Block(&'a [CStatement<'a>], &'a CExpression<'a>),
    AsyncBlock(usize, CCaptures<'a>, &'a CExpression<'a>),
    Global(Id<LGlobal>),
    GlobalFunction(CFunctionId<'a>),
    Call(CFunctionId<'a>, &'a [CExpression<'a>]),
    Structure(&'a [(usize, CExpression<'a>)]),
    Allocation(&'a [(usize, CExpression<'a>)]),
    Variant(Id<str>, &'a [(usize, CExpression<'a>)]),
    Access(&'a CExpression<'a>, usize),
    Assign(&'a CExpression<'a>, &'a CExpression<'a>),
    Or(&'a CExpression<'a>, &'a CExpression<'a>),
    And(&'a CExpression<'a>, &'a CExpression<'a>),
    PollTrampoline(&'a CExpression<'a>),
    Return(&'a CExpression<'a>),
    Break(LoopId, &'a CExpression<'a>),
    Continue(LoopId),
    Closure(CCaptures<'a>, &'a CFunction<'a>),
    Loop(LoopId, &'a CExpression<'a>),
    Match(&'a CExpression<'a>, &'a [(CPattern<'a>, CExpression<'a>)]),
    If(
        &'a CExpression<'a>,
        &'a CExpression<'a>,
        &'a CExpression<'a>,
    ),
}

#[derive(Debug, Copy, Clone)]
pub enum CLiteral<'a> {
    True,
    False,
    String(&'a str),
    Int(i64),
    Float(f64),
    Char(char),
}

#[derive(Debug, Copy, Clone)]
pub enum CStatement<'a> {
    Let(CPattern<'a>, CExpression<'a>),
    Expression(CExpression<'a>),
}

impl<'a> Translator<'_, 'a> {
    pub fn translate_expr(
        &mut self,
        expr: Id<LExpression>,
        tyck: &mut Typechecker,

        slots: &HashMap<VariableId, CStackId>,
    ) -> AResult<CExpression<'a>> {
        let info = &*expr.lookup(self.ctx);

        let expr = match &info.data {
            LExpressionData::Literal(lit) => CExpression::Literal(self.translate_literal(*lit)),
            LExpressionData::Variable(v) => CExpression::Variable(slots[&v.id]),
            LExpressionData::Block(statements, expr) => CExpression::Block(
                self.translate_sub_stmts(statements, tyck, slots)?,
                self.translate_sub_expr(*expr, tyck, slots)?,
            ),
            LExpressionData::AsyncBlock(vcx, expr, _) => {
                let (slots, captures) = self.translate_captures(vcx, slots);
                CExpression::AsyncBlock(
                    slots.len(),
                    captures,
                    self.translate_sub_expr(*expr, tyck, &slots)?,
                )
            },
            LExpressionData::Global(id) => {
                let id = id.get(self.ctx);
                self.translate_global(id)?;
                CExpression::Global(id)
            },
            LExpressionData::GlobalFunction(fun, generics) => {
                let generics = self.translate_sub_tys(&generics, tyck)?;
                CExpression::GlobalFunction(self.translate_function(fun.get(self.ctx), generics)?)
            },
            LExpressionData::Access(expr, _, member, _) => {
                let idx =
                    if let TType::Object(obj, _) = &*tyck.satisfy_expr(*expr)?.lookup(self.ctx) {
                        if let LMembers::Named(_, _, members) = &obj.lookup(self.ctx).members {
                            members[member].0
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    };
                CExpression::Access(self.translate_sub_expr(*expr, tyck, slots)?, idx)
            },
            LExpressionData::IndexAccess(expr, _, idx, _) =>
                CExpression::Access(self.translate_sub_expr(*expr, tyck, slots)?, *idx),
            LExpressionData::Tuple(exprs) => {
                let members = exprs
                    .iter()
                    .enumerate()
                    .map(|(idx, expr)| -> AResult<_> {
                        Ok((idx, self.translate_expr(*expr, tyck, slots)?))
                    })
                    .try_collect_vec()?;
                CExpression::Structure(self.alloc.alloc_slice_copy(&members))
            },
            LExpressionData::StructConstructor(_, _, exprs) => {
                let members = exprs
                    .iter()
                    .map(|(idx, expr)| -> AResult<_> {
                        Ok((*idx, self.translate_expr(*expr, tyck, slots)?))
                    })
                    .try_collect_vec()?;
                CExpression::Structure(self.alloc.alloc_slice_copy(&members))
            },
            LExpressionData::ArrayLiteral(exprs, _) => {
                let members = exprs
                    .iter()
                    .enumerate()
                    .map(|(idx, expr)| -> AResult<_> {
                        Ok((idx, self.translate_expr(*expr, tyck, slots)?))
                    })
                    .try_collect_vec()?;
                CExpression::Allocation(self.alloc.alloc_slice_copy(&members))
            },
            LExpressionData::ObjectAllocation(_, _, exprs) => {
                let members = exprs
                    .iter()
                    .map(|(idx, expr)| -> AResult<_> {
                        Ok((*idx, self.translate_expr(*expr, tyck, slots)?))
                    })
                    .try_collect_vec()?;
                CExpression::Allocation(self.alloc.alloc_slice_copy(&members))
            },
            LExpressionData::EnumConstructor(_, _, discriminant, exprs) => {
                let members = exprs
                    .iter()
                    .map(|(idx, expr)| -> AResult<_> {
                        Ok((*idx, self.translate_expr(*expr, tyck, slots)?))
                    })
                    .try_collect_vec()?;
                CExpression::Variant(*discriminant, self.alloc.alloc_slice_copy(&members))
            },
            LExpressionData::Assign(left, right) => CExpression::Assign(
                self.translate_sub_expr(*left, tyck, slots)?,
                self.translate_sub_expr(*right, tyck, slots)?,
            ),
            LExpressionData::Or(left, right) => CExpression::Or(
                self.translate_sub_expr(*left, tyck, slots)?,
                self.translate_sub_expr(*right, tyck, slots)?,
            ),
            LExpressionData::And(left, right) => CExpression::And(
                self.translate_sub_expr(*left, tyck, slots)?,
                self.translate_sub_expr(*right, tyck, slots)?,
            ),
            LExpressionData::PollTrampoline(expr, _) =>
                CExpression::PollTrampoline(self.translate_sub_expr(*expr, tyck, slots)?),
            LExpressionData::Return(expr) =>
                CExpression::Return(self.translate_sub_expr(*expr, tyck, slots)?),
            LExpressionData::Break(id, expr) =>
                CExpression::Break(*id, self.translate_sub_expr(*expr, tyck, slots)?),
            LExpressionData::Continue(id) => CExpression::Continue(*id),
            LExpressionData::Closure(vcx, ps, _, expr) => {
                let (slots, captures) = self.translate_captures(vcx, slots);
                CExpression::Closure(
                    captures,
                    self.alloc.alloc(CFunction::Simple {
                        slots: slots.len(),
                        parameters: self.translate_sub_patterns(&ps, tyck, &slots)?,
                        body: self.translate_expr(*expr, tyck, &slots)?,
                    }),
                )
            },
            LExpressionData::If(cond, left, right) => CExpression::If(
                self.translate_sub_expr(*cond, tyck, slots)?,
                self.translate_sub_expr(*left, tyck, slots)?,
                self.translate_sub_expr(*right, tyck, slots)?,
            ),
            LExpressionData::Loop(id, expr, _) =>
                CExpression::Loop(*id, self.translate_sub_expr(*expr, tyck, slots)?),
            LExpressionData::Match(cond, branches) => {
                let branches = branches
                    .iter()
                    .map(|(pattern, expr)| -> AResult<_> {
                        Ok((
                            self.translate_pattern(*pattern, tyck, slots)?,
                            self.translate_expr(*expr, tyck, slots)?,
                        ))
                    })
                    .try_collect_vec()?;
                CExpression::Match(
                    self.translate_sub_expr(*cond, tyck, slots)?,
                    self.alloc.alloc_slice_copy(&branches),
                )
            },
            LExpressionData::Call(fun, generics, args) => {
                let generics = self.translate_sub_tys(&generics, tyck)?;
                CExpression::Call(
                    self.translate_function(fun.get(self.ctx), generics)?,
                    self.translate_sub_exprs(&args, tyck, slots)?,
                )
            },
            LExpressionData::StaticCall(_, ty, Some(trait_ty), name, fn_generic_tys, args, _) => {
                let ty_span = ty.lookup(self.ctx).span;
                let ty = tyck.initialize_ty(*ty, &btreemap! {})?;
                let trait_ty_span = trait_ty.lookup(self.ctx).span;
                let trait_ty = tyck.initialize_trait_ty(*trait_ty, &btreemap! {})?;

                let witness = tyck
                    .do_goal_trait(ty, ty_span, trait_ty, trait_ty_span, false)?
                    .expect("Expected no ambiguity here");
                let fn_generic_tys = self.translate_sub_tys(&fn_generic_tys, tyck)?;

                CExpression::Call(
                    self.translate_method(&witness, *name, fn_generic_tys, info.span)?,
                    self.translate_sub_exprs(&args, tyck, slots)?,
                )
            },
            LExpressionData::StaticCall(
                has_self,
                call_ty,
                None,
                name,
                fn_generic_tys,
                args,
                return_ty,
            ) => {
                let call_ty = tyck.initialize_ty(*call_ty, &btreemap! {})?;
                let fn_generic_tys = tyck.initialize_tys(&fn_generic_tys, &btreemap! {})?;
                let param_tys = tyck.satisfy_exprs(&args)?;
                let param_spans: Vec<_> = args.iter().map(|p| p.lookup(self.ctx).span).collect();
                let return_ty = tyck.initialize_ty(*return_ty, &btreemap! {})?;

                let (witness, fn_generic_tys) = tyck
                    .do_goal_method_selection(
                        expr,
                        *has_self,
                        call_ty,
                        *name,
                        &fn_generic_tys,
                        &param_tys,
                        return_ty,
                        &param_spans,
                        info.span,
                    )?
                    .expect("Expected no ambiguity here");
                let fn_generics = self.translate_sub_ttys(&fn_generic_tys)?;

                CExpression::Call(
                    self.translate_method(&witness, *name, fn_generics, info.span)?,
                    self.translate_sub_exprs(&args, tyck, slots)?,
                )
            },
        };

        Ok(expr)
    }

    pub fn translate_literal(&mut self, lit: LLiteral) -> CLiteral<'a> {
        match lit {
            LLiteral::True => CLiteral::True,
            LLiteral::False => CLiteral::False,
            LLiteral::String(s) => CLiteral::String(self.deintern_string(s)),
            LLiteral::Int(v) => CLiteral::Int(v),
            LLiteral::Float(f) => CLiteral::Float(f64::from_bits(f)),
            LLiteral::Char(c) => CLiteral::Char(c),
        }
    }

    pub fn translate_captures(
        &self,
        vcx: &LVariableContext,
        slots: &HashMap<VariableId, CStackId>,
    ) -> (HashMap<VariableId, CStackId>, CCaptures<'a>) {
        (
            vcx.variables
                .keys()
                .enumerate()
                .map(|(idx, id)| (*id, CStackId(idx)))
                .collect(),
            CCaptures(
                self.alloc.alloc_slice_fill_iter(
                    vcx.captures
                        .iter()
                        .map(|(new_id, old_variable)| ((slots[&old_variable.id], slots[&new_id]))),
                ),
            ),
        )
    }

    pub fn translate_sub_expr(
        &mut self,
        expr: Id<LExpression>,
        tyck: &mut Typechecker,

        slots: &HashMap<VariableId, CStackId>,
    ) -> AResult<&'a CExpression<'a>> {
        Ok(self.alloc.alloc(self.translate_expr(expr, tyck, slots)?))
    }

    pub fn translate_sub_exprs(
        &mut self,
        exprs: &[Id<LExpression>],
        tyck: &mut Typechecker,

        slots: &HashMap<VariableId, CStackId>,
    ) -> AResult<&'a [CExpression<'a>]> {
        let exprs = exprs
            .iter()
            .map(|expr| self.translate_expr(*expr, tyck, slots))
            .try_collect_vec()?;
        Ok(self.alloc.alloc_slice_copy(&exprs))
    }

    pub fn translate_stmt(
        &mut self,
        stmt: Id<LStatement>,
        tyck: &mut Typechecker,

        slots: &HashMap<VariableId, CStackId>,
    ) -> AResult<CStatement<'a>> {
        let stmt = match &stmt.lookup(self.ctx).data {
            LStatementData::Expression(expr) =>
                CStatement::Expression(self.translate_expr(*expr, tyck, slots)?),
            LStatementData::Let(pat, expr) => CStatement::Let(
                self.translate_pattern(*pat, tyck, slots)?,
                self.translate_expr(*expr, tyck, slots)?,
            ),
        };

        Ok(stmt)
    }

    pub fn translate_sub_stmts(
        &mut self,
        stmts: &[Id<LStatement>],
        tyck: &mut Typechecker,

        slots: &HashMap<VariableId, CStackId>,
    ) -> AResult<&'a [CStatement<'a>]> {
        let stmts = stmts
            .iter()
            .map(|stmt| self.translate_stmt(*stmt, tyck, slots))
            .try_collect_vec()?;
        Ok(self.alloc.alloc_slice_copy(&stmts))
    }
}
