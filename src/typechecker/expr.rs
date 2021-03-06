//! All of the methods having to do with expression-level typechecking

use crate::{
    lowering::{
        LExpression, LExpressionData, LLiteral, LPattern, LPatternData, LStatement, LStatementData,
    },
    util::{AResult, Id, Intern, Pretty, TryCollectVec, ZipExact},
};

use super::{
    ty::{UnifyMode, UNIT_TYPE},
    TType, Typechecker,
};

impl Typechecker<'_> {
    pub fn satisfy_expr(&mut self, e: Id<LExpression>) -> AResult<Id<TType>> {
        let LExpression {
            source: _,
            span,
            data,
        } = &*e.lookup(self.ctx);

        let ty = match data {
            LExpressionData::Literal(LLiteral::True)
            | LExpressionData::Literal(LLiteral::False) => self.ctx.static_ty(&TType::Bool),
            LExpressionData::Literal(LLiteral::Int(_)) => self.ctx.static_ty(&TType::Int),
            LExpressionData::Literal(LLiteral::Float(_)) => self.ctx.static_ty(&TType::Float),
            LExpressionData::Literal(LLiteral::String(_)) => self.ctx.static_ty(&TType::String),
            LExpressionData::Literal(LLiteral::Char(_)) => self.ctx.static_ty(&TType::Char),
            LExpressionData::Variable(v) => self.normalize_ty(self.variables[&v.id], *span)?,
            LExpressionData::Block(ss, expr) => {
                for s in ss {
                    self.satisfy_stmt(*s)?;
                }

                self.satisfy_expr(*expr)?
            },
            LExpressionData::Generator(vcx, parameter, return_ty, yield_ty, expr) => {
                self.satisfy_vcx(vcx)?;

                // In type is either the param ty
                let in_ty = parameter
                    .map(|p| self.satisfy_ty(p.ty))
                    .transpose()?
                    .unwrap_or_else(|| TType::Tuple(vec![]).intern(self.ctx));

                let return_span = return_ty.lookup(self.ctx).span;
                let return_ty = self.satisfy_ty(*return_ty)?;
                let yield_span = yield_ty.lookup(self.ctx).span;
                let yield_ty = self.satisfy_ty(*yield_ty)?;

                self.return_tys.push((return_ty, return_span));
                self.yield_tys.push((in_ty, yield_ty, yield_span));
                let expr_ty = self.satisfy_expr(*expr)?;
                self.return_tys.pop();
                self.yield_tys.pop();

                let return_ty = self.unify_ty(
                    UnifyMode::Normal,
                    return_ty,
                    return_span,
                    expr_ty,
                    expr.lookup(self.ctx).span,
                )?;

                TType::Object(self.ctx.lower_generator_item()?, vec![
                    in_ty, yield_ty, return_ty,
                ])
                .intern(self.ctx)
            },
            LExpressionData::Global(g) => self.satisfy_ty(g.lookup(self.ctx).ty)?,
            LExpressionData::GlobalFunction(f, gs) => {
                let gs = self.satisfy_tys(&gs)?;

                let (param_tys, return_ty, restrictions) =
                    self.instantiate_function(f.get(self.ctx), &gs)?;

                self.satisfy_instantiated_restrictions(restrictions, *span)?;

                TType::FnPtr(param_tys, return_ty).intern(self.ctx)
            },
            LExpressionData::Call(f, gs, params) => {
                let fun_info = f.lookup(self.ctx);
                let gs = self.satisfy_tys(&gs)?;

                let (param_tys, return_ty, restrictions) =
                    self.instantiate_function(f.get(self.ctx), &gs)?;

                for (i, (param_ty, param)) in param_tys.iter().zip_exact(params).enumerate() {
                    let given_ty = self.satisfy_expr(*param)?;

                    let _ = self.unify_ty(
                        UnifyMode::Normal,
                        *param_ty,
                        fun_info.parameters[i].span,
                        given_ty,
                        param.lookup(self.ctx).span,
                    )?;
                }

                self.satisfy_instantiated_restrictions(restrictions, *span)?;

                return_ty
            },
            LExpressionData::Access(o, span, name, access_ty) => {
                let object_ty = self.satisfy_expr(*o)?;
                let access_ty = self.satisfy_ty(*access_ty)?;

                if let Some(ty) = self.do_goal_access(object_ty, *name, *span)? {
                    self.unify_ty(UnifyMode::Normal, access_ty, *span, ty, *span)?
                } else {
                    access_ty
                }
            },
            LExpressionData::IndexAccess(o, span, idx, access_ty) => {
                let object_ty = self.satisfy_expr(*o)?;
                let access_ty = self.satisfy_ty(*access_ty)?;

                if let Some(ty) = self.do_goal_index_access(object_ty, *idx, *span)? {
                    self.unify_ty(UnifyMode::Normal, access_ty, *span, ty, *span)?
                } else {
                    access_ty
                }
            },
            LExpressionData::Tuple(es) => TType::Tuple(self.satisfy_exprs(&es)?).intern(self.ctx),
            LExpressionData::ArrayLiteral(es, elem_ty) => {
                let mut elem_span = *span;
                let mut elem_ty = self.satisfy_ty(*elem_ty)?;

                for e in es {
                    let new_elem_ty = self.satisfy_expr(*e)?;
                    let new_elem_span = e.lookup(self.ctx).span;

                    elem_ty = self.unify_ty(
                        UnifyMode::Normal,
                        elem_ty,
                        elem_span,
                        new_elem_ty,
                        new_elem_span,
                    )?;
                    elem_span = new_elem_span;
                }

                TType::Array(elem_ty).intern(self.ctx)
            },
            LExpressionData::Assign(left, right) => {
                let left_ty = self.satisfy_expr(*left)?;
                let right_ty = self.satisfy_expr(*right)?;

                self.unify_ty(
                    UnifyMode::Normal,
                    left_ty,
                    left.lookup(self.ctx).span,
                    right_ty,
                    right.lookup(self.ctx).span,
                )?
            },
            LExpressionData::Return(expr) => {
                let (return_ty, return_span) = self.return_tys.last().cloned().unwrap();

                let expr_ty = self.satisfy_expr(*expr)?;
                let _ = self.unify_ty(
                    UnifyMode::Normal,
                    return_ty,
                    return_span,
                    expr_ty,
                    expr.lookup(self.ctx).span,
                )?;

                self.ctx.static_ty(&TType::Never)
            },
            LExpressionData::Yield(expr) => {
                let (in_ty, yield_ty, yield_span) = self.yield_tys.last().cloned().unwrap();

                let expr_ty = self.satisfy_expr(*expr)?;
                let _ = self.unify_ty(
                    UnifyMode::Normal,
                    yield_ty,
                    yield_span,
                    expr_ty,
                    expr.lookup(self.ctx).span,
                )?;

                in_ty
            },
            LExpressionData::Loop(id, expr, loop_ty) => {
                let loop_span = loop_ty.lookup(self.ctx).span;
                let loop_ty = self.satisfy_ty(*loop_ty)?;

                self.loop_tys.insert(*id, (loop_ty, loop_span));

                let expr_ty = self.satisfy_expr(*expr)?;
                let _ = self.unify_ty(
                    UnifyMode::Normal,
                    expr_ty,
                    expr.lookup(self.ctx).span,
                    self.ctx.static_ty(UNIT_TYPE),
                    *span,
                )?;

                loop_ty
            },
            LExpressionData::Break(id, expr) => {
                let (loop_ty, loop_span) = self.loop_tys[id];

                let expr_ty = self.satisfy_expr(*expr)?;
                let _ = self.unify_ty(
                    UnifyMode::Normal,
                    loop_ty,
                    loop_span,
                    expr_ty,
                    e.lookup(self.ctx).span,
                )?;

                self.ctx.static_ty(&TType::Never)
            },
            LExpressionData::Continue(_) => self.ctx.static_ty(&TType::Never),
            LExpressionData::Or(left, right) | LExpressionData::And(left, right) => {
                let left_ty = self.satisfy_expr(*left)?;
                let right_ty = self.satisfy_expr(*right)?;

                let ty = self.unify_ty(
                    UnifyMode::Normal,
                    left_ty,
                    left.lookup(self.ctx).span,
                    right_ty,
                    right.lookup(self.ctx).span,
                )?;

                self.unify_ty(
                    UnifyMode::Normal,
                    ty,
                    *span,
                    self.ctx.static_ty(&TType::Bool),
                    *span,
                )?
            },
            LExpressionData::If(c, t, e) => {
                let cond_ty = self.satisfy_expr(*c)?;

                let _ = self.unify_ty(
                    UnifyMode::Normal,
                    cond_ty,
                    c.lookup(self.ctx).span,
                    self.ctx.static_ty(&TType::Bool),
                    *span,
                )?;

                let then_ty = self.satisfy_expr(*t)?;
                let else_ty = self.satisfy_expr(*e)?;

                self.unify_ty(
                    UnifyMode::Normal,
                    then_ty,
                    t.lookup(self.ctx).span,
                    else_ty,
                    e.lookup(self.ctx).span,
                )?
            },
            LExpressionData::StructConstructor(o, gs, members)
            | LExpressionData::AllocateObject(o, gs, members) => {
                let generic_tys = self.satisfy_tys(&gs)?;

                let (object_ty, expected_tys, restrictions) =
                    self.instantiate_object(o.get(self.ctx), &generic_tys)?;

                for (idx, member) in members {
                    let member_ty = self.satisfy_expr(*member)?;

                    let _ = self.unify_ty(
                        UnifyMode::Normal,
                        expected_tys[*idx],
                        *span,
                        member_ty,
                        member.lookup(self.ctx).span,
                    )?;
                }

                self.satisfy_instantiated_restrictions(restrictions, *span)?;

                object_ty
            },
            LExpressionData::EnumConstructor(e, gs, n, members) => {
                let generic_tys = self.satisfy_tys(&gs)?;

                let (enum_ty, expected_tys, restrictions) =
                    self.instantiate_enum_variant(e.get(self.ctx), &generic_tys, *n)?;

                for (idx, member) in members {
                    let member_ty = self.satisfy_expr(*member)?;

                    let _ = self.unify_ty(
                        UnifyMode::Normal,
                        expected_tys[*idx],
                        *span,
                        member_ty,
                        member.lookup(self.ctx).span,
                    )?;
                }

                self.satisfy_instantiated_restrictions(restrictions, *span)?;

                enum_ty
            },
            LExpressionData::Closure(vcx, params, return_ty, expr) => {
                self.satisfy_vcx(vcx)?;

                let return_span = return_ty.lookup(self.ctx).span;
                let return_ty = self.satisfy_ty(*return_ty)?;

                self.return_tys.push((return_ty, return_span));
                let expr_ty = self.satisfy_expr(*expr)?;
                self.return_tys.pop();

                let param_tys = self.satisfy_patterns(&params)?;
                let return_ty = self.unify_ty(
                    UnifyMode::Normal,
                    return_ty,
                    return_span,
                    expr_ty,
                    expr.lookup(self.ctx).span,
                )?;

                TType::Closure(param_tys, return_ty).intern(self.ctx)
            },
            LExpressionData::Match(expr, patterns) => {
                let expr_span = expr.lookup(self.ctx).span;
                let mut expr_ty = self.satisfy_expr(*expr)?;

                let mut last_output = None;

                for (pattern, output) in patterns {
                    let pattern_ty = self.satisfy_pattern(*pattern)?;
                    expr_ty = self.unify_ty(
                        UnifyMode::Normal,
                        pattern_ty,
                        pattern.lookup(self.ctx).span,
                        expr_ty,
                        expr_span,
                    )?;

                    let output_span = output.lookup(self.ctx).span;
                    let mut output_ty = self.satisfy_expr(*output)?;

                    if let Some((last_output_ty, last_output_span)) = last_output {
                        output_ty = self.unify_ty(
                            UnifyMode::Normal,
                            last_output_ty,
                            last_output_span,
                            output_ty,
                            output_span,
                        )?;
                    }

                    last_output = Some((output_ty, output_span));
                }

                if let Some((output_ty, _)) = last_output {
                    output_ty
                } else {
                    self.ctx.static_ty(&TType::Never)
                }
            },
            LExpressionData::StaticCall(
                has_self,
                call_ty,
                Some(trait_ty),
                name,
                gs,
                params,
                return_ty,
            ) => {
                let call_span = call_ty.lookup(self.ctx).span;
                let call_ty = self.satisfy_ty(*call_ty)?;
                let trait_ty_span = trait_ty.lookup(self.ctx).span;
                let trait_ty = self.satisfy_trait_ty(*trait_ty)?;
                let fn_generic_tys = self.satisfy_tys(&gs)?;
                let param_spans: Vec<_> = params.iter().map(|p| p.lookup(self.ctx).span).collect();
                let mut param_tys = self.satisfy_exprs(&params)?;
                let return_ty = self.satisfy_ty(*return_ty)?;

                if *has_self {
                    param_tys[0] = self.unify_ty(
                        UnifyMode::Normal,
                        call_ty,
                        call_span,
                        param_tys[0],
                        params[0].lookup(self.ctx).span,
                    )?;
                }

                debug!(
                    "We have fn_generic_tys = {:?}",
                    Pretty(&fn_generic_tys, self.ctx)
                );

                self.do_goal_trait_call_unification(
                    call_ty,
                    trait_ty,
                    *name,
                    &fn_generic_tys,
                    &param_tys,
                    return_ty,
                    &param_spans,
                    *span,
                )?;

                if let Some(imp) =
                    self.do_goal_trait(call_ty, call_span, trait_ty, trait_ty_span, false)?
                {
                    self.do_goal_impl_call_unification(
                        &imp,
                        *name,
                        &fn_generic_tys,
                        &param_tys,
                        return_ty,
                        &param_spans,
                        *span,
                    )?
                } else {
                    return_ty
                }
            },
            LExpressionData::StaticCall(has_self, call_ty, None, name, gs, params, return_ty) => {
                let call_span = call_ty.lookup(self.ctx).span;
                let call_ty = self.satisfy_ty(*call_ty)?;
                let fn_generic_tys = self.satisfy_tys(&gs)?;
                let param_spans: Vec<_> = params.iter().map(|p| p.lookup(self.ctx).span).collect();
                let mut param_tys = self.satisfy_exprs(&params)?;
                let return_ty = self.satisfy_ty(*return_ty)?;

                if *has_self {
                    param_tys[0] = self.unify_ty(
                        UnifyMode::Normal,
                        call_ty,
                        call_span,
                        param_tys[0],
                        params[0].lookup(self.ctx).span,
                    )?;
                }

                if let Some((imp, fn_generic_tys)) = self.do_goal_method_selection(
                    e,
                    *has_self,
                    call_ty,
                    *name,
                    &fn_generic_tys,
                    &param_tys,
                    return_ty,
                    &param_spans,
                    *span,
                )? {
                    self.do_goal_impl_call_unification(
                        &imp,
                        *name,
                        &fn_generic_tys,
                        &param_tys,
                        return_ty,
                        &param_spans,
                        *span,
                    )?
                } else {
                    return_ty
                }
            },
        };

        Ok(ty)
    }

    pub fn satisfy_exprs(&mut self, es: &[Id<LExpression>]) -> AResult<Vec<Id<TType>>> {
        es.iter().map(|e| self.satisfy_expr(*e)).try_collect_vec()
    }

    fn satisfy_stmt(&mut self, s: Id<LStatement>) -> AResult<()> {
        match &s.lookup(self.ctx).data {
            LStatementData::Expression(e) => {
                let _ = self.satisfy_expr(*e)?;
            },
            LStatementData::Let(p, e) => {
                let pattern_ty = self.satisfy_pattern(*p)?;
                let expr_ty = self.satisfy_expr(*e)?;

                let _ = self.unify_ty(
                    UnifyMode::Normal,
                    pattern_ty,
                    p.lookup(self.ctx).span,
                    expr_ty,
                    p.lookup(self.ctx).span,
                )?;
            },
        }

        Ok(())
    }

    fn satisfy_pattern(&mut self, p: Id<LPattern>) -> AResult<Id<TType>> {
        let LPattern {
            source: _,
            span,
            ty: expected_ty,
            data,
        } = &*p.lookup(self.ctx);

        let ty = match data {
            LPatternData::Underscore(ty) => self.satisfy_ty(*ty)?,
            LPatternData::Literal(LLiteral::True) | LPatternData::Literal(LLiteral::False) =>
                self.ctx.static_ty(&TType::Bool),
            LPatternData::Literal(LLiteral::Int(_)) => self.ctx.static_ty(&TType::Int),
            LPatternData::Literal(LLiteral::Float(_)) => self.ctx.static_ty(&TType::Float),
            LPatternData::Literal(LLiteral::String(_)) => self.ctx.static_ty(&TType::String),
            LPatternData::Literal(LLiteral::Char(_)) => self.ctx.static_ty(&TType::Char),
            LPatternData::Variable(v) => self.normalize_ty(self.variables[&v.id], *span)?,
            LPatternData::Tuple(ps) => TType::Tuple(self.satisfy_patterns(&ps)?).intern(self.ctx),
            LPatternData::StructPattern(o, gs, members) => {
                let generic_tys = self.satisfy_tys(&gs)?;

                let (object_ty, expected_tys, restrictions) =
                    self.instantiate_object(o.get(self.ctx), &generic_tys)?;

                for (expected_ty, member) in expected_tys.iter().zip_exact(members) {
                    let member_ty = self.satisfy_pattern(*member)?;

                    let _ = self.unify_ty(
                        UnifyMode::Normal,
                        *expected_ty,
                        *span,
                        member_ty,
                        member.lookup(self.ctx).span,
                    )?;
                }

                self.satisfy_instantiated_restrictions(restrictions, *span)?;

                object_ty
            },
            LPatternData::EnumVariantPattern(e, gs, n, members) => {
                let generic_tys = self.satisfy_tys(&gs)?;

                let (enum_ty, expected_tys, restrictions) =
                    self.instantiate_enum_variant(e.get(self.ctx), &generic_tys, *n)?;

                for (expected_ty, member) in expected_tys.iter().zip_exact(members) {
                    let member_ty = self.satisfy_pattern(*member)?;

                    let _ = self.unify_ty(
                        UnifyMode::Normal,
                        *expected_ty,
                        *span,
                        member_ty,
                        member.lookup(self.ctx).span,
                    )?;
                }

                self.satisfy_instantiated_restrictions(restrictions, *span)?;

                enum_ty
            },
        };

        if let Some(expected_ty) = expected_ty {
            let expected_span = expected_ty.lookup(self.ctx).span;
            let expected_ty = self.satisfy_ty(*expected_ty)?;

            self.unify_ty(UnifyMode::Normal, expected_ty, expected_span, ty, *span)
        } else {
            Ok(ty)
        }
    }

    fn satisfy_patterns(&mut self, ps: &[Id<LPattern>]) -> AResult<Vec<Id<TType>>> {
        ps.iter()
            .map(|p| self.satisfy_pattern(*p))
            .try_collect_vec()
    }
}
