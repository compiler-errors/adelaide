use crate::{
    lexer::Span,
    parser::{
        PBinopKind, PConstructorArguments, PExpression, PExpressionData, PLiteral, PPattern,
        PStatement, PStatementData,
    },
    util::{AError, AResult, Id, Intern, LId},
};

use super::{
    fresh_name, ty::LTypeData, LConstructorShape, LEnum, LFunction, LGlobal, LObject, LPattern,
    LPatternData, LScopeItem, LTraitType, LType, LVariable, LVariableContext, LoopId,
    LoweringContext,
};

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LExpression {
    #[plain]
    pub source: Id<PExpression>,
    pub span: Span,
    pub data: LExpressionData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LExpressionData {
    Literal(LLiteral),
    Variable(LVariable),
    Block(Vec<Id<LStatement>>, Id<LExpression>),
    AsyncBlock(LVariableContext, Id<LExpression>, Id<LType>),
    Global(LId<LGlobal>),
    GlobalFunction(LId<LFunction>, Vec<Id<LType>>),
    Access(Id<LExpression>, Span, Id<str>, Id<LType>),
    IndexAccess(Id<LExpression>, Span, usize, Id<LType>),
    Tuple(Vec<Id<LExpression>>),
    ArrayLiteral(Vec<Id<LExpression>>, Id<LType>),
    Assign(Id<LExpression>, Id<LExpression>),
    StaticCall(
        bool,
        Id<LType>,
        Option<Id<LTraitType>>,
        Id<str>,
        Vec<Id<LType>>,
        Vec<Id<LExpression>>,
        Id<LType>,
    ),
    Call(LId<LFunction>, Vec<Id<LType>>, Vec<Id<LExpression>>),
    Or(Id<LExpression>, Id<LExpression>),
    And(Id<LExpression>, Id<LExpression>),
    Return(Id<LExpression>),
    Break(LoopId, Id<LExpression>),
    Continue(LoopId),
    StructConstructor(LId<LObject>, Vec<Id<LType>>, Vec<(usize, Id<LExpression>)>),
    AllocateObject(LId<LObject>, Vec<Id<LType>>, Vec<(usize, Id<LExpression>)>),
    EnumConstructor(
        LId<LEnum>,
        Vec<Id<LType>>,
        Id<str>,
        Vec<(usize, Id<LExpression>)>,
    ),
    Closure(
        LVariableContext,
        Vec<Id<LPattern>>,
        Id<LType>,
        Id<LExpression>,
    ),
    If(Id<LExpression>, Id<LExpression>, Id<LExpression>),
    Loop(LoopId, Id<LExpression>, Id<LType>),
    Match(Id<LExpression>, Vec<(Id<LPattern>, Id<LExpression>)>),
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LLiteral {
    True,
    False,
    String(Id<str>),
    Int(i64),
    Float(u64 /* as bits */),
    Char(char),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LStatement {
    #[plain]
    pub source: Option<Id<PStatement>>,
    pub span: Span,
    pub data: LStatementData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LStatementData {
    Expression(Id<LExpression>),
    Let(Id<LPattern>, Id<LExpression>),
}

impl LoweringContext<'_> {
    pub fn lower_expr(&mut self, e: Id<PExpression>) -> AResult<Id<LExpression>> {
        let PExpression { span, data } = &*e.lookup(self.ctx);

        let data = match data {
            PExpressionData::Literal(lit) =>
                LExpressionData::Literal(self.lower_literal(*lit, *span)?),
            PExpressionData::Unimplemented =>
            // Lower into the `std::unimplemented::<T>()` fn call
                if let LScopeItem::Function(unimplemented) = self.ctx.std_item("unimplemented") {
                    LExpressionData::Call(
                        unimplemented.into(),
                        vec![self.fresh_infer_ty(*span)],
                        vec![],
                    )
                } else {
                    unreachable!()
                },
            PExpressionData::Identifiers(id, generics) => match self.lookup_path(&id)? {
                LScopeItem::Function(f) => {
                    let info = f.lookup(self.ctx);

                    let generics = self.lower_tys(generics, true, true)?;
                    let generics = self.check_generics_parity(
                        generics,
                        *span,
                        info.generics.len(),
                        info.span,
                        true,
                    )?;

                    LExpressionData::GlobalFunction(f.into(), generics)
                },
                LScopeItem::Variable(v) => {
                    if !generics.is_empty() {
                        return Err(AError::DenyGenerics {
                            kind: "variable",
                            name: v.name,
                            use_span: *span,
                            def_span: v.span,
                        });
                    }

                    LExpressionData::Variable(v)
                },
                LScopeItem::Global(g) => {
                    if !generics.is_empty() {
                        let info = g.lookup(self.ctx);

                        return Err(AError::DenyGenerics {
                            kind: "global",
                            name: info.name,
                            use_span: *span,
                            def_span: info.span,
                        });
                    }

                    LExpressionData::Global(g.into())
                },
                item => {
                    let (kind, name, def_span) = item.info(self.ctx);

                    return Err(AError::NotAnExpression {
                        kind,
                        name,
                        def_span,
                        use_span: *span,
                    });
                },
            },
            PExpressionData::SelfRef => {
                if let LScopeItem::Variable(v) =
                    self.lookup_scoped_item(*span, self.ctx.static_name("self"))?
                {
                    LExpressionData::Variable(v)
                } else {
                    unreachable!()
                }
            },
            PExpressionData::Block(s, e) => {
                self.enter_block();
                let s = self.lower_statements(s)?;
                let e = self.lower_expr(*e)?;
                self.exit_block();

                LExpressionData::Block(s, e)
            },
            PExpressionData::AsyncBlock(e) => {
                self.enter_context(true, true);
                let e = self.lower_expr(*e)?;
                let ctx = self.exit_context();

                LExpressionData::AsyncBlock(ctx, e, self.fresh_infer_ty(*span))
            },
            PExpressionData::Tuple(es) => LExpressionData::Tuple(self.lower_exprs(es)?),
            PExpressionData::ArrayLiteral(es) =>
                LExpressionData::ArrayLiteral(self.lower_exprs(es)?, self.fresh_infer_ty(*span)),
            PExpressionData::Array(a, n) => {
                let a = self.lower_ty(*a, true, true)?;
                let n = self.lower_expr(*n)?;
                self.std_static_call(
                    false,
                    a,
                    "AllocateArray",
                    vec![],
                    "allocate_array",
                    vec![],
                    vec![n],
                    *span,
                )
            },
            PExpressionData::InfiniteRange(a) => {
                let a = self.lower_expr(*a)?;
                let b = self.get_range_bound(*span, e, "Unbounded", vec![]);
                self.std_static_call(
                    false,
                    self.fresh_infer_ty(*span),
                    "Range",
                    self.fresh_infer_tys(1, *span),
                    "range",
                    vec![],
                    vec![a, b],
                    *span,
                )
            },
            PExpressionData::BinOp(a, PBinopKind::RangeInclusive, b) => {
                let a = self.lower_expr(*a)?;
                let b = self.lower_expr(*b)?;
                let b = self.get_range_bound(*span, e, "Inclusive", vec![b]);
                self.std_static_call(
                    false,
                    self.fresh_infer_ty(*span),
                    "Range",
                    self.fresh_infer_tys(1, *span),
                    "range",
                    vec![],
                    vec![a, b],
                    *span,
                )
            },
            PExpressionData::BinOp(a, PBinopKind::RangeExclusive, b) => {
                let a = self.lower_expr(*a)?;
                let b = self.lower_expr(*b)?;
                let b = self.get_range_bound(*span, e, "Exclusive", vec![b]);
                self.std_static_call(
                    false,
                    self.fresh_infer_ty(*span),
                    "Range",
                    self.fresh_infer_tys(1, *span),
                    "range",
                    vec![],
                    vec![a, b],
                    *span,
                )
            },
            PExpressionData::BinOp(a, PBinopKind::OrCircuit, b) =>
                LExpressionData::Or(self.lower_expr(*a)?, self.lower_expr(*b)?),
            PExpressionData::BinOp(a, PBinopKind::AndCircuit, b) =>
                LExpressionData::And(self.lower_expr(*a)?, self.lower_expr(*b)?),
            PExpressionData::BinOp(a, op, b) => {
                let a = self.lower_expr(*a)?;
                let b = self.lower_expr(*b)?;

                // Map each operator onto its corresponding trait and method
                let (tr, f) = match op {
                    PBinopKind::Or => ("Or", "or"),
                    PBinopKind::And => ("And", "and"),
                    PBinopKind::Lt => ("Compare", "lt"),
                    PBinopKind::Gt => ("Compare", "gt"),
                    PBinopKind::Le => ("Compare", "le"),
                    PBinopKind::Ge => ("Compare", "ge"),
                    PBinopKind::Eq => ("Equals", "eq"),
                    PBinopKind::Ne => ("Equals", "ne"),
                    PBinopKind::Plus => ("Add", "add"),
                    PBinopKind::Minus => ("Subtract", "sub"),
                    PBinopKind::Mul => ("Multiply", "mul"),
                    PBinopKind::Div => ("Divide", "div"),
                    PBinopKind::Mod => ("Modulo", "rem"),
                    // These have been lowered separately
                    PBinopKind::OrCircuit
                    | PBinopKind::AndCircuit
                    | PBinopKind::RangeInclusive
                    | PBinopKind::RangeExclusive => unreachable!(),
                };

                self.std_static_call(
                    true,
                    self.fresh_infer_ty(*span),
                    tr,
                    self.fresh_infer_tys(1, *span),
                    f,
                    vec![],
                    vec![a, b],
                    *span,
                )
            },
            PExpressionData::Assign(a, b) => match &a.lookup(self.ctx).data {
                PExpressionData::Index(a, i) => {
                    // If the LHS of the assign operator is an index operator,
                    // e.g. `a[i] = b`, then we will desugar this into the
                    // `DerefAssign` trait specifically
                    let a = self.lower_expr(*a)?;
                    let b = self.lower_expr(*b)?;
                    let i = self.lower_expr(*i)?;

                    self.std_static_call(
                        true,
                        self.fresh_infer_ty(*span),
                        "DerefAssign",
                        self.fresh_infer_tys(1, *span),
                        "deref_assign",
                        vec![],
                        vec![a, i, b],
                        *span,
                    )
                },
                _ => {
                    let a = self.lower_expr(*a)?;
                    let b = self.lower_expr(*b)?;
                    LExpressionData::Assign(a, b)
                },
            },
            PExpressionData::Not(e) => {
                let e = self.lower_expr(*e)?;
                self.std_static_call(
                    true,
                    self.fresh_infer_ty(*span),
                    "Not",
                    vec![],
                    "not",
                    vec![],
                    vec![e],
                    *span,
                )
            },
            PExpressionData::Neg(e) => {
                let e = self.lower_expr(*e)?;
                self.std_static_call(
                    true,
                    self.fresh_infer_ty(*span),
                    "Negate",
                    vec![],
                    "negate",
                    vec![],
                    vec![e],
                    *span,
                )
            },
            PExpressionData::InterpolationBegin(l, s) => {
                let string = LType {
                    data: LTypeData::String,
                    span: *span,
                }
                .intern(self.ctx);
                let l = LExpression {
                    source: e,
                    span: *span,
                    data: LExpressionData::Literal(LLiteral::String(*l)),
                }
                .intern(self.ctx);
                let s = self.lower_expr(*s)?;

                self.std_static_call(
                    true,
                    string,
                    "Add",
                    vec![string],
                    "add",
                    vec![],
                    vec![l, s],
                    *span,
                )
            },
            PExpressionData::InterpolationContinue(a, l, s) => {
                let string = LType {
                    data: LTypeData::String,
                    span: *span,
                }
                .intern(self.ctx);
                let l = LExpression {
                    source: e,
                    span: *span,
                    data: LExpressionData::Literal(LLiteral::String(*l)),
                }
                .intern(self.ctx);
                let a = self.lower_expr(*a)?;
                let a = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(
                        true,
                        self.fresh_infer_ty(*span),
                        "Into",
                        vec![string],
                        "into",
                        vec![],
                        vec![a],
                        *span,
                    ),
                }
                .intern(self.ctx);
                let al = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(
                        true,
                        string,
                        "Add",
                        vec![string],
                        "add",
                        vec![],
                        vec![a, l],
                        *span,
                    ),
                }
                .intern(self.ctx);
                let s = self.lower_expr(*s)?;

                self.std_static_call(
                    true,
                    string,
                    "Add",
                    vec![string],
                    "add",
                    vec![],
                    vec![al, s],
                    *span,
                )
            },
            PExpressionData::InterpolationEnd(a, l) => {
                let string = LType {
                    data: LTypeData::String,
                    span: *span,
                }
                .intern(self.ctx);
                let l = LExpression {
                    source: e,
                    span: *span,
                    data: LExpressionData::Literal(LLiteral::String(*l)),
                }
                .intern(self.ctx);
                let a = self.lower_expr(*a)?;
                let a = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(
                        true,
                        self.fresh_infer_ty(*span),
                        "Into",
                        vec![string],
                        "into",
                        vec![],
                        vec![a],
                        *span,
                    ),
                }
                .intern(self.ctx);

                self.std_static_call(
                    true,
                    string,
                    "Add",
                    vec![string],
                    "add",
                    vec![],
                    vec![a, l],
                    *span,
                )
            },
            PExpressionData::Call(c, ps) => {
                let c = self.lower_expr(*c)?;
                let ps = self.lower_exprs(ps)?;

                if let LExpression {
                    data: LExpressionData::GlobalFunction(f, g),
                    ..
                } = &*c.lookup(self.ctx)
                {
                    let fn_info = f.source().lookup(self.ctx);
                    if ps.len() != fn_info.parameters.len() {
                        return Err(AError::ParityDisparity {
                            kind: "arguments",
                            expected: fn_info.parameters.len(),
                            expected_span: fn_info.span,
                            given: ps.len(),
                            given_span: *span,
                        });
                    }

                    LExpressionData::Call(f.clone(), g.clone(), ps)
                } else {
                    let ps = LExpression {
                        source: e,
                        span: *span,
                        data: LExpressionData::Tuple(ps),
                    }
                    .intern(self.ctx);

                    self.std_static_call(
                        true,
                        self.fresh_infer_ty(*span),
                        "Call",
                        self.fresh_infer_tys(1, *span),
                        "call",
                        vec![],
                        vec![c, ps],
                        *span,
                    )
                }
            },
            PExpressionData::StaticCall(t, n, gs, ps) => {
                let (t, trait_ty) = self.lower_elaborated_ty(*t, true, true)?;
                let gs = self.lower_tys(gs, true, true)?;
                let ps = self.lower_exprs(ps)?;

                if let Some(trait_ty) = trait_ty {
                    let tr = trait_ty.lookup(self.ctx).tr.source();
                    let shape = self.ctx.trait_shape(tr)?;

                    if !shape.methods.contains_key(n) {
                        let info = tr.lookup(self.ctx);
                        return Err(AError::NoMethod {
                            trait_name: info.name,
                            trait_span: info.span,
                            name: *n,
                            use_span: *span,
                        });
                    }

                    let gs = self.check_generics_parity(
                        gs,
                        *span,
                        shape.method_generics_and_parameters[n].0,
                        shape.methods[n],
                        true,
                    )?;

                    if ps.len() != shape.method_generics_and_parameters[n].1 {
                        return Err(AError::ParityDisparity {
                            kind: "arguments",
                            expected: shape.method_generics_and_parameters[n].1,
                            expected_span: shape.methods[n],
                            given: ps.len(),
                            given_span: *span,
                        });
                    }

                    LExpressionData::StaticCall(
                        false,
                        t,
                        Some(trait_ty),
                        *n,
                        gs,
                        ps,
                        self.fresh_infer_ty(*span),
                    )
                } else {
                    LExpressionData::StaticCall(
                        false,
                        t,
                        None,
                        *n,
                        gs,
                        ps,
                        self.fresh_infer_ty(*span),
                    )
                }
            },
            PExpressionData::ObjectCall(e, n, g, p) => {
                let mut p = self.lower_exprs(&p)?;
                p.insert(0, self.lower_expr(*e)?);

                LExpressionData::StaticCall(
                    true,
                    self.fresh_infer_ty(*span),
                    None,
                    *n,
                    self.lower_tys(g, true, true)?,
                    p,
                    self.fresh_infer_ty(*span),
                )
            },
            PExpressionData::If(p, t, e) => LExpressionData::If(
                self.lower_expr(*p)?,
                self.lower_expr(*t)?,
                self.lower_expr(*e)?,
            ),
            PExpressionData::IfLet(p, v, t, e) =>
                LExpressionData::Match(self.lower_expr(*v)?, vec![
                    (self.lower_pattern(*p)?, self.lower_expr(*t)?),
                    (self.fresh_empty_pattern(*span), self.lower_expr(*e)?),
                ]),
            PExpressionData::Loop(l, e) => {
                let l = self.enter_label(*l);
                let e = self.lower_expr(*e)?;

                let ty = if self.exit_label() {
                    self.fresh_infer_ty(*span)
                } else {
                    LType {
                        data: LTypeData::Never,
                        span: *span,
                    }
                    .intern(self.ctx)
                };

                LExpressionData::Loop(l, e, ty)
            },
            PExpressionData::While(l, p, t, els) =>
                self.lower_expr_while(e, *span, *l, *p, *t, *els)?,
            PExpressionData::For(l, p, es, t, els) =>
                self.lower_expr_for(e, *span, *l, *p, *es, *t, *els)?,
            PExpressionData::Match(e, ps) => {
                let m = self.lower_expr(*e)?;

                let mut lps = vec![];
                for (p, e) in ps {
                    self.enter_block();
                    let p = self.lower_pattern(*p)?;
                    let e = self.lower_expr(*e)?;
                    lps.push((p, e));
                    self.exit_block();
                }

                LExpressionData::Match(m, lps)
            },
            PExpressionData::StructuralAmbiguous(p, g, a) => match self.lookup_path(p)? {
                LScopeItem::Object(o) => {
                    let info = o.lookup(self.ctx);
                    if !info.is_structural {
                        return Err(AError::TriedConstructingObject {
                            parent_name: info.name,
                            parent_span: info.span,
                            use_span: *span,
                        });
                    }

                    let g = self.lower_tys(g, true, true)?;
                    let g =
                        self.check_generics_parity(g, *span, info.generics.len(), info.span, true)?;

                    let s = self.ctx.object_constructor(o)?;
                    let a = self.lower_constructor("object", info.name, &s, a)?;

                    LExpressionData::StructConstructor(o.into(), g, a)
                },
                LScopeItem::EnumVariant(e, v) => {
                    if !g.is_empty() {
                        return Err(AError::BareEnumGenerics {
                            enum_name: e.lookup(self.ctx).name,
                            variant_name: v,
                            use_span: *span,
                            def_span: self.ctx.enum_variant_span(e, v)?,
                        });
                    }

                    let g = self.fresh_infer_tys(e.lookup(self.ctx).generics.len(), *span);

                    let s = self.ctx.enum_variant_constructor(e, v)?;
                    let a = self.lower_constructor("enum variant", v, &s, a)?;

                    LExpressionData::EnumConstructor(e.into(), g, v, a)
                },
                i => {
                    let (kind, name, def_span) = i.info(self.ctx);

                    return Err(AError::CannotConstruct {
                        kind,
                        name,
                        def_span,
                        use_span: *span,
                    });
                },
            },
            PExpressionData::StructuralVariant(p, g, v, a) => match self.lookup_path(p)? {
                LScopeItem::Enum(e) => {
                    let info = e.lookup(self.ctx);

                    let g = self.lower_tys(g, true, true)?;
                    let g =
                        self.check_generics_parity(g, *span, info.generics.len(), info.span, true)?;

                    let s = self.ctx.enum_variant_constructor(e, *v)?;
                    let a = self.lower_constructor("enum variant", *v, &s, a)?;

                    LExpressionData::EnumConstructor(e.into(), g, *v, a)
                },
                i => {
                    let (kind, name, _) = i.info(self.ctx);
                    return Err(AError::NotAnEnumVariant {
                        kind,
                        name,
                        variant: *v,
                        use_span: *span,
                    });
                },
            },
            PExpressionData::Allocate(p, g, a) => match self.lookup_path(p)? {
                LScopeItem::Object(o) => {
                    if LScopeItem::Object(o) == self.ctx.std_item("Awaitable") {
                        return Err(AError::CannotConstruct {
                            kind: "object",
                            name: self.ctx.static_name("Awaitable"),
                            use_span: *span,
                            def_span: o.lookup(self.ctx).span,
                        });
                    }

                    let info = o.lookup(self.ctx);
                    if info.is_structural {
                        return Err(AError::TriedAllocatingStruct {
                            parent_name: info.name,
                            parent_span: info.span,
                            use_span: *span,
                        });
                    }

                    let g = self.lower_tys(g, true, true)?;
                    let g =
                        self.check_generics_parity(g, *span, info.generics.len(), info.span, true)?;

                    let s = self.ctx.object_constructor(o)?;
                    let a = self.lower_constructor("object", info.name, &s, a)?;

                    LExpressionData::AllocateObject(o.into(), g, a)
                },
                i => {
                    let (kind, name, def_span) = i.info(self.ctx);

                    return Err(AError::CannotConstruct {
                        kind,
                        name,
                        def_span,
                        use_span: *span,
                    });
                },
            },
            PExpressionData::Return(v) =>
                if self.scopes.last_mut().unwrap().return_allowed {
                    LExpressionData::Return(self.lower_expr(*v)?)
                } else {
                    return Err(AError::IllegalReturn { span: *span });
                },
            PExpressionData::Assert(v) => {
                if let LScopeItem::Function(assert) = self.ctx.std_item("assert_impl") {
                    LExpressionData::Call(assert.into(), vec![], vec![self.lower_expr(*v)?])
                } else {
                    unreachable!()
                }
            },
            PExpressionData::Break(b, l) => {
                let e = if let Some(e) = b {
                    self.lower_expr(*e)?
                } else {
                    LExpression {
                        source: e,
                        span: *span,
                        data: LExpressionData::Tuple(vec![]),
                    }
                    .intern(self.ctx)
                };

                let s = l.map_or(*span, |(s, _)| s);
                let l = l.map(|(_, l)| l);

                LExpressionData::Break(self.lookup_label(s, l)?, e)
            },
            PExpressionData::Continue(l) => {
                let s = l.map_or(*span, |(s, _)| s);
                let l = l.map(|(_, l)| l);

                LExpressionData::Continue(self.lookup_label(s, l)?)
            },
            PExpressionData::Closure(ps, r, e) => {
                self.enter_context(true, false);

                let ps = self.lower_patterns(ps)?;
                let e = self.lower_expr(*e)?;
                let r = self.lower_ty(*r, true, true)?;

                let vcx = self.exit_context();

                LExpressionData::Closure(vcx, ps, r, e)
            },
            PExpressionData::Throw(t) => self.lower_expr_throw(e, *span, *t)?,
            PExpressionData::Index(a, i) => {
                let a = self.lower_expr(*a)?;
                let i = self.lower_expr(*i)?;
                self.std_static_call(
                    true,
                    self.fresh_infer_ty(*span),
                    "Deref",
                    self.fresh_infer_tys(1, *span),
                    "deref",
                    vec![],
                    vec![a, i],
                    *span,
                )
            },
            PExpressionData::NamedAccess(o, span, i) =>
                LExpressionData::Access(self.lower_expr(*o)?, *span, *i, self.fresh_infer_ty(*span)),
            PExpressionData::IndexAccess(o, span, i) => {
                let i = i.lookup(self.ctx).parse().map_err(|_| AError::NotANumber {
                    kind: "tuple index",
                    number: *i,
                    span: *span,
                })?;

                LExpressionData::IndexAccess(
                    self.lower_expr(*o)?,
                    *span,
                    i,
                    self.fresh_infer_ty(*span),
                )
            },
            PExpressionData::Await(a) => {
                if !self.scopes.last().unwrap().await_allowed {
                    return Err(AError::IllegalAwait { span: *span });
                }

                let a = self.lower_expr(*a)?;

                // Lower `e:await` into `poll_trampoline(p)`, which will do all of the heavy
                // work of unwinding the stack when our awaitable is incomplete
                if let LScopeItem::Function(trampoline_call) =
                    self.ctx.std_item("internal_poll_trampoline")
                {
                    LExpressionData::Call(
                        trampoline_call.into(),
                        vec![self.fresh_infer_ty(*span)],
                        vec![a],
                    )
                } else {
                    unreachable!()
                }
            },
        };

        Ok(LExpression {
            source: e,
            span: *span,
            data,
        }
        .intern(self.ctx))
    }

    pub fn lower_literal(&self, lit: PLiteral, span: Span) -> AResult<LLiteral> {
        Ok(match lit {
            PLiteral::True => LLiteral::True,
            PLiteral::False => LLiteral::False,
            PLiteral::String(s) => LLiteral::String(s),
            PLiteral::Char(c) => LLiteral::Char(c),
            PLiteral::Int(s) =>
                LLiteral::Int(s.lookup(self.ctx).parse().map_err(|_| AError::NotANumber {
                    kind: "integer literal",
                    number: s,
                    span,
                })?),
            PLiteral::Float(s) =>
            // Transmute the f64 into bits, since f64 is not hash, eq, etc. and we only care about
            // the _exact_ bitwise representation when impling this operation for LLiteral
                LLiteral::Float(f64::to_bits(s.lookup(self.ctx).parse().map_err(|_| {
                    AError::NotANumber {
                        kind: "float literal",
                        number: s,
                        span,
                    }
                })?)),
        })
    }

    /// Lower the provided information into a call which references a static
    /// method of a trait existing in the standard library.
    ///
    /// If `c` is provided, this is the call type (e.g. `<c as Trait>::f()`),
    /// otherwise, it is left as an infer type.
    ///
    /// Trait and function generics are obligatory and must match the expected
    /// generics, since there is no parity validation in this call.
    fn std_static_call(
        &self,
        is_method: bool,
        call_ty: Id<LType>,
        tr: &'static str,
        tr_generics: Vec<Id<LType>>,
        fun_name: &'static str,
        fun_generics: Vec<Id<LType>>,
        params: Vec<Id<LExpression>>,
        return_span: Span,
    ) -> LExpressionData {
        if let LScopeItem::Trait(tr) = self.ctx.std_item(tr) {
            LExpressionData::StaticCall(
                is_method,
                call_ty,
                Some(
                    LTraitType {
                        span: call_ty.lookup(self.ctx).span,
                        tr: tr.into(),
                        generics: tr_generics,
                    }
                    .intern(self.ctx),
                ),
                self.ctx.static_name(fun_name),
                fun_generics,
                params,
                self.fresh_infer_ty(return_span),
            )
        } else {
            unreachable!()
        }
    }

    fn lower_exprs(&mut self, es: &[Id<PExpression>]) -> AResult<Vec<Id<LExpression>>> {
        let mut ret = vec![];

        for e in es {
            ret.push(self.lower_expr(*e)?);
        }

        Ok(ret)
    }

    fn lower_expr_while(
        &mut self,
        source: Id<PExpression>,
        span: Span,
        label: Option<Id<str>>,
        condition: Id<PExpression>,
        then_expr: Id<PExpression>,
        else_expr: Id<PExpression>,
    ) -> AResult<LExpressionData> {
        let condition = self.lower_expr(condition)?;

        let label = self.enter_label(label);
        let then_expr = self.lower_expr(then_expr)?;

        let else_expr = LExpression {
            source,
            span,
            data: LExpressionData::Break(label, self.lower_expr(else_expr)?),
        }
        .intern(self.ctx);

        Ok(LExpressionData::Loop(
            label,
            LExpression {
                source,
                span,
                data: LExpressionData::If(condition, then_expr, else_expr),
            }
            .intern(self.ctx),
            self.fresh_infer_ty(span),
        ))
    }

    fn lower_expr_for(
        &mut self,
        source: Id<PExpression>,
        span: Span,
        label: Option<Id<str>>,
        pattern: Id<PPattern>,
        iterable: Id<PExpression>,
        then_expr: Id<PExpression>,
        else_expr: Id<PExpression>,
    ) -> AResult<LExpressionData> {
        // It's fine, we can leak these because they're unreferenceable
        let iterator_var = self.declare_variable(
            fresh_name("for").intern(self.ctx),
            span,
            self.fresh_infer_ty(span),
        );
        let new_iterator_var = self.declare_variable(
            fresh_name("for").intern(self.ctx),
            span,
            self.fresh_infer_ty(span),
        );

        let iterable = self.lower_expr(iterable)?;
        // lower the `ITERABLE` to
        // `let ITERATOR_VAR = <_ as Iterable>::iterator(ITERABLE).`
        let iterable_to_iterator = LStatement {
            source: None,
            span,
            data: LStatementData::Let(
                LPattern {
                    source: None,
                    span,
                    ty: Some(self.fresh_infer_ty(span)),
                    data: LPatternData::Variable(iterator_var),
                }
                .intern(self.ctx),
                LExpression {
                    source,
                    span,
                    data: self.std_static_call(
                        true,
                        self.fresh_infer_ty(span),
                        "Iterable",
                        vec![],
                        "iterator",
                        vec![],
                        vec![iterable],
                        span,
                    ),
                }
                .intern(self.ctx),
            ),
        }
        .intern(self.ctx);

        let next_call = LExpression {
            source,
            span,
            data: self.std_static_call(
                true,
                self.fresh_infer_ty(span),
                "Iterator",
                vec![],
                "next",
                vec![],
                vec![LExpression {
                    source,
                    span,
                    data: LExpressionData::Variable(iterator_var),
                }
                .intern(self.ctx)],
                span,
            ),
        }
        .intern(self.ctx);

        let option_enum = if let LScopeItem::Enum(option_enum) = self.ctx.std_item("Option") {
            option_enum
        } else {
            unreachable!()
        };

        self.enter_block();
        let pattern = self.lower_pattern(pattern)?;
        // This is gnarly... So basically what happens here is:
        // Destructure the return value from the `:next()` call into the new pattern
        // `(NEW_ITERATOR_VAR, Some(PATTERN))`, both to capture the return iterator
        // from next, and destructure the item that is yielded
        let good_pattern = LPattern {
            source: None,
            span,
            ty: Some(self.fresh_infer_ty(span)),
            data: LPatternData::Tuple(vec![
                LPattern {
                    source: None,
                    span,
                    ty: Some(self.fresh_infer_ty(span)),
                    data: LPatternData::EnumVariantPattern(
                        option_enum.into(),
                        vec![self.fresh_infer_ty(span)],
                        self.ctx.static_name("Some"),
                        vec![pattern],
                    ),
                }
                .intern(self.ctx),
                LPattern {
                    source: None,
                    span,
                    ty: Some(self.fresh_infer_ty(span)),
                    data: LPatternData::Variable(new_iterator_var),
                }
                .intern(self.ctx),
            ]),
        }
        .intern(self.ctx);

        let label = self.enter_label(label);
        // we need to insert the statement `ITERATOR_VAR = NEW_ITERATOR_VAR;`
        // before our THEN block, to reassign this returned iterator back into
        // what we're calling `:next()` on
        let good_path = LExpression {
            source,
            span,
            data: LExpressionData::Block(
                vec![LStatement {
                    source: None,
                    span,
                    data: LStatementData::Expression(
                        LExpression {
                            source,
                            span,
                            data: LExpressionData::Assign(
                                LExpression {
                                    source,
                                    span,
                                    data: LExpressionData::Variable(iterator_var),
                                }
                                .intern(self.ctx),
                                LExpression {
                                    source,
                                    span,
                                    data: LExpressionData::Variable(new_iterator_var),
                                }
                                .intern(self.ctx),
                            ),
                        }
                        .intern(self.ctx),
                    ),
                }
                .intern(self.ctx)],
                self.lower_expr(then_expr)?,
            ),
        }
        .intern(self.ctx);
        self.exit_label();
        self.exit_block();

        let bad_pattern = self.fresh_empty_pattern(span);
        let bad_path = LExpression {
            source,
            span,
            data: LExpressionData::Break(label, self.lower_expr(else_expr)?),
        }
        .intern(self.ctx);

        let unwrap_match = LExpression {
            source,
            span,
            data: LExpressionData::Match(next_call, vec![
                (good_pattern, good_path),
                (bad_pattern, bad_path),
            ]),
        }
        .intern(self.ctx);

        let unwrap_loop = LExpression {
            source,
            span,
            data: LExpressionData::Loop(label, unwrap_match, self.fresh_infer_ty(span)),
        }
        .intern(self.ctx);

        Ok(LExpressionData::Block(
            vec![iterable_to_iterator],
            unwrap_loop,
        ))
    }

    fn lower_expr_throw(
        &mut self,
        source: Id<PExpression>,
        span: Span,
        throwable: Id<PExpression>,
    ) -> AResult<LExpressionData> {
        let throwable = self.lower_expr(throwable)?;
        let throwable = LExpression {
            source,
            span,
            data: self.std_static_call(
                true,
                self.fresh_infer_ty(span),
                "IntoResult",
                vec![],
                "into_result",
                vec![],
                vec![throwable],
                span,
            ),
        }
        .intern(self.ctx);

        if !self.scopes.last_mut().unwrap().return_allowed {
            return Err(AError::IllegalReturn { span });
        }

        let good_ty = self.fresh_infer_ty(span);
        let bad_ty = self.fresh_infer_ty(span);

        let result_enum = if let LScopeItem::Enum(result_enum) = self.ctx.std_item("Result") {
            result_enum
        } else {
            unreachable!()
        };

        self.enter_block();

        let good_var = self.declare_variable(
            fresh_name("try").intern(self.ctx),
            span,
            self.fresh_infer_ty(span),
        );
        let bad_var = self.declare_variable(
            fresh_name("try").intern(self.ctx),
            span,
            self.fresh_infer_ty(span),
        );

        let good_name = LPattern {
            source: None,
            span,
            ty: Some(self.fresh_infer_ty(span)),
            data: LPatternData::Variable(good_var),
        }
        .intern(self.ctx);
        let good_pattern = LPattern {
            source: None,
            span,
            ty: Some(self.fresh_infer_ty(span)),
            data: LPatternData::EnumVariantPattern(
                result_enum.into(),
                vec![good_ty, bad_ty],
                self.ctx.static_name("Ok"),
                vec![good_name],
            ),
        }
        .intern(self.ctx);
        let good_value = LExpression {
            source,
            span,
            data: LExpressionData::Variable(good_var),
        }
        .intern(self.ctx);

        let bad_name = LPattern {
            source: None,
            span,
            ty: Some(self.fresh_infer_ty(span)),
            data: LPatternData::Variable(bad_var),
        }
        .intern(self.ctx);
        let bad_pattern = LPattern {
            source: None,
            span,
            ty: Some(self.fresh_infer_ty(span)),
            data: LPatternData::EnumVariantPattern(
                result_enum.into(),
                vec![good_ty, bad_ty],
                self.ctx.static_name("Error"),
                vec![bad_name],
            ),
        }
        .intern(self.ctx);
        let bad_path = LExpression {
            source,
            span,
            data: LExpressionData::Return(
                LExpression {
                    source,
                    span,
                    data: self.std_static_call(
                        false,
                        self.fresh_infer_ty(span),
                        "IntoResult",
                        vec![],
                        "from_error",
                        vec![],
                        vec![LExpression {
                            source,
                            span,
                            data: LExpressionData::Variable(bad_var),
                        }
                        .intern(self.ctx)],
                        span,
                    ),
                }
                .intern(self.ctx),
            ),
        }
        .intern(self.ctx);

        Ok(LExpressionData::Match(throwable, vec![
            (good_pattern, good_value),
            (bad_pattern, bad_path),
        ]))
    }

    fn lower_constructor(
        &mut self,
        parent_kind: &'static str,
        parent_name: Id<str>,
        shape: &LConstructorShape,
        args: &PConstructorArguments,
    ) -> AResult<Vec<(usize, Id<LExpression>)>> {
        match (shape, args) {
            (LConstructorShape::Empty(_), PConstructorArguments::Empty(_)) => Ok(vec![]),
            (LConstructorShape::Positional(s, n), PConstructorArguments::Positional(s2, es)) =>
                if es.len() == *n {
                    Ok(self.lower_exprs(es)?.into_iter().enumerate().collect())
                } else {
                    return Err(AError::ParityDisparity {
                        kind: "constructor",
                        expected: *n,
                        expected_span: *s,
                        given: es.len(),
                        given_span: *s2,
                    });
                },
            (
                LConstructorShape::Named(_, expected),
                PConstructorArguments::Named(constructor_span, given),
            ) => {
                let mut seen = hashmap! {};
                let mut args = vec![];

                for (is, n, e) in given {
                    if let Some(old_is) = seen.insert(*n, *is) {
                        return Err(AError::DuplicatedDefinition {
                            kind: "constructor field",
                            name: *n,
                            span: *is,
                            span2: old_is,
                        });
                    }
                    if let Some((pos, _)) = expected.get(n) {
                        args.push((*pos, self.lower_expr(*e)?));
                    } else {
                        return Err(AError::UnexpectedSubItem {
                            parent_kind,
                            parent_name,
                            item_kind: "field",
                            item_name: *n,
                            span: *is,
                        });
                    }
                }

                for (n, (_, is)) in expected {
                    if !seen.contains_key(n) {
                        return Err(AError::ExpectedField {
                            parent_kind,
                            parent_name,
                            item_name: *n,
                            def_span: *is,
                            use_span: *constructor_span,
                        });
                    }
                }

                Ok(args)
            },
            (expected, given) => {
                let (expected_kind, expected_span) = expected.info();
                let (given_kind, given_span) = given.info();

                return Err(AError::IncorrectConstructor {
                    parent_kind,
                    parent_name,
                    expected_kind,
                    expected_span,
                    given_kind,
                    given_span,
                });
            },
        }
    }

    fn lower_statement(&mut self, s: Id<PStatement>) -> AResult<Id<LStatement>> {
        let PStatement { span, data } = &*s.lookup(self.ctx);
        let data = match data {
            PStatementData::Expression(e) => LStatementData::Expression(self.lower_expr(*e)?),
            PStatementData::Let(p, e) => {
                let e = self.lower_expr(*e)?;
                let p = self.lower_pattern(*p)?;

                LStatementData::Let(p, e)
            },
        };
        Ok(LStatement {
            source: Some(s),
            span: *span,
            data,
        }
        .intern(self.ctx))
    }

    fn lower_statements(&mut self, ss: &[Id<PStatement>]) -> AResult<Vec<Id<LStatement>>> {
        let mut ret = vec![];
        for s in ss {
            ret.push(self.lower_statement(*s)?);
        }
        Ok(ret)
    }

    /// Return the Bound enum variant of the given `kind`, with the given
    /// parameters (for use in range operator desugaring)
    fn get_range_bound(
        &self,
        span: Span,
        source: Id<PExpression>,
        kind: &'static str,
        parameters: Vec<Id<LExpression>>,
    ) -> Id<LExpression> {
        if let LScopeItem::Enum(bound) = self.ctx.std_item("Bound") {
            LExpression {
                source,
                span,
                data: LExpressionData::EnumConstructor(
                    bound.into(),
                    self.fresh_infer_tys(1, span),
                    self.ctx.static_name(kind),
                    parameters.into_iter().enumerate().collect(),
                ),
            }
            .intern(self.ctx)
        } else {
            unreachable!()
        }
    }
}
