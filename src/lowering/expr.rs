use crate::{
    lexer::Span,
    parser::{
        PBinopKind, PConstructorArguments, PExpression, PExpressionData, PLiteral, PPattern,
        PStatement, PStatementData,
    },
    util::{AError, AResult, Id, Intern, LId},
};

use super::{
    fresh_name, LConstructorShape, LEnum, LFunction, LGlobal, LObject, LPattern, LPatternData,
    LScopeItem, LTraitType, LType, LVariable, LVariableContext, LoweringContext,
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
    Literal(PLiteral),
    Variable(LVariable),
    Block(Vec<Id<LStatement>>, Id<LExpression>),
    AsyncBlock(LVariableContext, Id<LExpression>),
    Global(LId<LGlobal>),
    GlobalFunction(LId<LFunction>, Vec<Id<LType>>),
    Access(Id<LExpression>, Span, Id<str>),
    IndexAccess(Id<LExpression>, Span, usize),
    Tuple(Vec<Id<LExpression>>),
    ArrayLiteral(Vec<Id<LExpression>>),
    Assign(Id<LExpression>, Id<LExpression>),
    StaticCall(
        Id<LType>,
        Option<Id<LTraitType>>,
        Id<str>,
        Vec<Id<LType>>,
        Vec<Id<LExpression>>,
    ),
    Call(LId<LFunction>, Vec<Id<LType>>, Vec<Id<LExpression>>),
    Or(Id<LExpression>, Id<LExpression>),
    And(Id<LExpression>, Id<LExpression>),
    Await(Id<LExpression>),
    Return(Id<LExpression>),
    Break(usize, Id<LExpression>),
    Continue(usize),
    StructConstructor(LId<LObject>, Vec<Id<LType>>, Vec<(usize, Id<LExpression>)>),
    ObjectAllocation(LId<LObject>, Vec<Id<LType>>, Vec<(usize, Id<LExpression>)>),
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
    Loop(usize, Id<LExpression>),
    Match(Id<LExpression>, Vec<(Id<LPattern>, Id<LExpression>)>),
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
            PExpressionData::Unimplemented =>
            // Lower into the `std::unimplemented::<T>()` fn call
                if let LScopeItem::Function(unimplemented) = self.lookup_std_item("unimplemented") {
                    LExpressionData::Call(unimplemented.into(), vec![self.fresh_infer_ty()], vec![])
                } else {
                    unreachable!()
                },
            PExpressionData::Identifiers(id, generics) => match self.lookup_path_partial(&id)? {
                (span, LScopeItem::Function(f), &[]) => {
                    let info = f.lookup(self.ctx);

                    let generics = self.lower_tys(generics, true)?;
                    let generics = self.check_generics_parity(
                        generics,
                        span,
                        info.generics.len(),
                        info.span,
                        true,
                    )?;

                    LExpressionData::GlobalFunction(f.into(), generics)
                },
                (_, LScopeItem::Function(f), &[(span, mem), ..]) => {
                    let info = f.lookup(self.ctx);
                    return Err(AError::CannotAccessMembers {
                        kind: "function",
                        name: info.name,
                        mem,
                        use_span: span,
                        def_span: info.span,
                    });
                },
                (span, LScopeItem::Variable(v), rest) => {
                    let data =
                        self.lower_expr_accesses(span, e, LExpressionData::Variable(v), rest)?;

                    if !generics.is_empty() {
                        return Err(AError::DenyGenerics {
                            kind: "variable",
                            name: v.name,
                            use_span: span,
                            def_span: v.span,
                        });
                    }

                    data
                },
                (span, LScopeItem::Global(g), rest) => {
                    let data =
                        self.lower_expr_accesses(span, e, LExpressionData::Global(g.into()), rest)?;

                    if !generics.is_empty() {
                        let info = g.lookup(self.ctx);

                        return Err(AError::DenyGenerics {
                            kind: "global",
                            name: info.name,
                            use_span: span,
                            def_span: info.span,
                        });
                    }

                    data
                },
                (span, i, _) => {
                    let (kind, name, def_span) = i.info(self.ctx);

                    return Err(AError::NotAnExpression {
                        kind,
                        name,
                        def_span,
                        use_span: span,
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

                LExpressionData::AsyncBlock(ctx, e)
            },
            PExpressionData::Tuple(es) => LExpressionData::Tuple(self.lower_exprs(es)?),
            PExpressionData::ArrayLiteral(es) =>
                LExpressionData::ArrayLiteral(self.lower_exprs(es)?),
            PExpressionData::Array(a, n) => {
                let a = self.lower_ty(*a, true)?;
                let n = self.lower_expr(*n)?;
                self.std_static_call(
                    Some(a),
                    "AllocateArray",
                    vec![],
                    "allocate_array",
                    vec![],
                    vec![n],
                )
            },
            PExpressionData::Literal(l) => LExpressionData::Literal(*l),
            PExpressionData::InfiniteRange(a) => {
                let a = self.lower_expr(*a)?;
                let b = self.get_range_bound(*span, e, "Unbounded", vec![]);
                self.std_static_call(
                    None,
                    "Range",
                    self.fresh_infer_tys(1),
                    "range",
                    vec![],
                    vec![a, b],
                )
            },
            PExpressionData::BinOp(a, PBinopKind::RangeInclusive, b) => {
                let a = self.lower_expr(*a)?;
                let b = self.lower_expr(*b)?;
                let b = self.get_range_bound(*span, e, "Inclusive", vec![b]);
                self.std_static_call(
                    None,
                    "Range",
                    self.fresh_infer_tys(1),
                    "range",
                    vec![],
                    vec![a, b],
                )
            },
            PExpressionData::BinOp(a, PBinopKind::RangeExclusive, b) => {
                let a = self.lower_expr(*a)?;
                let b = self.lower_expr(*b)?;
                let b = self.get_range_bound(*span, e, "Exclusive", vec![b]);
                self.std_static_call(
                    None,
                    "Range",
                    self.fresh_infer_tys(1),
                    "range",
                    vec![],
                    vec![a, b],
                )
            },
            PExpressionData::BinOp(a, PBinopKind::OrCircuit, b) =>
                LExpressionData::Or(self.lower_expr(*a)?, self.lower_expr(*b)?),
            PExpressionData::BinOp(a, PBinopKind::AndCircuit, b) =>
                LExpressionData::And(self.lower_expr(*a)?, self.lower_expr(*b)?),
            PExpressionData::BinOp(a, op, b) => {
                let a = self.lower_expr(*a)?;
                let b = self.lower_expr(*b)?;

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
                    PBinopKind::Minus => ("Subtract", "subtract"),
                    PBinopKind::Mul => ("Multiply", "mul"),
                    PBinopKind::Div => ("Divide", "div"),
                    PBinopKind::Mod => ("Modulo", "mod"),
                    PBinopKind::OrCircuit
                    | PBinopKind::AndCircuit
                    | PBinopKind::RangeInclusive
                    | PBinopKind::RangeExclusive => unreachable!(),
                };

                self.std_static_call(None, tr, self.fresh_infer_tys(1), f, vec![], vec![a, b])
            },
            PExpressionData::Assign(a, b) => match &a.lookup(self.ctx).data {
                PExpressionData::Index(a, i) => {
                    let a = self.lower_expr(*a)?;
                    let b = self.lower_expr(*b)?;
                    let i = self.lower_expr(*i)?;

                    self.std_static_call(
                        None,
                        "DerefAssign",
                        self.fresh_infer_tys(1),
                        "deref_assign",
                        vec![],
                        vec![a, i, b],
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
                self.std_static_call(None, "Not", vec![], "not", vec![], vec![e])
            },
            PExpressionData::Neg(e) => {
                let e = self.lower_expr(*e)?;
                self.std_static_call(None, "Negate", vec![], "negate", vec![], vec![e])
            },
            PExpressionData::InterpolationBegin(l, s) => {
                let string = LType::String.intern(self.ctx);
                let l = LExpression {
                    source: e,
                    span: *span,
                    data: LExpressionData::Literal(PLiteral::String(*l)),
                }
                .intern(self.ctx);
                let s = self.lower_expr(*s)?;

                self.std_static_call(Some(string), "Add", vec![string], "add", vec![], vec![l, s])
            },
            PExpressionData::InterpolationContinue(a, l, s) => {
                let string = LType::String.intern(self.ctx);
                let l = LExpression {
                    source: e,
                    span: *span,
                    data: LExpressionData::Literal(PLiteral::String(*l)),
                }
                .intern(self.ctx);
                let a = self.lower_expr(*a)?;
                let a = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(None, "Into", vec![string], "into", vec![], vec![a]),
                }
                .intern(self.ctx);
                let al = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(
                        Some(string),
                        "Add",
                        vec![string],
                        "add",
                        vec![],
                        vec![a, l],
                    ),
                }
                .intern(self.ctx);
                let s = self.lower_expr(*s)?;

                self.std_static_call(Some(string), "Add", vec![string], "add", vec![], vec![
                    al, s,
                ])
            },
            PExpressionData::InterpolationEnd(a, l) => {
                let string = LType::String.intern(self.ctx);
                let l = LExpression {
                    source: e,
                    span: *span,
                    data: LExpressionData::Literal(PLiteral::String(*l)),
                }
                .intern(self.ctx);
                let a = self.lower_expr(*a)?;
                let a = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(None, "Into", vec![string], "into", vec![], vec![a]),
                }
                .intern(self.ctx);

                self.std_static_call(Some(string), "Add", vec![string], "add", vec![], vec![a, l])
            },
            PExpressionData::Call(c, ps) => {
                let c = self.lower_expr(*c)?;
                let ps = self.lower_exprs(ps)?;

                if let LExpression {
                    data: LExpressionData::GlobalFunction(f, g),
                    ..
                } = &*c.lookup(self.ctx)
                {
                    LExpressionData::Call(f.clone(), g.clone(), ps)
                } else {
                    let ps = LExpression {
                        source: e,
                        span: *span,
                        data: LExpressionData::Tuple(ps),
                    }
                    .intern(self.ctx);

                    self.std_static_call(
                        None,
                        "Call",
                        self.fresh_infer_tys(1),
                        "call",
                        vec![],
                        vec![c, ps],
                    )
                }
            },
            PExpressionData::StaticCall(t, n, g, p) => {
                let (t, tr) = self.lower_elaborated_ty(*t, true)?;
                LExpressionData::StaticCall(
                    t,
                    tr,
                    *n,
                    self.lower_tys(g, true)?,
                    self.lower_exprs(p)?,
                )
            },
            PExpressionData::ObjectCall(e, n, g, p) => {
                let mut p = self.lower_exprs(&p)?;
                p.insert(0, self.lower_expr(*e)?);
                LExpressionData::StaticCall(
                    self.fresh_infer_ty(),
                    None,
                    *n,
                    self.lower_tys(g, true)?,
                    p,
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
                    (
                        LPattern {
                            source: None,
                            span: *span,
                            data: LPatternData::Underscore,
                            ty: self.fresh_infer_ty(),
                        }
                        .intern(self.ctx),
                        self.lower_expr(*e)?,
                    ),
                ]),
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

                    let g = self.lower_tys(g, true)?;
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

                    let g = self.fresh_infer_tys(e.lookup(self.ctx).generics.len());

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

                    let g = self.lower_tys(g, true)?;
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
                    let info = o.lookup(self.ctx);
                    if info.is_structural {
                        return Err(AError::TriedAllocatingStruct {
                            parent_name: info.name,
                            parent_span: info.span,
                            use_span: *span,
                        });
                    }

                    let g = self.lower_tys(g, true)?;
                    let g =
                        self.check_generics_parity(g, *span, info.generics.len(), info.span, true)?;

                    let s = self.ctx.object_constructor(o)?;
                    let a = self.lower_constructor("object", info.name, &s, a)?;

                    LExpressionData::ObjectAllocation(o.into(), g, a)
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
                if let LScopeItem::Function(assert) = self.lookup_std_item("assert_impl") {
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
                let r = self.lower_ty(*r, true)?;

                let vcx = self.exit_context();

                LExpressionData::Closure(vcx, ps, r, e)
            },
            PExpressionData::Throw(t) => self.lower_expr_throw(e, *span, *t)?,
            PExpressionData::Index(a, i) => {
                let a = self.lower_expr(*a)?;
                let i = self.lower_expr(*i)?;
                self.std_static_call(
                    None,
                    "Deref",
                    self.fresh_infer_tys(1),
                    "deref",
                    vec![],
                    vec![a, i],
                )
            },
            PExpressionData::NamedAccess(o, span, i) =>
                LExpressionData::Access(self.lower_expr(*o)?, *span, *i),
            PExpressionData::IndexAccess(o, span, i) => {
                let i = i.lookup(self.ctx).parse().map_err(|_| AError::NotANumber {
                    kind: "tuple index",
                    number: *i,
                    span: *span,
                })?;

                LExpressionData::IndexAccess(self.lower_expr(*o)?, *span, i)
            },
            PExpressionData::Await(a) => {
                if !self.scopes.last().unwrap().await_allowed {
                    return Err(AError::IllegalAwait { span: *span });
                }

                LExpressionData::Await(self.lower_expr(*a)?)
            },
        };

        Ok(LExpression {
            source: e,
            span: *span,
            data,
        }
        .intern(self.ctx))
    }

    fn std_static_call(
        &self,
        c: Option<Id<LType>>,
        tr: &'static str,
        tr_generics: Vec<Id<LType>>,
        f: &'static str,
        f_generics: Vec<Id<LType>>,
        params: Vec<Id<LExpression>>,
    ) -> LExpressionData {
        if let LScopeItem::Trait(tr) = self.lookup_std_item(tr) {
            LExpressionData::StaticCall(
                c.unwrap_or_else(|| self.fresh_infer_ty()),
                Some(
                    LTraitType {
                        tr: tr.into(),
                        generics: tr_generics,
                        bounds: btreemap! {},
                    }
                    .intern(self.ctx),
                ),
                self.ctx.static_name(f),
                f_generics,
                params,
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

    fn lower_expr_accesses(
        &self,
        mut span: Span,
        source: Id<PExpression>,
        mut data: LExpressionData,
        accesses: &[(Span, Id<str>)],
    ) -> AResult<LExpressionData> {
        for (a_span, a) in accesses {
            data = LExpressionData::Access(
                LExpression { source, span, data }.intern(self.ctx),
                *a_span,
                *a,
            );
            span = span.unite(*a_span);
        }

        Ok(data)
    }

    fn lower_expr_while(
        &mut self,
        source: Id<PExpression>,
        span: Span,
        label: Option<Id<str>>,
        condition: Id<PExpression>,
        t: Id<PExpression>,
        e: Id<PExpression>,
    ) -> AResult<LExpressionData> {
        let condition = self.lower_expr(condition)?;

        let label = self.enter_label(label);
        let t = self.lower_expr(t)?;
        self.exit_label();

        let e = LExpression {
            source,
            span,
            data: LExpressionData::Break(label, self.lower_expr(e)?),
        }
        .intern(self.ctx);

        Ok(LExpressionData::Loop(
            label,
            LExpression {
                source,
                span,
                data: LExpressionData::If(condition, t, e),
            }
            .intern(self.ctx),
        ))
    }

    fn lower_expr_for(
        &mut self,
        source: Id<PExpression>,
        span: Span,
        label: Option<Id<str>>,
        pattern: Id<PPattern>,
        iterable: Id<PExpression>,
        t: Id<PExpression>,
        e: Id<PExpression>,
    ) -> AResult<LExpressionData> {
        let iterator_var = self.declare_variable(
            fresh_name("for").intern(self.ctx),
            span,
            self.fresh_infer_ty(),
        );

        let iterable = self.lower_expr(iterable)?;
        let iterable_to_iterator = LStatement {
            source: None,
            span,
            data: LStatementData::Let(
                LPattern {
                    source: None,
                    span,
                    ty: self.fresh_infer_ty(),
                    data: LPatternData::Variable(iterator_var),
                }
                .intern(self.ctx),
                LExpression {
                    source,
                    span,
                    data: self.std_static_call(None, "Iterable", vec![], "iterator", vec![], vec![
                        iterable,
                    ]),
                }
                .intern(self.ctx),
            ),
        }
        .intern(self.ctx);

        let next_call = LExpression {
            source,
            span,
            data: self.std_static_call(None, "Iterator", vec![], "next", vec![], vec![
                LExpression {
                    source,
                    span,
                    data: LExpressionData::Variable(iterator_var),
                }
                .intern(self.ctx),
            ]),
        }
        .intern(self.ctx);

        let option_enum = if let LScopeItem::Enum(option_enum) = self.lookup_std_item("Option") {
            option_enum
        } else {
            unreachable!()
        };

        self.enter_block();
        let pattern = self.lower_pattern(pattern)?;
        let good_pattern = LPattern {
            source: None,
            span,
            ty: self.fresh_infer_ty(),
            data: LPatternData::EnumVariantPattern(
                option_enum.into(),
                vec![self.fresh_infer_ty()],
                self.ctx.static_name("Some"),
                vec![pattern],
            ),
        }
        .intern(self.ctx);

        let label = self.enter_label(label);
        let good_path = self.lower_expr(t)?;
        self.exit_label();
        self.exit_block();

        let bad_pattern = LPattern {
            source: None,
            span,
            ty: self.fresh_infer_ty(),
            data: LPatternData::Underscore,
        }
        .intern(self.ctx);
        let bad_path = LExpression {
            source,
            span,
            data: LExpressionData::Break(label, self.lower_expr(e)?),
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
            data: LExpressionData::Loop(label, unwrap_match),
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

        let good_ty = self.fresh_infer_ty();
        let bad_ty = self.fresh_infer_ty();

        let result_enum = if let LScopeItem::Enum(result_enum) = self.lookup_std_item("Result") {
            result_enum
        } else {
            unreachable!()
        };

        self.enter_block();

        let good_var = self.declare_variable(
            fresh_name("try").intern(self.ctx),
            span,
            self.fresh_infer_ty(),
        );
        let bad_var = self.declare_variable(
            fresh_name("try").intern(self.ctx),
            span,
            self.fresh_infer_ty(),
        );

        let good_name = LPattern {
            source: None,
            span,
            ty: self.fresh_infer_ty(),
            data: LPatternData::Variable(good_var),
        }
        .intern(self.ctx);
        let good_pattern = LPattern {
            source: None,
            span,
            ty: self.fresh_infer_ty(),
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

        let bad_pattern = LPattern {
            source: None,
            span,
            ty: self.fresh_infer_ty(),
            data: LPatternData::Variable(good_var),
        }
        .intern(self.ctx);
        let bad_path = LExpression {
            source,
            span,
            data: LExpressionData::Return(
                LExpression {
                    source,
                    span,
                    data: LExpressionData::Variable(bad_var),
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
}
