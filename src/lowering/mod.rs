mod uses;

use std::{
    collections::{BTreeMap, HashMap},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    parser::{
        PBinopKind, PConstructorArguments, PEnum, PExpression, PExpressionData, PFunction, PGlobal,
        PImpl, PLiteral, PModule, PObject, PPattern, PPatternConstructorArguments, PPatternData,
        PStatement, PStatementData, PTrait, PTraitMember, PTraitType, PType, PTypeData,
    },
    util::{AError, AResult, Id, Intern, LId, LateLookup},
};

pub use uses::{
    early_lookup_ctx, local_mod_items, lookup_item, lookup_item_early, lower_mod_base, mod_items,
    LEarlyContext, LUseError, LUseItem, LUseResult,
};

static IDS: AtomicUsize = AtomicUsize::new(0);

fn fresh_id() -> usize {
    IDS.fetch_add(1, Ordering::Relaxed)
}

fn fresh_name(kind: &'static str) -> String {
    format!("${}{}", kind, fresh_id())
}

fn fresh_infer_ty() -> LType {
    LType::Infer(fresh_id())
}

pub fn lower_root(ctx: &dyn AdelaideContext) -> AResult<Id<LModule>> {
    ctx.lower_mod(ctx.parse_root()?)
}

pub fn lower_mod(ctx: &dyn AdelaideContext, key: Id<PModule>) -> AResult<Id<LModule>> {
    let info = key.lookup(ctx);

    let mut lcx = LoweringContext::try_new(ctx, key)?;

    let mut modules = btreemap! {};
    let mut globals = btreemap! {};
    let mut functions = btreemap! {};
    let mut objects = btreemap! {};
    let enums = btreemap! {};
    let traits = btreemap! {};
    let impls = btreemap! {};

    for (&n, &i) in &*ctx.local_mod_items(key)? {
        match i {
            LScopeItem::Global(g) => {
                globals.insert(g, lcx.lower_global(g)?);
            },
            LScopeItem::Module(m) => {
                modules.insert(m, lower_mod(ctx, m)?);
            },
            LScopeItem::Function(f) => todo!(),
            LScopeItem::Object(o) => todo!(),
            LScopeItem::Enum(e) => todo!(),
            LScopeItem::Trait(_) => todo!(),
            LScopeItem::EnumVariant(e, v) => todo!(),
            LScopeItem::Variable(_) => unreachable!(),
        }
    }

    Ok(LModule {
        source: key,
        parent: info.parent.get(ctx).into(),
        modules,
        globals,
        functions,
        objects,
        enums,
        traits,
        impls,
    }
    .intern(ctx))
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LModule {
    #[plain]
    source: Id<PModule>,
    parent: LId<LModule>,
    modules: BTreeMap<Id<PModule>, Id<LModule>>,
    globals: BTreeMap<Id<PGlobal>, Id<LGlobal>>,
    functions: BTreeMap<Id<PFunction>, Id<LFunction>>,
    objects: BTreeMap<Id<PObject>, Id<LObject>>,
    enums: BTreeMap<Id<PEnum>, Id<LEnum>>,
    traits: BTreeMap<Id<PTrait>, Id<LTrait>>,
    impls: BTreeMap<Id<PImpl>, Id<LImpl>>,
}

impl LateLookup for LModule {
    type Source = PModule;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .modules
            .get(&id)
            .unwrap()
    }
}

struct LoweringContext<'ctx> {
    ctx: &'ctx dyn AdelaideContext,
    module: Id<PModule>,
    base_items: Arc<HashMap<Id<str>, LScopeItem>>,
    scopes: Vec<FunctionScope>,
}

impl<'ctx> LoweringContext<'ctx> {
    fn try_new(
        ctx: &'ctx dyn AdelaideContext,
        parent: Id<PModule>,
    ) -> AResult<LoweringContext<'ctx>> {
        let base_items = ctx.mod_items(parent)?;
        Ok(LoweringContext {
            ctx,
            module: parent,
            base_items,
            scopes: vec![],
        })
    }

    fn enter_context(&mut self, return_allowed: bool, await_allowed: bool) {
        self.scopes
            .push(FunctionScope::new(return_allowed, await_allowed))
    }

    fn exit_context(&mut self) -> LVariableContext {
        self.scopes.pop().unwrap().vcx
    }

    fn enter_block(&mut self) {
        self.scopes
            .last_mut()
            .unwrap()
            .variable_scopes
            .push(hashmap! {})
    }

    fn exit_block(&mut self) {
        self.scopes
            .last_mut()
            .unwrap()
            .variable_scopes
            .pop()
            .unwrap();
    }

    fn lookup_std_item(&self, name: &'static str) -> LScopeItem {
        // We expect none of these results to be an error, so just unwrap them.
        *self
            .ctx
            .mod_items(self.ctx.parse_root().unwrap())
            .unwrap()
            .get(&self.ctx.static_name(name))
            .unwrap()
    }

    fn lookup_path(&mut self, path: &[(Span, Id<str>)]) -> AResult<LScopeItem> {
        match self.lookup_path_partial(path)? {
            (_, i, &[]) => Ok(i),
            (_, i, &[(s, _), ..]) => Err(AError::ScopeItemNotAModule(i, s)),
        }
    }

    fn lookup_path_partial<'p>(
        &mut self,
        path: &'p [(Span, Id<str>)],
    ) -> AResult<(Span, LScopeItem, &'p [(Span, Id<str>)])> {
        assert!(!path.is_empty());
        let ((mut span, name), path) = path.split_first().unwrap();

        let mut item = self.lookup_scoped_item(span, *name)?;

        for (i, (next_span, name)) in path.iter().enumerate() {
            span = span.unite(*next_span);

            match item {
                LScopeItem::Module(m) => {
                    item = self.ctx.lookup_item(span, m, *name)?;
                },
                LScopeItem::Enum(e) => {
                    let info = e.lookup(self.ctx);

                    if info.variants.iter().any(|(_, n, _)| n == name) {
                        return Ok((span, LScopeItem::EnumVariant(e, *name), &path[i + 1..]));
                    } else {
                        return Err(AError::MissingIn(
                            "enum", info.name, info.span, "variant", *name, *next_span,
                        ));
                    }
                },
                item => {
                    return Ok((span, item, &path[i + 1..]));
                },
            }
        }

        Ok((span, item, &[]))
    }

    fn lookup_scoped_item(&mut self, at: Span, name: Id<str>) -> AResult<LScopeItem> {
        if let Some(item) = Self::lookup_function_scoped_item(at, name, &mut self.scopes) {
            Ok(LScopeItem::Variable(item))
        } else if let Some(item) = self.base_items.get(&name) {
            Ok(*item)
        } else {
            todo!("Die")
        }
    }

    fn lookup_function_scoped_item(
        at: Span,
        name: Id<str>,
        scopes: &mut [FunctionScope],
    ) -> Option<LVariable> {
        if scopes.is_empty() {
            return None;
        }

        let (scope, scopes) = scopes.split_last_mut().unwrap();

        if let Some(item) = scope.get(name) {
            Some(item)
        } else if let Some(captured_item) = Self::lookup_function_scoped_item(at, name, scopes) {
            Some(scope.capture(captured_item))
        } else {
            None
        }
    }

    fn lower_ty(&mut self, t: Id<PType>, infer_allowed: bool) -> AResult<Id<LType>> {
        let PType { data, span } = &*t.lookup(self.ctx);

        let data = match data {
            PTypeData::Infer =>
                if infer_allowed {
                    fresh_infer_ty()
                } else {
                    todo!("Die!")
                },
            PTypeData::Awaitable(a) => {
                if let LScopeItem::Object(awaitable) = self.lookup_std_item("Awaitable") {
                    LType::Object(awaitable.into(), vec![self.lower_ty(*a, infer_allowed)?])
                } else {
                    unreachable!()
                }
            },
            PTypeData::AmbiguousPath(p, g) => {
                let generics = self.lower_tys(g, infer_allowed)?;

                match self.lookup_path(&*p)? {
                    LScopeItem::Enum(e) => {
                        let generics = self.check_generics_parity(
                            *span,
                            generics,
                            e.lookup(self.ctx).generics.len(),
                            infer_allowed,
                        )?;
                        LType::Enum(e.into(), generics)
                    },
                    LScopeItem::Object(o) => {
                        let generics = self.check_generics_parity(
                            *span,
                            generics,
                            o.lookup(self.ctx).generics.len(),
                            infer_allowed,
                        )?;
                        LType::Object(o.into(), generics)
                    },
                    otherwise => {
                        todo!("Die, not an object or enum or struct");
                    },
                }
            },
            PTypeData::Associated(ty, m) => {
                let (ty, trt) = self.lower_elaborated_ty(*ty, infer_allowed)?;
                LType::Associated(ty, trt, *m)
            },
            PTypeData::Closure(es, r) => LType::Closure(
                self.lower_tys(es, infer_allowed)?,
                self.lower_ty(*r, infer_allowed)?,
            ),
            PTypeData::FnPtr(es, r) => LType::FnPtr(
                self.lower_tys(es, infer_allowed)?,
                self.lower_ty(*r, infer_allowed)?,
            ),
            PTypeData::Elaborated(..) => todo!("Die"),
            PTypeData::Int => LType::Int,
            PTypeData::Float => LType::Float,
            PTypeData::Char => LType::Char,
            PTypeData::Bool => LType::Bool,
            PTypeData::String => LType::String,
            PTypeData::SelfType => LType::SelfType,
            PTypeData::Array(e) => LType::Array(self.lower_ty(*e, infer_allowed)?),
            PTypeData::Tuple(es) => LType::Tuple(self.lower_tys(es, infer_allowed)?),
            PTypeData::Dynamic(ts) => LType::Dynamic(self.lower_trait_tys(ts, infer_allowed)?),
        };

        Ok(data.intern(self.ctx))
    }

    fn lower_tys(&mut self, ts: &[Id<PType>], infer_allowed: bool) -> AResult<Vec<Id<LType>>> {
        let mut ret = vec![];

        for t in ts {
            ret.push(self.lower_ty(*t, infer_allowed)?);
        }

        Ok(ret)
    }

    fn lower_elaborated_ty(
        &mut self,
        t: Id<PType>,
        infer_allowed: bool,
    ) -> AResult<(Id<LType>, Option<Id<LTraitType>>)> {
        match &*t.lookup(self.ctx) {
            PType {
                data: PTypeData::Elaborated(t, trt),
                ..
            } => Ok((
                self.lower_ty(*t, infer_allowed)?,
                Some(self.lower_trait_ty(*trt, infer_allowed)?),
            )),
            // Otherwise, we did not extract an elaborated trait...
            _ => Ok((self.lower_ty(t, infer_allowed)?, None)),
        }
    }

    fn lower_trait_ty(
        &mut self,
        t: Id<PTraitType>,
        infer_allowed: bool,
    ) -> AResult<Id<LTraitType>> {
        match &*t.lookup(self.ctx) {
            PTraitType::Plain {
                span,
                path,
                generics,
                bounds,
            } => {
                let tr = match self.lookup_path(path)? {
                    LScopeItem::Trait(t) => t,
                    otherwise => todo!("Die"),
                };

                let generics = self.lower_tys(generics, infer_allowed)?;
                let generics = self.check_generics_parity(
                    *span,
                    generics,
                    tr.lookup(self.ctx).generics.len(),
                    infer_allowed,
                )?;

                let trait_bounds = self.ctx.get_bound_names(tr)?;
                let mut lowered_at = hashmap! {};
                let mut lowered_bounds = btreemap! {};

                for (span, name, ty) in bounds {
                    if !trait_bounds.contains_key(name) {
                        let tr = tr.lookup(self.ctx);
                        return Err(AError::MissingIn(
                            "trait",
                            tr.name,
                            tr.span,
                            "associated type",
                            *name,
                            *span,
                        ));
                    }

                    if let Some(old_span) = lowered_at.get(name) {
                        return Err(AError::Duplicated(
                            "trait bound",
                            *old_span,
                            "trait bound",
                            *span,
                            "trait bound",
                            name.lookup(self.ctx).to_string(),
                        ));
                    }

                    lowered_at.insert(*name, *span);
                    lowered_bounds.insert(*name, self.lower_ty(*ty, infer_allowed)?);
                }

                Ok(LTraitType {
                    tr: tr.into(),
                    generics,
                    bounds: lowered_bounds,
                }
                .intern(self.ctx))
            },
            PTraitType::Function { span, params, ret } => {
                let tr = if let LScopeItem::Trait(tr) = self.lookup_std_item("Call") {
                    tr.into()
                } else {
                    unreachable!()
                };

                let generics =
                    vec![LType::Tuple(self.lower_tys(params, infer_allowed)?).intern(self.ctx)];

                let bounds = btreemap! { self.ctx.static_name("Ret") => self.lower_ty(*ret, infer_allowed)? };

                Ok(LTraitType {
                    tr,
                    generics,
                    bounds,
                }
                .intern(self.ctx))
            },
        }
    }

    fn lower_trait_tys(
        &mut self,
        ts: &[Id<PTraitType>],
        infer_allowed: bool,
    ) -> AResult<Vec<Id<LTraitType>>> {
        let mut ret = vec![];

        for t in ts {
            ret.push(self.lower_trait_ty(*t, infer_allowed)?);
        }

        Ok(ret)
    }

    fn lower_global(&mut self, g: Id<PGlobal>) -> AResult<Id<LGlobal>> {
        let PGlobal {
            span,
            name,
            ty,
            expr,
            ..
        } = &*g.lookup(self.ctx);

        self.enter_context(false, false);

        let ty = self.lower_ty(*ty, false)?;
        let expr = self.lower_expr(*expr)?;

        let vcx = self.exit_context();

        Ok(LGlobal {
            source: g,
            span: *span,
            name: *name,
            ty,
            expr,
            vcx,
        }
        .intern(self.ctx))
    }

    fn lower_expr(&mut self, e: Id<PExpression>) -> AResult<Id<LExpression>> {
        let PExpression { span, data } = &*e.lookup(self.ctx);

        let data = match data {
            PExpressionData::Unimplemented => LExpressionData::Unimplemented,
            PExpressionData::Identifiers(id, generics) => match self.lookup_path_partial(&id)? {
                (span, LScopeItem::Function(f), &[]) => {
                    let generics = self.lower_tys(generics, true)?;
                    let generics = self.check_generics_parity(
                        span,
                        generics,
                        f.lookup(self.ctx).generics.len(),
                        true,
                    )?;

                    LExpressionData::GlobalFunction(f.into(), generics)
                },
                (span, LScopeItem::Function(f), &[(access_span, mem), ..]) => {
                    todo!("Die: Cannot access function $f at $mem... defined here X");
                },
                (span, LScopeItem::Variable(v), rest) => {
                    let data =
                        self.lower_expr_accesses(span, e, LExpressionData::Variable(v), rest)?;

                    if !generics.is_empty() {
                        todo!("Die: Deny generics on variable");
                    }

                    data
                },
                (span, LScopeItem::Global(g), rest) => {
                    let data =
                        self.lower_expr_accesses(span, e, LExpressionData::Global(g.into()), rest)?;

                    if !generics.is_empty() {
                        todo!("Die: Deny generics on global");
                    }

                    data
                },
                (span, i, _) => {
                    todo!("Die: Cannot access $what $item as an expression")
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
                let a = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(None, "into", vec![string], "into", vec![], vec![
                        self.lower_expr(*a)?,
                    ]),
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
                let a = LExpression {
                    source: e,
                    span: *span,
                    data: self.std_static_call(None, "into", vec![string], "into", vec![], vec![
                        self.lower_expr(*a)?,
                    ]),
                }
                .intern(self.ctx);

                self.std_static_call(Some(string), "Add", vec![string], "add", vec![], vec![a, l])
            },
            PExpressionData::Call(c, ps) => {
                let c = self.lower_expr(*c)?;
                let ps = LExpression {
                    source: e,
                    span: *span,
                    data: LExpressionData::Tuple(self.lower_exprs(ps)?),
                }
                .intern(self.ctx);

                self.std_static_call(None, "Call", self.fresh_infer_tys(1), "call", vec![], vec![
                    ps,
                ])
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
            PExpressionData::While(l, p, t, e) => {
                self.enter_block();
                let i = self.declare_label(*l);
                let p = self.lower_expr(*p)?;
                let t = self.lower_expr(*t)?;
                let e = self.lower_expr(*e)?;
                self.exit_block();

                LExpressionData::While(i, p, t, e)
            },
            PExpressionData::For(l, p, es, t, e) => self.lower_expr_for(*l, *p, *es, *t, *e),
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
                        todo!("Die")
                    }

                    let g = self.lower_tys(g, true)?;
                    let g = self.check_generics_parity(*span, g, info.generics.len(), true)?;

                    let s = self.ctx.object_constructor(o)?;
                    let a = self.lower_constructor(&s, a)?;

                    LExpressionData::StructConstructor(o.into(), g, a)
                },
                LScopeItem::EnumVariant(e, v) => {
                    if !g.is_empty() {
                        todo!("Die")
                    }

                    let g = self.fresh_infer_tys(e.lookup(self.ctx).generics.len());

                    let s = self.ctx.enum_variant_constructor(e, v)?;
                    let a = self.lower_constructor(&s, a)?;

                    LExpressionData::EnumConstructor(e.into(), g, v, a)
                },
                _ => todo!("Die"),
            },
            PExpressionData::StructuralVariant(p, g, v, a) => match self.lookup_path(p)? {
                LScopeItem::Enum(e) => {
                    let g = self.lower_tys(g, true)?;
                    let g = self.check_generics_parity(
                        *span,
                        g,
                        e.lookup(self.ctx).generics.len(),
                        true,
                    )?;

                    let s = self.ctx.enum_variant_constructor(e, *v)?;
                    let a = self.lower_constructor(&s, a)?;

                    LExpressionData::EnumConstructor(e.into(), g, *v, a)
                },
                _ => todo!("Die"),
            },
            PExpressionData::Allocate(p, g, a) => match self.lookup_path(p)? {
                LScopeItem::Object(o) => {
                    let info = o.lookup(self.ctx);
                    if info.is_structural {
                        todo!("Die")
                    }
                    let g = self.lower_tys(g, true)?;
                    let g = self.check_generics_parity(*span, g, info.generics.len(), true)?;

                    let s = self.ctx.object_constructor(o)?;
                    let a = self.lower_constructor(&s, a)?;

                    LExpressionData::ObjectAllocation(o.into(), g, a)
                },
                _ => todo!("Die"),
            },
            PExpressionData::Return(v) =>
                if self.scopes.last_mut().unwrap().return_allowed {
                    LExpressionData::Return(self.lower_expr(*v)?)
                } else {
                    todo!("Die")
                },
            PExpressionData::Assert(v) => {
                if let LScopeItem::Function(assert) = self.lookup_std_item("assert") {
                    LExpressionData::Call(assert.into(), vec![], vec![self.lower_expr(*v)?])
                } else {
                    unreachable!()
                }
            },
            PExpressionData::Break(b, l) => {
                // TODO: This is a lot of work, it's kinda gross...
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
                let i = i
                    .lookup(self.ctx)
                    .parse()
                    .map_err(|e| AError::NotANumber("tuple index", *span, *i))?;

                LExpressionData::IndexAccess(self.lower_expr(*o)?, *span, i)
            },
            PExpressionData::Await(a) => {
                if !self.scopes.last().unwrap().await_allowed {
                    todo!("Die")
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

    fn lower_expr_for() -> AResult<LExpressionData> {
        todo!()
    }

    fn lower_expr_throw(
        &self,
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
        &self,
        shape: &LConstructorShape,
        args: &PConstructorArguments,
    ) -> AResult<Vec<(usize, Id<LExpression>)>> {
        match (shape, args) {
            (LConstructorShape::Empty(_), PConstructorArguments::Empty(_)) => Ok(vec![]),
            (LConstructorShape::Positional(s, n), PConstructorArguments::Positional(s2, es)) =>
                if es.len() == *n {
                    Ok(self.lower_exprs(es)?.into_iter().enumerate().collect())
                } else {
                    todo!("Die")
                },
            (LConstructorShape::Named(s, expected), PConstructorArguments::Named(s2, given)) => {
                let seen = hashmap! {};
                let args = vec![];

                for (is, n, e) in given {
                    if let Some(old_is) = seen.insert(*n, *is) {
                        todo!("Die")
                    }
                    if let Some((pos, _)) = expected.get(n) {
                        args.push((*pos, self.lower_expr(*e)?));
                    } else {
                        todo!("Die, Unexpected arg n in s2")
                    }
                }

                for (n, (_, is)) in expected {
                    if !seen.contains_key(n) {
                        todo!("Die, missing arg n in s2")
                    }
                }

                Ok(args)
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

    fn lower_pattern(&mut self, p: Id<PPattern>) -> AResult<Id<LPattern>> {
        let PPattern { span, ty, data } = &*p.lookup(self.ctx);
        let ty = self.lower_ty(*ty, true)?;
        let data = match data {
            PPatternData::Underscore => LPatternData::Underscore,
            PPatternData::Literal(l) => LPatternData::Literal(*l),
            PPatternData::Identifier(v) => {
                let v = self.declare_variable(*v, *span, ty);
                LPatternData::Variable(v)
            },
            PPatternData::Tuple(ps) => LPatternData::Tuple(self.lower_patterns(ps)?),
            PPatternData::StructuralAmbiguous(p, g, a) => match self.lookup_path(p)? {
                LScopeItem::Object(o) => {
                    let info = o.lookup(self.ctx);
                    if !info.is_structural {
                        todo!("Die")
                    }

                    let g = self.lower_tys(g, true)?;
                    let g = self.check_generics_parity(*span, g, info.generics.len(), true)?;

                    let s = self.ctx.object_constructor(o)?;
                    let a = self.lower_destructor(&s, a)?;

                    LPatternData::StructPattern(o.into(), g, a)
                },
                LScopeItem::EnumVariant(e, v) => {
                    if !g.is_empty() {
                        todo!("Die")
                    }

                    let g = self.fresh_infer_tys(e.lookup(self.ctx).generics.len());

                    let s = self.ctx.enum_variant_constructor(e, v)?;
                    let a = self.lower_destructor(&s, a)?;

                    LPatternData::EnumVariantPattern(e.into(), g, v, a)
                },
                _ => todo!("Die"),
            },
            PPatternData::StructuralVariant(p, g, v, a) => match self.lookup_path(p)? {
                LScopeItem::Enum(e) => {
                    let g = self.lower_tys(g, true)?;
                    let g = self.check_generics_parity(
                        *span,
                        g,
                        e.lookup(self.ctx).generics.len(),
                        true,
                    )?;

                    let s = self.ctx.enum_variant_constructor(e, *v)?;
                    let a = self.lower_destructor(&s, a)?;

                    LPatternData::EnumVariantPattern(e.into(), g, *v, a)
                },
                _ => todo!("Die"),
            },
        };
        Ok(LPattern {
            source: Some(p),
            span: *span,
            ty,
            data,
        }
        .intern(self.ctx))
    }

    fn lower_destructor(
        &mut self,
        shape: &LConstructorShape,
        constructor: &PPatternConstructorArguments,
    ) -> AResult<Vec<Id<LPattern>>> {
        match (shape, constructor) {
            (LConstructorShape::Empty(_), PPatternConstructorArguments::Empty(_)) => Ok(vec![]),
            (
                LConstructorShape::Positional(s, n),
                PPatternConstructorArguments::Positional(s2, ps, ignore),
            ) => {
                if (!ignore && *n < ps.len()) || (*n > ps.len()) {
                    todo!("Die")
                }

                let ps = self.lower_patterns(ps)?;
                // Extend with any missing members
                ps.extend((ps.len()..*n).map(|_| self.fresh_empty_pattern(*s2)));

                Ok(ps)
            },
            (
                LConstructorShape::Named(s, expected),
                PPatternConstructorArguments::Named(s2, given, ignore),
            ) => {
                let seen = hashmap! {};
                let args = vec![];

                for (is, n, p) in given {
                    if let Some(old_is) = seen.insert(*n, *is) {
                        todo!("Die")
                    }
                    if let Some((pos, _)) = expected.get(n) {
                        args[*pos] = Some(self.lower_pattern(*p)?);
                    } else {
                        todo!("Die, Unexpected arg n in s2")
                    }
                }

                if !ignore {
                    for (n, (_, is)) in expected {
                        if !seen.contains_key(n) {
                            todo!("Die, missing arg n in s2")
                        }
                    }
                }

                // Unwrap each None hole into an `_` pattern
                Ok(args
                    .into_iter()
                    .map(|p| p.unwrap_or_else(|| self.fresh_empty_pattern(*s2)))
                    .collect())
            },
            _ => todo!("Die"),
        }
    }

    fn fresh_empty_pattern(&self, span: Span) -> Id<LPattern> {
        LPattern {
            span,
            source: None,
            ty: self.fresh_infer_ty(),
            data: LPatternData::Underscore,
        }
        .intern(self.ctx)
    }

    fn lower_patterns(&mut self, ps: &[Id<PPattern>]) -> AResult<Vec<Id<LPattern>>> {
        let mut ret = vec![];

        for p in ps {
            ret.push(self.lower_pattern(*p)?);
        }

        Ok(ret)
    }

    fn declare_variable(&mut self, name: Id<str>, span: Span, ty: Id<LType>) -> LVariable {
        let var = LVariable {
            id: fresh_id(),
            name,
            span,
            ty,
        };

        self.scopes
            .last_mut()
            .unwrap()
            .variable_scopes
            .last_mut()
            .unwrap()
            .insert(name, var);

        var
    }

    fn declare_label(&mut self, label: Option<Id<str>>) -> usize {
        let label = label.unwrap_or_else(|| fresh_name("LABEL").intern(self.ctx));
        let id = fresh_id();

        self.scopes
            .last_mut()
            .unwrap()
            .label_scopes
            .push((label, id));
        id
    }

    fn lookup_label(&mut self, s: Span, l: Option<Id<str>>) -> AResult<usize> {
        for (n, i) in self.scopes.last_mut().unwrap().label_scopes.iter().rev() {
            if l.map_or(true, |l| l == *n) {
                return Ok(*i);
            }
        }

        todo!("Die")
    }

    fn check_generics_parity(
        &self,
        span: Span,
        generics: Vec<Id<LType>>,
        num: usize,
        infer_allowed: bool,
    ) -> AResult<Vec<Id<LType>>> {
        if generics.len() == num {
            Ok(generics)
        } else if generics.is_empty() && infer_allowed {
            Ok((0..num).map(|_| self.fresh_infer_ty()).collect())
        } else {
            Err(AError::IncorrectGenerics(span, generics.len(), num))
        }
    }

    fn get_range_bound(
        &self,
        span: Span,
        source: Id<PExpression>,
        kind: &'static str,
        es: Vec<Id<LExpression>>,
    ) -> Id<LExpression> {
        if let LScopeItem::Enum(bound) = self.lookup_std_item("Bound") {
            LExpression {
                source,
                span,
                data: LExpressionData::EnumConstructor(
                    bound.into(),
                    self.fresh_infer_tys(1),
                    self.ctx.static_name(kind),
                    es.into_iter().enumerate().collect(),
                ),
            }
            .intern(self.ctx)
        } else {
            unreachable!()
        }
    }

    fn fresh_infer_ty(&self) -> Id<LType> {
        fresh_infer_ty().intern(self.ctx)
    }

    fn fresh_infer_tys(&self, n: usize) -> Vec<Id<LType>> {
        (0..n).map(|_| fresh_infer_ty().intern(self.ctx)).collect()
    }
}

enum LValueKind {
    LValue,
    ArrayAccess(Id<LExpression>, Id<LExpression>),
    NotAnLValue,
}

impl LValueKind {
    fn is_lvalue(self) -> bool {
        matches!(self, LValueKind::LValue | LValueKind::ArrayAccess(..))
    }
}

struct FunctionScope {
    return_allowed: bool,
    await_allowed: bool,
    variable_scopes: Vec<HashMap<Id<str>, LVariable>>,
    label_scopes: Vec<(Id<str>, usize)>,
    vcx: LVariableContext,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
struct LVariableContext {
    variables: BTreeMap<usize, LVariable>,
    captures: BTreeMap<usize, LVariable>,
}

impl FunctionScope {
    fn new(return_allowed: bool, await_allowed: bool) -> Self {
        FunctionScope {
            return_allowed,
            await_allowed,
            variable_scopes: vec![],
            label_scopes: vec![],
            vcx: LVariableContext {
                variables: btreemap! {},
                captures: btreemap! {},
            },
        }
    }

    fn get(&self, name: Id<str>) -> Option<LVariable> {
        for s in self.variable_scopes.iter().rev() {
            if let Some(item) = s.get(&name) {
                return Some(*item);
            }
        }

        None
    }

    fn capture(&mut self, item: LVariable) -> LVariable {
        let fresh = fresh_id();

        self.vcx.captures.insert(fresh, item);

        let item = LVariable {
            id: fresh,
            name: item.name,
            ty: item.ty,
            span: item.span,
        };

        self.vcx.variables.insert(fresh, item);

        item
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LVariable {
    id: usize,
    name: Id<str>,
    ty: Id<LType>,
    span: Span,
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub enum LType {
    Infer(usize),
    Int,
    Float,
    Char,
    Bool,
    String,
    SelfType,
    Array(Id<LType>),
    Tuple(Vec<Id<LType>>),
    Closure(Vec<Id<LType>>, Id<LType>),
    FnPtr(Vec<Id<LType>>, Id<LType>),
    Dynamic(Vec<Id<LTraitType>>),
    Object(LId<LObject>, Vec<Id<LType>>),
    Enum(LId<LEnum>, Vec<Id<LType>>),
    Associated(Id<LType>, Option<Id<LTraitType>>, Id<str>),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LTraitType {
    tr: LId<LTrait>,
    generics: Vec<Id<LType>>,
    bounds: BTreeMap<Id<str>, Id<LType>>,
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LExpression {
    #[plain]
    source: Id<PExpression>,
    span: Span,
    data: LExpressionData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
enum LExpressionData {
    Unimplemented,
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
    While(usize, Id<LExpression>, Id<LExpression>, Id<LExpression>),
    Match(Id<LExpression>, Vec<(Id<LPattern>, Id<LExpression>)>),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LStatement {
    #[plain]
    source: Option<Id<PStatement>>,
    span: Span,
    data: LStatementData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
enum LStatementData {
    Expression(Id<LExpression>),
    Let(Id<LPattern>, Id<LExpression>),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LPattern {
    #[plain]
    source: Option<Id<PPattern>>,
    span: Span,
    ty: Id<LType>,
    data: LPatternData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
enum LPatternData {
    Underscore,
    Literal(PLiteral),
    Variable(LVariable),
    Tuple(Vec<Id<LPattern>>),
    EnumVariantPattern(LId<LEnum>, Vec<Id<LType>>, Id<str>, Vec<Id<LPattern>>),
    StructPattern(LId<LObject>, Vec<Id<LType>>, Vec<Id<LPattern>>),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LGlobal {
    #[plain]
    source: Id<PGlobal>,
    span: Span,
    name: Id<str>,
    ty: Id<LType>,
    expr: Id<LExpression>,
    vcx: LVariableContext,
}

impl LateLookup for LGlobal {
    type Source = PGlobal;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .globals
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LFunction {
    #[plain]
    source: Id<PFunction>,
}

impl LateLookup for LFunction {
    type Source = PFunction;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .functions
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LObject {
    #[plain]
    source: Id<PObject>,
}

impl LateLookup for LObject {
    type Source = PObject;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .objects
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LEnum {}

impl LateLookup for LEnum {
    type Source = PEnum;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .enums
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LTrait {}

impl LateLookup for LTrait {
    type Source = PTrait;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .traits
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LImpl {}

impl LateLookup for LImpl {
    type Source = PImpl;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .impls
            .get(&id)
            .unwrap()
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LScopeItem {
    Module(Id<PModule>),
    Global(Id<PGlobal>),
    Function(Id<PFunction>),
    Object(Id<PObject>),
    Enum(Id<PEnum>),
    EnumVariant(Id<PEnum>, Id<str>),
    Trait(Id<PTrait>),
    Variable(LVariable),
}

impl LScopeItem {
    pub fn info(&self, ctx: &dyn AdelaideContext) -> (&'static str, Id<str>, Span) {
        match self {
            LScopeItem::Module(e) => {
                let e = e.lookup(ctx);
                ("module", e.name, e.span)
            },
            LScopeItem::Global(e) => {
                let e = e.lookup(ctx);
                ("global variable", e.name, e.span)
            },
            LScopeItem::Function(e) => {
                let e = e.lookup(ctx);
                ("function", e.name, e.span)
            },
            LScopeItem::Object(e) => {
                let e = e.lookup(ctx);
                (
                    if e.is_structural { "struct" } else { "object" },
                    e.name,
                    e.span,
                )
            },
            LScopeItem::Enum(e) => {
                let e = e.lookup(ctx);
                ("enum", e.name, e.span)
            },
            LScopeItem::EnumVariant(e, v) => {
                let e = e.lookup(ctx);

                for (span, name, _) in &e.variants {
                    if name == v {
                        let full_name =
                            format!("{}::{}", e.name.lookup(ctx), v.lookup(ctx)).intern(ctx);
                        return ("enum variant", full_name, *span);
                    }
                }

                unreachable!("This enum variant should be guaranteed to exist")
            },
            LScopeItem::Trait(e) => {
                let e = e.lookup(ctx);
                ("trait", e.name, e.span)
            },
            LScopeItem::Variable(LVariable { name, span, .. }) => ("variable", *name, *span),
        }
    }
}

impl From<LUseItem> for LScopeItem {
    fn from(i: LUseItem) -> Self {
        match i {
            LUseItem::Module(m) => LScopeItem::Module(m),
            LUseItem::Global(g) => LScopeItem::Global(g),
            LUseItem::Function(f) => LScopeItem::Function(f),
            LUseItem::Object(o) => LScopeItem::Object(o),
            LUseItem::Enum(e) => LScopeItem::Enum(e),
            LUseItem::EnumVariant(e, v) => LScopeItem::EnumVariant(e, v),
            LUseItem::Trait(t) => LScopeItem::Trait(t),
            LUseItem::Imported { .. } => unreachable!(),
        }
    }
}

pub fn get_bound_names(
    ctx: &dyn AdelaideContext,
    tr: Id<PTrait>,
) -> AResult<Arc<HashMap<Id<str>, Span>>> {
    let mut bounds = hashmap! {};

    for i in &tr.lookup(ctx).members {
        match i {
            PTraitMember::Type(span, name, _) => {
                if let Some(old_span) = bounds.get(name) {
                    todo!("Die")
                }

                bounds.insert(*name, *span);
            },
            _ => { /* Ignore */ },
        }
    }

    Ok(Arc::new(bounds))
}

#[derive(Debug, Eq, PartialEq)]
pub enum LConstructorShape {
    Empty(Span),
    Positional(Span, usize),
    Named(Span, HashMap<Id<str>, (usize, Span)>),
}

pub fn object_constructor(
    ctx: &dyn AdelaideContext,
    o: Id<PObject>,
) -> AResult<Arc<LConstructorShape>> {
    let data = match &o.lookup(ctx).members {
        crate::parser::PObjectMembers::Empty(s) => LConstructorShape::Empty(*s),
        crate::parser::PObjectMembers::Positional(s, n) =>
            LConstructorShape::Positional(*s, n.len()),
        crate::parser::PObjectMembers::Named(s, es) => {
            let mut members = hashmap! {};

            for (i, (s, n, _)) in es.iter().enumerate() {
                if let Some(old_s) = members.insert(*n, (i, *s)) {
                    todo!("Die")
                }
            }

            LConstructorShape::Named(*s, members)
        },
    };
    Ok(Arc::new(data))
}

pub fn enum_variant_constructor(
    ctx: &dyn AdelaideContext,
    e: Id<PEnum>,
    v: Id<str>,
) -> AResult<Arc<LConstructorShape>> {
    for (span, variant, members) in &e.lookup(ctx).variants {
        if *variant == v {
            let data = match members {
                crate::parser::PObjectMembers::Empty(s) => LConstructorShape::Empty(*s),
                crate::parser::PObjectMembers::Positional(s, n) =>
                    LConstructorShape::Positional(*s, n.len()),
                crate::parser::PObjectMembers::Named(s, es) => {
                    let mut members = hashmap! {};

                    for (i, (s, n, _)) in es.iter().enumerate() {
                        if let Some(old_s) = members.insert(*n, (i, *s)) {
                            todo!("Die")
                        }
                    }

                    LConstructorShape::Named(*s, members)
                },
            };

            return Ok(Arc::new(data));
        }
    }
    todo!("Die, no variant")
}
