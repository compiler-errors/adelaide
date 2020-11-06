mod lid;
mod uses;

use std::{
    collections::BTreeMap,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    parser::{
        PEnum, PExpression, PFunction, PGlobal, PImpl, PModule, PObject, PTrait, PTraitType, PType,
        PTypeData,
    },
    util::{AError, AResult, Id, Intern},
};

pub use uses::{
    check_mod, early_lookup_ctx, local_mod_items, lookup_item, lookup_item_early, lower_mod_base,
    mod_items, LEarlyContext, LUseError, LUseItem, LUseResult,
};

use self::lid::{LId, LateLookup};

static INFER_IDS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LModule {
    #[plain]
    source: Id<PModule>,
    globals: BTreeMap<Id<PGlobal>, Id<LGlobal>>,
    functions: BTreeMap<Id<PFunction>, Id<LFunction>>,
    objects: BTreeMap<Id<PObject>, Id<LObject>>,
    enums: BTreeMap<Id<PEnum>, Id<LEnum>>,
    traits: BTreeMap<Id<PTrait>, Id<LTrait>>,
    impls: BTreeMap<Id<PImpl>, Id<LImpl>>,
}

pub fn lower_mod(ctx: &dyn AdelaideContext, key: Id<PModule>) -> AResult<Id<LModule>> {
    let info = key.lookup(ctx);

    let mut lcx = LoweringContext::try_new(ctx, key)?;

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
            LScopeItem::Module(m) => todo!(),
            LScopeItem::Function(f) => todo!(),
            LScopeItem::Object(o) => todo!(),
            LScopeItem::Enum(e) => todo!(),
            LScopeItem::Trait(_) => todo!(),
            LScopeItem::EnumVariant(e, v) => todo!(),
        }
    }

    Ok(LModule {
        source: key,
        globals,
        functions,
        objects,
        enums,
        traits,
        impls,
    }
    .intern(ctx))
}

struct LoweringContext<'ctx> {
    ctx: &'ctx dyn AdelaideContext,
    parent: Id<PModule>,
}

impl<'ctx> LoweringContext<'ctx> {
    fn try_new(
        ctx: &'ctx dyn AdelaideContext,
        parent: Id<PModule>,
    ) -> AResult<LoweringContext<'ctx>> {
        Ok(LoweringContext { ctx, parent })
    }

    fn lookup_std_item(&self, name: &str) -> LScopeItem {
        // We expect none of these results to be an error, so just unwrap them.
        *self.ctx
            .mod_items(self.ctx.parsed_root().unwrap())
            .unwrap()
            .get(&name.intern(self.ctx))
            .unwrap()
    }

    fn lookup_scoped_item(&self, at: Span, item: Id<str>) -> AResult<LScopeItem> {
        todo!()
    }

    fn lookup_path(&self, path: &[(Span, Id<str>)]) -> AResult<LScopeItem> {
        match self.lookup_path_partial(path)? {
            (i, &[]) => Ok(i),
            (i, &[(s, _), ..]) => Err(AError::ScopeItemNotAModule(i, s)),
        }
    }

    fn lookup_path_partial<'p>(
        &self,
        path: &'p [(Span, Id<str>)],
    ) -> AResult<(LScopeItem, &'p [(Span, Id<str>)])> {
        assert!(!path.is_empty());
        let ((span, name), path) = path.split_first().unwrap();

        let mut item = self.lookup_scoped_item(*span, *name)?;

        for (i, (span, name)) in path.iter().enumerate() {
            match item {
                LScopeItem::Module(m) => {
                    item = self.ctx.lookup_item(*span, m, *name)?;
                },
                LScopeItem::Enum(e) => {
                    let info = e.lookup(self.ctx);

                    if info.variants.iter().any(|(_, n, _)| n == name) {
                        return Ok((LScopeItem::EnumVariant(e, *name), &path[i + 1..]));
                    } else {
                        return Err(AError::EnumMissingVariant(*span, e, *name));
                    }
                },
                item => {
                    return Ok((item, &path[i + 1..]));
                },
            }
        }

        Ok((item, &[]))
    }

    fn lower_ty(&mut self, t: Id<PType>, infer_allowed: bool) -> AResult<Id<LType>> {
        let PType { span, data } = &*t.lookup(self.ctx);

        let data = match data {
            PTypeData::Infer =>
                if infer_allowed {
                    LType::Infer(INFER_IDS.fetch_add(1, Ordering::Relaxed))
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
            PTypeData::AmbiguousPath(p, g) => match self.lookup_path(&*p)? {
                LScopeItem::Enum(e) => LType::Enum(e.into(), self.lower_tys(g, infer_allowed)?),
                LScopeItem::Object(o) => LType::Object(o.into(), self.lower_tys(g, infer_allowed)?),
                otherwise => {
                    todo!("Die, not an object or enum or struct");
                },
            },
            PTypeData::Elaborated(_, _) => todo!("Die"),
            PTypeData::Associated(ty, m) => {
                let (ty, trt) = self.lower_elaborated_ty(*ty, infer_allowed)?;
                LType::Associated(ty, trt, *m)
            },
            PTypeData::Int => LType::Int,
            PTypeData::Float => LType::Float,
            PTypeData::Char => LType::Char,
            PTypeData::Bool => LType::Bool,
            PTypeData::String => LType::String,
            PTypeData::SelfType => LType::SelfType,
            PTypeData::Array(e) => LType::Array(self.lower_ty(*e, infer_allowed)?),
            PTypeData::Tuple(es) => LType::Tuple(self.lower_tys(es, infer_allowed)?),
            PTypeData::Closure(es, r) => LType::Closure(
                self.lower_tys(es, infer_allowed)?,
                self.lower_ty(*r, infer_allowed)?,
            ),
            PTypeData::FnPtr(es, r) => LType::FnPtr(
                self.lower_tys(es, infer_allowed)?,
                self.lower_ty(*r, infer_allowed)?,
            ),
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
                // TODO:

                let lowered_bounds = btreemap! {};
                for (name, ty) in bounds {
                    // TODO: Verify that all trait members are actually in the
                    // given trait. We can do that with tr!
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
                let bounds =
                    btreemap! { "Ret".intern(self.ctx) => self.lower_ty(*ret, infer_allowed)? };

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

        let ty = self.lower_ty(*ty, false)?;
        let expr = self.lower_expr(*expr)?;

        Ok(LGlobal {
            source: g,
            span: *span,
            name: *name,
            ty,
            expr,
        }
        .intern(self.ctx))
    }

    fn lower_expr(&mut self, e: Id<PExpression>) -> AResult<Id<LExpression>> {
        todo!()
    }
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
pub struct LExpression {}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LGlobal {
    #[plain]
    source: Id<PGlobal>,
    span: Span,
    name: Id<str>,
    ty: Id<LType>,
    expr: Id<LExpression>,
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LFunction {
    #[plain]
    source: Id<PFunction>,
}

impl LateLookup for LFunction {
    type Source = PFunction;

    fn late_lookup(id: &LId<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.0.lookup(ctx);
        *ctx.lower_mod(source.parent.get())
            .unwrap()
            .lookup(ctx)
            .functions
            .get(&id.0)
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

    fn late_lookup(id: &LId<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.0.lookup(ctx);
        *ctx.lower_mod(source.parent.get())
            .unwrap()
            .lookup(ctx)
            .objects
            .get(&id.0)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LEnum {}

impl LateLookup for LEnum {
    type Source = PEnum;

    fn late_lookup(id: &LId<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.0.lookup(ctx);
        *ctx.lower_mod(source.parent.get())
            .unwrap()
            .lookup(ctx)
            .enums
            .get(&id.0)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LTrait {}

impl LateLookup for LTrait {
    type Source = PTrait;

    fn late_lookup(id: &LId<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.0.lookup(ctx);
        *ctx.lower_mod(source.parent.get())
            .unwrap()
            .lookup(ctx)
            .traits
            .get(&id.0)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LImpl {}

impl LateLookup for LImpl {
    type Source = PImpl;

    fn late_lookup(id: &LId<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.0.lookup(ctx);
        *ctx.lower_mod(source.parent.get())
            .unwrap()
            .lookup(ctx)
            .impls
            .get(&id.0)
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
