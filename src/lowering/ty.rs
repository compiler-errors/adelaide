use std::collections::BTreeMap;

use crate::{
    lexer::Span,
    parser::{PTraitType, PTraitTypeWithBindings, PType, PTypeData},
    util::{AError, AResult, Id, Intern, LId},
};

use super::{fresh_id, InferId, LEnum, LGeneric, LObject, LScopeItem, LTrait, LoweringContext};

#[derive(Debug, Hash, Eq, PartialEq, Clone, Lookup, PrettyPrint)]
pub struct LType {
    pub span: Span,
    pub data: LTypeData,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, PrettyPrint)]
pub enum LTypeData {
    Infer(InferId),
    Int,
    Float,
    Char,
    Bool,
    String,
    Never,
    SelfSkolem(LGeneric),
    Generic(LGeneric),
    Array(Id<LType>),
    Tuple(Vec<Id<LType>>),
    Closure(Vec<Id<LType>>, Id<LType>),
    FnPtr(Vec<Id<LType>>, Id<LType>),
    Dynamic(Id<LTraitTypeWithBindings>),
    Object(LId<LObject>, Vec<Id<LType>>),
    Enum(LId<LEnum>, Vec<Id<LType>>),
    Associated(Id<LType>, Option<Id<LTraitType>>, Id<str>),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LTraitType {
    pub span: Span,
    pub tr: LId<LTrait>,
    pub generics: Vec<Id<LType>>,
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LTraitTypeWithBindings {
    pub span: Span,
    pub tr: LId<LTrait>,
    pub generics: Vec<Id<LType>>,
    pub bindings: BTreeMap<Id<str>, Id<LType>>,
}

impl LoweringContext<'_> {
    pub fn lower_ty(
        &mut self,
        t: Id<PType>,
        infer_allowed: bool,
        assoc_allowed: bool,
    ) -> AResult<Id<LType>> {
        let PType { data, span } = &*t.lookup(self.ctx);

        let data = match data {
            PTypeData::Infer =>
                if infer_allowed {
                    LTypeData::Infer(fresh_id())
                } else {
                    return Err(AError::IllegalInfer { span: *span });
                },
            PTypeData::AsyncBlock(a) => {
                if let LScopeItem::Object(awaitable) = self.ctx.std_item("AsyncBlock") {
                    LTypeData::Object(awaitable.into(), vec![self.lower_ty(
                        *a,
                        infer_allowed,
                        assoc_allowed,
                    )?])
                } else {
                    unreachable!()
                }
            },
            PTypeData::AmbiguousPath(p, g) => {
                let generics = self.lower_tys(g, infer_allowed, assoc_allowed)?;

                match self.lookup_path(&*p)? {
                    LScopeItem::Enum(e) => {
                        let info = e.lookup(self.ctx);
                        let generics = self.check_generics_parity(
                            generics,
                            *span,
                            info.generics.len(),
                            info.span,
                            infer_allowed,
                        )?;

                        LTypeData::Enum(e.into(), generics)
                    },
                    LScopeItem::Object(o) => {
                        let info = o.lookup(self.ctx);
                        let generics = self.check_generics_parity(
                            generics,
                            *span,
                            info.generics.len(),
                            info.span,
                            infer_allowed,
                        )?;

                        LTypeData::Object(o.into(), generics)
                    },
                    LScopeItem::Generic(g) => {
                        if !generics.is_empty() {
                            return Err(AError::DenyGenerics {
                                kind: "generic type",
                                name: g.name,
                                use_span: *span,
                                def_span: g.span,
                            });
                        }

                        LTypeData::Generic(g)
                    },
                    i => {
                        let (kind, name, def_span) = i.info(self.ctx);
                        return Err(AError::ItemIsNotAType {
                            kind,
                            name,
                            def_span,
                            use_span: *span,
                        });
                    },
                }
            },
            PTypeData::Associated(ty, m) =>
                if assoc_allowed {
                    let (ty, trt) = self.lower_elaborated_ty(*ty, infer_allowed, assoc_allowed)?;
                    LTypeData::Associated(ty, trt, *m)
                } else {
                    return Err(AError::IllegalAssoc { span: *span });
                },
            PTypeData::Closure(es, r) => LTypeData::Closure(
                self.lower_tys(es, infer_allowed, assoc_allowed)?,
                self.lower_ty(*r, infer_allowed, assoc_allowed)?,
            ),
            PTypeData::FnPtr(es, r) => LTypeData::FnPtr(
                self.lower_tys(es, infer_allowed, assoc_allowed)?,
                self.lower_ty(*r, infer_allowed, assoc_allowed)?,
            ),
            PTypeData::Elaborated(..) => {
                return Err(AError::IllegalElaboration { span: *span });
            },
            PTypeData::Int => LTypeData::Int,
            PTypeData::Float => LTypeData::Float,
            PTypeData::Char => LTypeData::Char,
            PTypeData::Bool => LTypeData::Bool,
            PTypeData::String => LTypeData::String,
            PTypeData::SelfType =>
                if let Some(self_ty) = self.self_ty {
                    return Ok(self_ty);
                } else {
                    return Err(AError::IllegalSelf { span: *span });
                },
            PTypeData::Never => LTypeData::Never,
            PTypeData::Array(e) =>
                LTypeData::Array(self.lower_ty(*e, infer_allowed, assoc_allowed)?),
            PTypeData::Tuple(es) =>
                LTypeData::Tuple(self.lower_tys(es, infer_allowed, assoc_allowed)?),
            PTypeData::Dynamic(t) => {
                let lowered =
                    self.lower_trait_ty_with_bindings(*t, infer_allowed, assoc_allowed)?;

                let info = lowered.lookup(self.ctx);
                let parent = info.tr.source();

                for (bound, def_span) in &*self.ctx.get_bound_names(parent)? {
                    if !info.bindings.contains_key(bound) {
                        return Err(AError::MissingTraitBound {
                            trait_name: parent.lookup(self.ctx).name,
                            bound: *bound,
                            use_span: *span,
                            def_span: *def_span,
                        });
                    }
                }

                LTypeData::Dynamic(lowered)
            },
        };

        Ok(LType { data, span: *span }.intern(self.ctx))
    }

    pub fn lower_tys(
        &mut self,
        ts: &[Id<PType>],
        infer_allowed: bool,
        assoc_allowed: bool,
    ) -> AResult<Vec<Id<LType>>> {
        let mut ret = vec![];

        for t in ts {
            ret.push(self.lower_ty(*t, infer_allowed, assoc_allowed)?);
        }

        Ok(ret)
    }

    pub fn lower_elaborated_ty(
        &mut self,
        t: Id<PType>,
        infer_allowed: bool,
        assoc_allowed: bool,
    ) -> AResult<(Id<LType>, Option<Id<LTraitType>>)> {
        match &*t.lookup(self.ctx) {
            PType {
                data: PTypeData::Elaborated(t, trt),
                ..
            } => Ok((
                self.lower_ty(*t, infer_allowed, assoc_allowed)?,
                Some(self.lower_trait_ty(*trt, infer_allowed, assoc_allowed)?),
            )),
            // Otherwise, we did not extract an elaborated trait...
            _ => Ok((self.lower_ty(t, infer_allowed, assoc_allowed)?, None)),
        }
    }

    pub fn lower_trait_ty(
        &mut self,
        t: Id<PTraitType>,
        infer_allowed: bool,
        assoc_allowed: bool,
    ) -> AResult<Id<LTraitType>> {
        let PTraitType {
            span,
            path,
            generics,
        } = &*t.lookup(self.ctx);

        let tr = match self.lookup_path(path)? {
            LScopeItem::Trait(t) => t,
            i => {
                let (kind, name, def_span) = i.info(self.ctx);
                return Err(AError::ItemIsNotATrait {
                    kind,
                    name,
                    def_span,
                    use_span: *span,
                });
            },
        };

        let info = tr.lookup(self.ctx);

        let generics = self.lower_tys(generics, infer_allowed, assoc_allowed)?;
        let generics = self.check_generics_parity(
            generics,
            *span,
            info.generics.len(),
            info.span,
            infer_allowed,
        )?;

        Ok(LTraitType {
            span: *span,
            tr: tr.into(),
            generics,
        }
        .intern(self.ctx))
    }

    pub fn lower_trait_ty_with_bindings(
        &mut self,
        t: Id<PTraitTypeWithBindings>,
        infer_allowed: bool,
        assoc_allowed: bool,
    ) -> AResult<Id<LTraitTypeWithBindings>> {
        match &*t.lookup(self.ctx) {
            PTraitTypeWithBindings::Plain {
                span,
                path,
                generics,
                bindings,
            } => {
                let tr = match self.lookup_path(path)? {
                    LScopeItem::Trait(t) => t,
                    i => {
                        let (kind, name, def_span) = i.info(self.ctx);
                        return Err(AError::ItemIsNotATrait {
                            kind,
                            name,
                            def_span,
                            use_span: *span,
                        });
                    },
                };

                let info = tr.lookup(self.ctx);

                let generics = self.lower_tys(generics, infer_allowed, assoc_allowed)?;
                let generics = self.check_generics_parity(
                    generics,
                    *span,
                    info.generics.len(),
                    info.span,
                    infer_allowed,
                )?;

                let trait_bindings = self.ctx.get_bound_names(tr)?;
                let mut lowered_at = hashmap! {};
                let mut lowered_bindings = btreemap! {};

                for (span, name, ty) in bindings {
                    if !trait_bindings.contains_key(name) {
                        let tr = tr.lookup(self.ctx);
                        return Err(AError::MissingSubItem {
                            parent_kind: "trait",
                            parent_name: tr.name,
                            parent_span: tr.span,
                            child_kind: "associated type",
                            child_name: *name,
                            use_span: *span,
                        });
                    }

                    if let Some(old_span) = lowered_at.get(name) {
                        return Err(AError::DuplicatedTraitBound {
                            name: *name,
                            span: *old_span,
                            span2: *span,
                        });
                    }

                    lowered_at.insert(*name, *span);
                    lowered_bindings
                        .insert(*name, self.lower_ty(*ty, infer_allowed, assoc_allowed)?);
                }

                Ok(LTraitTypeWithBindings {
                    span: *span,
                    tr: tr.into(),
                    generics,
                    bindings: lowered_bindings,
                }
                .intern(self.ctx))
            },
            PTraitTypeWithBindings::Function { span, params, ret } => {
                let tr = if let LScopeItem::Trait(tr) = self.ctx.std_item("Call") {
                    tr
                } else {
                    unreachable!()
                };

                let generics = vec![LType {
                    data: LTypeData::Tuple(self.lower_tys(params, infer_allowed, assoc_allowed)?),
                    span: *span,
                }
                .intern(self.ctx)];

                let bindings = btreemap! { self.ctx.static_name("Return") => self.lower_ty(*ret, infer_allowed, assoc_allowed)? };

                Ok(LTraitTypeWithBindings {
                    span: *span,
                    tr: tr.into(),
                    generics,
                    bindings,
                }
                .intern(self.ctx))
            },
        }
    }

    pub fn lower_trait_tys_with_bindings(
        &mut self,
        ts: &[Id<PTraitTypeWithBindings>],
        infer_allowed: bool,
        assoc_allowed: bool,
    ) -> AResult<Vec<Id<LTraitTypeWithBindings>>> {
        let mut ret = vec![];

        for t in ts {
            ret.push(self.lower_trait_ty_with_bindings(*t, infer_allowed, assoc_allowed)?);
        }

        Ok(ret)
    }
}
