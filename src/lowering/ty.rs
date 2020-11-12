use std::collections::BTreeMap;

use crate::{
    parser::{PTraitType, PType, PTypeData},
    util::{AError, AResult, Id, Intern, LId},
};

use super::{fresh_infer_ty, LEnum, LGeneric, LObject, LScopeItem, LTrait, LoweringContext};

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
    Generic(LGeneric),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LTraitType {
    pub tr: LId<LTrait>,
    pub generics: Vec<Id<LType>>,
    pub bounds: BTreeMap<Id<str>, Id<LType>>,
}

impl LoweringContext<'_> {
    pub fn lower_ty(&mut self, t: Id<PType>, infer_allowed: bool) -> AResult<Id<LType>> {
        let PType { data, span } = &*t.lookup(self.ctx);

        let data = match data {
            PTypeData::Infer =>
                if infer_allowed {
                    fresh_infer_ty()
                } else {
                    return Err(AError::IllegalInfer { span: *span });
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
                        let info = e.lookup(self.ctx);
                        let generics = self.check_generics_parity(
                            generics,
                            *span,
                            info.generics.len(),
                            info.span,
                            infer_allowed,
                        )?;

                        LType::Enum(e.into(), generics)
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

                        LType::Object(o.into(), generics)
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

                        LType::Generic(g)
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
            PTypeData::Elaborated(..) => {
                return Err(AError::IllegalElaboration { span: *span });
            },
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

    pub fn lower_tys(&mut self, ts: &[Id<PType>], infer_allowed: bool) -> AResult<Vec<Id<LType>>> {
        let mut ret = vec![];

        for t in ts {
            ret.push(self.lower_ty(*t, infer_allowed)?);
        }

        Ok(ret)
    }

    pub fn lower_elaborated_ty(
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

    pub fn lower_trait_ty(
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

                let generics = self.lower_tys(generics, infer_allowed)?;
                let generics = self.check_generics_parity(
                    generics,
                    *span,
                    info.generics.len(),
                    info.span,
                    infer_allowed,
                )?;

                let trait_bounds = self.ctx.get_bound_names(tr)?;
                let mut lowered_at = hashmap! {};
                let mut lowered_bounds = btreemap! {};

                for (span, name, ty) in bounds {
                    if !trait_bounds.contains_key(name) {
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
                    lowered_bounds.insert(*name, self.lower_ty(*ty, infer_allowed)?);
                }

                Ok(LTraitType {
                    tr: tr.into(),
                    generics,
                    bounds: lowered_bounds,
                }
                .intern(self.ctx))
            },
            PTraitType::Function {
                span: _,
                params,
                ret,
            } => {
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

    pub fn lower_trait_tys(
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
}
