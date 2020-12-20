use std::collections::{BTreeMap, HashSet};

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    lowering::{
        GenericId, InferId, LEnum, LGeneric, LImpl, LObject, LTrait, LTraitType,
        LTraitTypeWithBindings, LType, LTypeData,
    },
    util::{
        AError, AResult, Id, Intern, Pretty, PrettyPrint, TryCollectBTreeMap, TryCollectVec,
        ZipExact,
    },
};

use super::{item::TRestriction, Typechecker};

pub const UNIT_TYPE: &'static TType = &TType::Tuple(vec![]);

#[derive(Debug, Hash, Eq, PartialEq, Clone, Lookup)]
pub enum TType {
    Skolem(LGeneric),
    MethodSkolem(Id<LImpl>, LGeneric),
    GenericInfer(InferId),
    Infer(InferId),
    AssociatedSkolem(InferId, Id<TType>, Id<TTraitType>, Id<str>),
    Int,
    Float,
    Char,
    Bool,
    String,
    Never,
    Array(Id<TType>),
    Tuple(Vec<Id<TType>>),
    Closure(Vec<Id<TType>>, Id<TType>),
    FnPtr(Vec<Id<TType>>, Id<TType>),
    Dynamic(Id<TTraitTypeWithBindings>),
    Object(Id<LObject>, Vec<Id<TType>>),
    Enum(Id<LEnum>, Vec<Id<TType>>),
    Associated(Id<LType>, Id<TType>, Option<Id<TTraitType>>, Id<str>),
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Lookup)]
pub struct TTraitType(pub Id<LTrait>, pub Vec<Id<TType>>);

#[derive(Debug, Hash, Eq, PartialEq, Clone, Lookup)]
pub struct TTraitTypeWithBindings(pub Id<TTraitType>, pub BTreeMap<Id<str>, Id<TType>>);

#[derive(Copy, Clone, Debug, PrettyPrint)]
pub enum UnifyMode<'a> {
    Normal,
    GenericInference(&'a HashSet<InferId>),
    SkolemInference,
    Nothing,
}

impl<'a> UnifyMode<'a> {}

impl Typechecker<'_> {
    pub fn initialize_ty(
        &mut self,
        ty: Id<LType>,
        substitutions: &BTreeMap<GenericId, Id<TType>>,
    ) -> AResult<Id<TType>> {
        let LType { span, data } = &*ty.lookup(self.ctx);

        let lowered_ty = match data {
            LTypeData::Infer(id) => {
                self.infer_spans.insert(*id, *span);
                if let Some(ty) = self.get_infer(*id, *span)? {
                    debug!(
                        "Did we let {:?} escape? (subs = {:?})",
                        Pretty(ty, self.ctx),
                        Pretty(substitutions, self.ctx)
                    );
                    ty
                } else {
                    TType::Infer(*id).intern(self.ctx)
                }
            },
            LTypeData::Generic(g) | LTypeData::SelfSkolem(g) =>
                if let Some(ty) = substitutions.get(&g.id).copied() {
                    debug!(
                        "We DID replace {:?} => {:?} (subs = {:?})",
                        Pretty(g.id, self.ctx),
                        Pretty(ty, self.ctx),
                        Pretty(substitutions, self.ctx)
                    );
                    self.normalize_ty(ty, *span)?
                } else if let Some(global_substitutions) = &self.global_substitutions {
                    global_substitutions[&g.id]
                } else {
                    debug!(
                        "We didn't replace {:?} (subs = {:?})",
                        Pretty(g.id, self.ctx),
                        Pretty(substitutions, self.ctx)
                    );
                    TType::Skolem(*g).intern(self.ctx)
                },
            LTypeData::Int => self.ctx.static_ty(&TType::Int),
            LTypeData::Float => self.ctx.static_ty(&TType::Float),
            LTypeData::Char => self.ctx.static_ty(&TType::Char),
            LTypeData::Bool => self.ctx.static_ty(&TType::Bool),
            LTypeData::String => self.ctx.static_ty(&TType::String),
            LTypeData::Never => self.ctx.static_ty(&TType::Never),
            LTypeData::Array(e) =>
                TType::Array(self.initialize_ty(*e, substitutions)?).intern(self.ctx),
            LTypeData::Tuple(es) =>
                TType::Tuple(self.initialize_tys(&es, substitutions)?).intern(self.ctx),
            LTypeData::Closure(ps, r) => TType::Closure(
                self.initialize_tys(&ps, substitutions)?,
                self.initialize_ty(*r, substitutions)?,
            )
            .intern(self.ctx),
            LTypeData::FnPtr(ps, r) => TType::FnPtr(
                self.initialize_tys(&ps, substitutions)?,
                self.initialize_ty(*r, substitutions)?,
            )
            .intern(self.ctx),
            LTypeData::Dynamic(trait_ty) =>
                TType::Dynamic(self.initialize_trait_ty_with_bindings(*trait_ty, substitutions)?)
                    .intern(self.ctx),
            LTypeData::Object(o, gs) =>
                TType::Object(o.get(self.ctx), self.initialize_tys(&gs, substitutions)?)
                    .intern(self.ctx),
            LTypeData::Enum(e, gs) =>
                TType::Enum(e.get(self.ctx), self.initialize_tys(&gs, substitutions)?)
                    .intern(self.ctx),
            LTypeData::Associated(parent_ty, Some(trait_ty), name) => {
                let ty_span = parent_ty.lookup(self.ctx).span;
                let parent_ty = self.initialize_ty(*parent_ty, substitutions)?;
                let trait_ty_span = trait_ty.lookup(self.ctx).span;
                let trait_ty = self.initialize_trait_ty(*trait_ty, substitutions)?;

                if let Some(imp) =
                    self.do_goal_trait(parent_ty, ty_span, trait_ty, trait_ty_span, true)?
                {
                    self.instantiate_ty_from_impl(&imp, *name, *span)?
                } else {
                    TType::Associated(ty, parent_ty, Some(trait_ty), *name).intern(self.ctx)
                }
            },
            LTypeData::Associated(parent_ty, None, name) => {
                let parent_ty = self.initialize_ty(*parent_ty, substitutions)?;
                self.do_goal_associated_type(ty, parent_ty, *name, *span)?
            },
        };

        Ok(lowered_ty)
    }

    pub fn initialize_tys(
        &mut self,
        tys: &[Id<LType>],
        substitutions: &BTreeMap<GenericId, Id<TType>>,
    ) -> AResult<Vec<Id<TType>>> {
        tys.iter()
            .map(|ty| self.initialize_ty(*ty, substitutions))
            .try_collect_vec()
    }

    pub fn initialize_trait_ty(
        &mut self,
        trait_ty: Id<LTraitType>,
        substitutions: &BTreeMap<GenericId, Id<TType>>,
    ) -> AResult<Id<TTraitType>> {
        let LTraitType {
            tr,
            generics,
            span: _,
        } = &*trait_ty.lookup(self.ctx);

        Ok(TTraitType(
            tr.get(self.ctx),
            self.initialize_tys(&generics, substitutions)?,
        )
        .intern(self.ctx))
    }

    pub fn initialize_trait_ty_with_bindings(
        &mut self,
        trait_ty: Id<LTraitTypeWithBindings>,
        substitutions: &BTreeMap<GenericId, Id<TType>>,
    ) -> AResult<Id<TTraitTypeWithBindings>> {
        let LTraitTypeWithBindings {
            tr,
            generics,
            bindings,
            span: _,
        } = &*trait_ty.lookup(self.ctx);

        let bindings = bindings
            .iter()
            .map(|(n, ty)| -> AResult<_> { Ok((*n, self.initialize_ty(*ty, substitutions)?)) })
            .try_collect_btreemap()?;

        let trait_ty = TTraitType(
            tr.get(self.ctx),
            self.initialize_tys(&generics, substitutions)?,
        )
        .intern(self.ctx);

        Ok(TTraitTypeWithBindings(trait_ty, bindings).intern(self.ctx))
    }

    pub fn initialize_restrictions(
        &mut self,
        restrictions: &[(Id<LType>, Id<LTraitTypeWithBindings>)],
        substitutions: &BTreeMap<GenericId, Id<TType>>,
    ) -> AResult<Vec<TRestriction>> {
        debug!(
            "Instantiating restrictions given: {:?}",
            Pretty(substitutions, self.ctx)
        );

        restrictions
            .iter()
            .map(|(ty, trait_ty)| -> AResult<_> {
                let ty = self.initialize_ty(*ty, substitutions)?;

                let trait_ty = self.initialize_trait_ty_with_bindings(*trait_ty, substitutions)?;

                debug!(
                    "Instantiated {:?} requires {:?}",
                    Pretty(ty, self.ctx),
                    Pretty(&trait_ty, self.ctx)
                );

                // TODO: use better spans here lol
                Ok(TRestriction(ty, trait_ty))
            })
            .try_collect_vec()
    }

    pub fn unify_ty(
        &mut self,
        mode: UnifyMode,
        left_ty: Id<TType>,
        left_span: Span,
        right_ty: Id<TType>,
        right_span: Span,
    ) -> AResult<Id<TType>> {
        let left_ty = self.normalize_ty(left_ty, left_span)?;
        let right_ty = self.normalize_ty(right_ty, right_span)?;

        debug!(
            "{}Unifying {:?} and {:?} (mode = {:?})",
            "  ".repeat(self.epochs.len() - 1),
            Pretty(left_ty, self.ctx),
            Pretty(right_ty, self.ctx),
            Pretty(mode, self.ctx),
        );

        if left_ty == right_ty {
            return Ok(left_ty);
        }

        let ty = match (
            mode,
            &*left_ty.lookup(self.ctx),
            &*right_ty.lookup(self.ctx),
        ) {
            (UnifyMode::SkolemInference, TType::AssociatedSkolem(id, ..), _) => {
                let epoch = self.epoch();
                epoch.progress = true;
                epoch.infers.insert(*id, right_ty);
                right_ty
            },
            (UnifyMode::SkolemInference, _, TType::AssociatedSkolem(id, ..)) => {
                let epoch = self.epoch();
                epoch.progress = true;
               epoch.infers.insert(*id, left_ty);
                left_ty
            },

            (UnifyMode::GenericInference(allowed), TType::GenericInfer(id), _)
            //| (UnifyMode::Bounded(allowed, inferences), TType::SelfSkolem(g), _)
                if allowed.contains(id) =>
            {
                self.epoch().infers.insert(*id, right_ty);
                right_ty
            }
            (UnifyMode::GenericInference(allowed), _, TType::GenericInfer(id))
            //| (UnifyMode::Bounded(allowed, inferences), _, TType::SelfSkolem(g))
                if allowed.contains(id) =>
            {
                self.epoch().infers.insert(*id, left_ty);
                left_ty
            }

            (UnifyMode::Normal, TType::Infer(id), TType::Never) | (UnifyMode::GenericInference(_), TType::Infer(id), TType::Never) => {
                self.epoch().never_candidates.insert(*id);
                left_ty
            },
            (UnifyMode::Normal, TType::Never, TType::Infer(id)) | (UnifyMode::GenericInference(_), TType::Never, TType::Infer(id)) => {
                self.epoch().never_candidates.insert(*id);
                right_ty
            },

            (UnifyMode::Normal, TType::Infer(id), _) | (UnifyMode::GenericInference(_), TType::Infer(id), _) => {
                let epoch = self.epoch();
                epoch.progress = true;
                epoch.infers.insert(*id, right_ty);
                right_ty
            },
            (UnifyMode::Normal, _, TType::Infer(id)) | (UnifyMode::GenericInference(_), _, TType::Infer(id)) => {
                let epoch = self.epoch();
                epoch.progress = true;
                epoch.infers.insert(*id, left_ty);
                left_ty
            },

            (_, TType::Never, _) => right_ty,
            (_, _, TType::Never) => left_ty,

            (_, TType::Infer(_), _) | (_, TType::GenericInfer(_), _) | (_, TType::Associated(..), _) => {
                self.set_ambiguity(AError::CannotUnify {
                    left_ty,
                    left_span,
                    right_ty,
                    right_span,
                });
                right_ty
            },
            (_, _, TType::Infer(_)) | (_, _, TType::GenericInfer(_)) | (_, _, TType::Associated(..)) => {
                self.set_ambiguity(AError::CannotUnify {
                    left_ty,
                    left_span,
                    right_ty,
                    right_span,
                });
                left_ty
            },

            (_, TType::Array(left_ty), TType::Array(right_ty)) =>
                TType::Array(self.unify_ty(mode, *left_ty, left_span, *right_ty, right_span)?)
                    .intern(self.ctx),

            (_, TType::Tuple(left_tys), TType::Tuple(right_tys))
                if left_tys.len() == right_tys.len() =>
                TType::Tuple(self.unify_tys(mode, &left_tys, left_span, &right_tys, right_span)?)
                    .intern(self.ctx),

            (_, TType::Closure(left_params, left_ret), TType::Closure(right_params, right_ret))
                if left_params.len() == right_params.len() =>
                TType::Closure(
                    self.unify_tys(mode, &left_params, left_span, &right_params, right_span)?,
                    self.unify_ty(mode, *left_ret, left_span, *right_ret, right_span)?,
                )
                .intern(self.ctx),

            (_, TType::FnPtr(left_params, left_ret), TType::FnPtr(right_params, right_ret))
                if left_params.len() == right_params.len() =>
                TType::FnPtr(
                    self.unify_tys(mode, &left_params, left_span, &right_params, right_span)?,
                    self.unify_ty(mode, *left_ret, left_span, *right_ret, right_span)?,
                )
                .intern(self.ctx),

            (
                _,
                TType::Dynamic(left_trait_ty),
                TType::Dynamic(right_trait_ty),
            ) => {
                let TTraitTypeWithBindings(left_trait_ty, left_bindings) = &*left_trait_ty.lookup(self.ctx);
                let left_trait_ty = left_trait_ty.lookup(self.ctx);

                let TTraitTypeWithBindings(right_trait_ty, right_bindings) = &*right_trait_ty.lookup(self.ctx);
                let right_trait_ty = right_trait_ty.lookup(self.ctx);

                if left_trait_ty.0 == right_trait_ty.0 {
                    let generics = self.unify_tys(
                        mode,
                        &left_trait_ty.1,
                        left_span,
                        &right_trait_ty.1,
                        right_span,
                    )?;

                    let bindings = left_bindings
                        .keys()
                        .map(|n| -> AResult<_> {
                            Ok((
                                *n,
                                self.unify_ty(
                                    mode,
                                    left_bindings[n],
                                    left_span,
                                    right_bindings[n],
                                    right_span,
                                )?,
                            ))
                        })
                        .try_collect_btreemap()?;

                    TType::Dynamic(TTraitTypeWithBindings(
                        TTraitType(left_trait_ty.0, generics).intern(self.ctx),
                        bindings,
                    ).intern(self.ctx))
                    .intern(self.ctx)
                } else {
                    return Err(AError::CannotUnify {
                        left_ty,
                        left_span,
                        right_ty,
                        right_span,
                    });
                }
            },

            (
                _,
                TType::Object(left_obj, left_generics),
                TType::Object(right_obj, right_generics),
            ) if left_obj == right_obj => TType::Object(
                *left_obj,
                self.unify_tys(mode, &left_generics, left_span, &right_generics, right_span)?,
            )
            .intern(self.ctx),

            (
                _,
                TType::Enum(left_enum, left_generics), //
                TType::Enum(right_enum, right_generics),
            ) if left_enum == right_enum => TType::Enum(
                *left_enum,
                self.unify_tys(mode, &left_generics, left_span, &right_generics, right_span)?,
            )
            .intern(self.ctx),

            _ => {
                debug!("{}Cannot unify {:?} and {:?}", "  ".repeat(self.epochs.len() - 1), Pretty(left_ty, self.ctx), Pretty(right_ty, self.ctx));
                return Err(AError::CannotUnify {
                    left_ty,
                    left_span,
                    right_ty,
                    right_span,
                });
            },
        };

        Ok(ty)
    }

    pub fn unify_tys(
        &mut self,
        mode: UnifyMode,
        left_ty: &[Id<TType>],
        left_span: Span,
        right_ty: &[Id<TType>],
        right_span: Span,
    ) -> AResult<Vec<Id<TType>>> {
        left_ty
            .iter()
            .zip_exact(right_ty.iter())
            .map(|(left_ty, right_ty)| {
                self.unify_ty(mode, *left_ty, left_span, *right_ty, right_span)
            })
            .try_collect_vec()
    }

    pub fn unify_trait_ty(
        &mut self,
        mode: UnifyMode,
        left_trait_ty: Id<TTraitType>,
        left_span: Span,
        right_trait_ty: Id<TTraitType>,
        right_span: Span,
    ) -> AResult<Id<TTraitType>> {
        let TTraitType(left_tr, left_gs) = &*left_trait_ty.lookup(self.ctx);
        let TTraitType(right_tr, right_gs) = &*right_trait_ty.lookup(self.ctx);

        if left_tr == right_tr {
            Ok(TTraitType(
                *left_tr,
                self.unify_tys(mode, &left_gs, left_span, &right_gs, right_span)?,
            )
            .intern(self.ctx))
        } else {
            Err(AError::CannotUnifyTraits {
                left_trait_ty,
                left_span,
                right_trait_ty,
                right_span,
            })
        }
    }

    fn get_infer(&mut self, id: InferId, span: Span) -> AResult<Option<Id<TType>>> {
        for e in self.epochs.iter().rev() {
            if let Some(ty) = e.infers.get(&id).cloned() {
                return Ok(Some(self.normalize_ty(ty, span)?));
            }
        }

        Ok(None)
    }

    pub fn normalize_ty(&mut self, ty: Id<TType>, span: Span) -> AResult<Id<TType>> {
        let ty = match &*ty.lookup(self.ctx) {
            // All of these types are already concrete (as far as we care)
            TType::Int
            | TType::Float
            | TType::Char
            | TType::Bool
            | TType::String
            | TType::Never => ty,
            TType::Skolem(_) | TType::MethodSkolem(..) => {
                assert!(self.global_substitutions.is_none());
                ty
            },
            // Attempt to deref an infer (TODO: loop detection)
            TType::AssociatedSkolem(id, ..) =>
                if let Some(ty) = self.get_infer(*id, span)? {
                    ty
                } else {
                    assert!(self.global_substitutions.is_none());
                    ty
                },
            // Attempt to deref an infer (TODO: loop detection)
            TType::GenericInfer(id) | TType::Infer(id) =>
                if let Some(ty) = self.get_infer(*id, span)? {
                    ty
                } else {
                    ty
                },
            TType::Array(e) => TType::Array(self.normalize_ty(*e, span)?).intern(self.ctx),
            TType::Tuple(es) => TType::Tuple(self.normalize_tys(&es, span)?).intern(self.ctx),
            TType::Closure(ps, r) =>
                TType::Closure(self.normalize_tys(&ps, span)?, self.normalize_ty(*r, span)?)
                    .intern(self.ctx),
            TType::FnPtr(ps, r) =>
                TType::FnPtr(self.normalize_tys(&ps, span)?, self.normalize_ty(*r, span)?)
                    .intern(self.ctx),
            TType::Dynamic(trait_ty) => {
                let TTraitTypeWithBindings(trait_ty, bindings) = &*trait_ty.lookup(self.ctx);

                TType::Dynamic(
                    TTraitTypeWithBindings(
                        self.normalize_trait_ty(*trait_ty, span)?,
                        bindings
                            .iter()
                            .map(|(n, ty)| AResult::Ok((*n, self.normalize_ty(*ty, span)?)))
                            .try_collect_btreemap()?,
                    )
                    .intern(self.ctx),
                )
                .intern(self.ctx)
            },
            TType::Object(o, gs) =>
                TType::Object(*o, self.normalize_tys(&gs, span)?).intern(self.ctx),
            TType::Enum(e, gs) => TType::Enum(*e, self.normalize_tys(&gs, span)?).intern(self.ctx),

            // Attempt to actually solve the associated type
            TType::Associated(key, ty, Some(trait_ty), name) => {
                let ty = self.normalize_ty(*ty, span)?;
                let trait_ty = self.normalize_trait_ty(*trait_ty, span)?;

                if let Some(imp) = self.do_goal_trait(ty, span, trait_ty, span, true)? {
                    let ty = self.instantiate_ty_from_impl(&imp, *name, span)?;
                    self.normalize_ty(ty, span)?
                } else if !self.is_concrete_ty(ty)
                    || !self.is_concrete_trait_ty(trait_ty)
                    || self.type_facts.is_none()
                {
                    // If we have a non-concrete type, or our facts are not yet initialized, then
                    // woo.
                    TType::Associated(*key, ty, Some(trait_ty), *name).intern(self.ctx)
                } else {
                    return Err(AError::NoSolution {
                        ty,
                        trait_ty,
                        ty_span: span,
                        trait_ty_span: span,
                    });
                }
            },
            TType::Associated(key, ty, None, name) =>
                self.do_goal_associated_type(*key, *ty, *name, span)?,
        };

        Ok(ty)
    }

    pub fn normalize_tys(&mut self, tys: &[Id<TType>], span: Span) -> AResult<Vec<Id<TType>>> {
        tys.iter()
            .map(|ty| self.normalize_ty(*ty, span))
            .try_collect_vec()
    }

    pub fn normalize_trait_ty(
        &mut self,
        ty: Id<TTraitType>,
        span: Span,
    ) -> AResult<Id<TTraitType>> {
        let TTraitType(tr, gs) = &*ty.lookup(self.ctx);

        Ok(TTraitType(*tr, self.normalize_tys(&gs, span)?).intern(self.ctx))
    }

    pub fn is_concrete_ty(&self, ty: Id<TType>) -> bool {
        match &*ty.lookup(self.ctx) {
            TType::Skolem(_)
            | TType::MethodSkolem(..)
            | TType::AssociatedSkolem(..)
            | TType::Int
            | TType::Float
            | TType::Char
            | TType::Bool
            | TType::String
            | TType::Never => true,
            TType::Infer(_) | TType::GenericInfer(_) | TType::Associated(..) => false,
            TType::Array(e) => self.is_concrete_ty(*e),
            TType::Tuple(es) => self.is_concrete_tys(&es),
            TType::Closure(ps, r) | TType::FnPtr(ps, r) =>
                self.is_concrete_tys(&ps) && self.is_concrete_ty(*r),
            TType::Dynamic(trait_ty) => {
                let TTraitTypeWithBindings(trait_ty, bindings) = &*trait_ty.lookup(self.ctx);
                self.is_concrete_trait_ty(*trait_ty)
                    && bindings.iter().all(|(_, ty)| self.is_concrete_ty(*ty))
            },
            TType::Object(_, gs) | TType::Enum(_, gs) => self.is_concrete_tys(&gs),
        }
    }

    pub fn is_concrete_tys(&self, tys: &[Id<TType>]) -> bool {
        tys.iter().all(|ty| self.is_concrete_ty(*ty))
    }

    pub fn is_concrete_trait_ty(&self, trait_ty: Id<TTraitType>) -> bool {
        self.is_concrete_tys(&trait_ty.lookup(self.ctx).1)
    }
}

impl PrettyPrint for TType {
    fn fmt(&self, f: &mut std::fmt::Formatter, ctx: &dyn AdelaideContext) -> std::fmt::Result {
        match self {
            TType::Skolem(id) | TType::MethodSkolem(_, id) => {
                write!(f, "{:?}{}", Pretty(id.name, ctx), id.id.0)?;
            },
            TType::AssociatedSkolem(id, ty, trait_ty, name) => {
                write!(
                    f,
                    "<{:?} as {:?}>::{:?} [SKOLEM {}]",
                    Pretty(ty, ctx),
                    Pretty(trait_ty, ctx),
                    Pretty(name, ctx),
                    id.0,
                )?;
            },
            TType::GenericInfer(id) => {
                write!(f, "_G{}", id.0)?;
            },
            TType::Infer(id) => {
                write!(f, "_{}", id.0)?;
            },
            TType::Int => {
                write!(f, "Int")?;
            },
            TType::Float => {
                write!(f, "Float")?;
            },
            TType::Char => {
                write!(f, "Char")?;
            },
            TType::Bool => {
                write!(f, "Bool")?;
            },
            TType::String => {
                write!(f, "String")?;
            },
            TType::Never => {
                write!(f, "!")?;
            },
            TType::Array(e) => {
                write!(f, "[{:?}]", Pretty(e, ctx))?;
            },
            TType::Tuple(ts) if ts.len() == 0 => {
                write!(f, "()")?;
            },
            TType::Tuple(ts) if ts.len() == 1 => {
                write!(f, "({:?},)", Pretty(ts[0], ctx))?;
            },
            TType::Tuple(ts) => {
                write!(f, "(")?;

                for (i, t) in ts.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{:?}", Pretty(t, ctx))?;
                }

                write!(f, ")")?;
            },
            TType::Closure(ps, r) => {
                write!(f, "|")?;

                for (i, t) in ps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{:?}", Pretty(t, ctx))?;
                }

                write!(f, "| -> {:?}", Pretty(r, ctx))?;
            },
            TType::FnPtr(ps, r) => {
                write!(f, "fn(")?;

                for (i, t) in ps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{:?}", Pretty(t, ctx))?;
                }

                write!(f, ") -> {:?}", Pretty(r, ctx))?;
            },
            TType::Dynamic(trait_ty) => {
                write!(f, "Dyn<{:?}>", Pretty(trait_ty, ctx))?;
            },
            TType::Object(o, gs) => {
                write!(f, "{:?}", Pretty(o.lookup(ctx).name, ctx))?;

                if gs.len() > 0 {
                    write!(f, "<")?;

                    for (i, t) in gs.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }

                        write!(f, "{:?}", Pretty(t, ctx))?;
                    }

                    write!(f, ">")?;
                }
            },
            TType::Enum(o, gs) => {
                write!(f, "{:?}", Pretty(o.lookup(ctx).name, ctx))?;

                if gs.len() > 0 {
                    write!(f, "<")?;

                    for (i, t) in gs.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }

                        write!(f, "{:?}", Pretty(t, ctx))?;
                    }

                    write!(f, ">")?;
                }
            },
            TType::Associated(_, ty, Some(trait_ty), name) => {
                write!(
                    f,
                    "<{:?} as {:?}>::{:?}",
                    Pretty(ty, ctx),
                    Pretty(trait_ty, ctx),
                    Pretty(name, ctx)
                )?;
            },
            TType::Associated(_, ty, None, name) => {
                write!(f, "<{:?}>::{:?}", Pretty(ty, ctx), Pretty(name, ctx))?;
            },
        }

        Ok(())
    }
}

impl PrettyPrint for TTraitType {
    fn fmt(&self, f: &mut std::fmt::Formatter, ctx: &dyn AdelaideContext) -> std::fmt::Result {
        let TTraitType(o, gs) = self;

        write!(f, "{:?}", Pretty(o.lookup(ctx).name, ctx))?;

        if gs.len() > 0 {
            write!(f, "<")?;

            for (i, t) in gs.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }

                write!(f, "{:?}", Pretty(t, ctx))?;
            }

            write!(f, ">")?;
        }

        Ok(())
    }
}

impl PrettyPrint for TTraitTypeWithBindings {
    fn fmt(&self, f: &mut std::fmt::Formatter, ctx: &dyn AdelaideContext) -> std::fmt::Result {
        let TTraitTypeWithBindings(trait_ty, bindings) = self;

        let trait_ty = trait_ty.lookup(ctx);
        write!(f, "{:?}", Pretty(trait_ty.0.lookup(ctx).name, ctx))?;

        let mut generics = 0usize;

        for t in &trait_ty.1 {
            write!(
                f,
                "{}{:?}",
                if generics == 0 { "<" } else { ", " },
                Pretty(t, ctx)
            )?;

            generics += 1;
        }

        for (n, t) in bindings {
            write!(
                f,
                "{}{:?} = {:?}",
                if generics == 0 { "<" } else { ", " },
                Pretty(n, ctx),
                Pretty(t, ctx)
            )?;

            generics += 1;
        }

        if generics > 0 {
            write!(f, ">")?;
        }

        Ok(())
    }
}
