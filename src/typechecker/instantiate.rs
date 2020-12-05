use std::{collections::BTreeMap, iter::once};

use crate::{
    lexer::Span,
    lowering::{LEnum, LFunction, LMembers, LObject, LTrait},
    util::{AResult, Id, Intern, Pretty, TryCollectVec, ZipExact},
};

use super::{
    item::TRestriction, solve::TImplWitness, ty::TTraitTypeWithBindings, TTraitType, TType,
    Typechecker,
};

impl Typechecker<'_> {
    pub fn instantiate_function(
        &mut self,
        f: Id<LFunction>,
        tr_generics: &[Id<TType>],
    ) -> AResult<(Vec<Id<TType>>, Id<TType>, Vec<TRestriction>)> {
        let info = f.lookup(self.ctx);

        let substitutions: BTreeMap<_, _> = info
            .generics
            .iter()
            .zip_exact(tr_generics)
            .map(|(g, ty)| (g.id, *ty))
            .collect();

        Ok((
            info.parameters
                .iter()
                .map(|v| self.initialize_ty(v.ty, &substitutions))
                .try_collect_vec()?,
            self.initialize_ty(info.return_ty, &substitutions)?,
            self.initialize_restrictions(&info.restrictions, &substitutions)?,
        ))
    }

    pub fn instantiate_trait_fn(
        &mut self,
        self_ty: Id<TType>,
        tr: Id<LTrait>,
        tr_generics: &[Id<TType>],
        name: Id<str>,
        fn_generics: &[Id<TType>],
    ) -> AResult<(Vec<Id<TType>>, Id<TType>, Vec<TRestriction>)> {
        let info = tr.lookup(self.ctx);
        debug!(
            "Instantiating {:?} in {:?}",
            Pretty(name, self.ctx),
            Pretty(tr, self.ctx)
        );
        let method_info = &tr.lookup(self.ctx).methods[&name];

        let substitutions: BTreeMap<_, _> = Iterator::chain(
            info.generics.iter().zip_exact(tr_generics),
            method_info.generics.iter().zip_exact(fn_generics),
        )
        .map(|(g, ty)| (g.id, *ty))
        .chain(once((info.self_skolem.id, self_ty)))
        .collect();

        debug!("Substitutions: {:?}", Pretty(&substitutions, self.ctx));

        Ok((
            method_info
                .parameters
                .iter()
                .map(|v| self.initialize_ty(v.ty, &substitutions))
                .try_collect_vec()?,
            self.initialize_ty(method_info.return_ty, &substitutions)?,
            self.initialize_restrictions(&method_info.restrictions, &substitutions)?,
        ))
    }

    pub fn instantiate_object(
        &mut self,
        obj: Id<LObject>,
        generics: &[Id<TType>],
    ) -> AResult<(Id<TType>, Vec<Id<TType>>, Vec<TRestriction>)> {
        let info = obj.lookup(self.ctx);

        let substitutions = info
            .generics
            .iter()
            .zip_exact(generics.iter())
            .map(|(g, t)| (g.id, *t))
            .collect();

        let members = match &info.members {
            LMembers::Empty(_) => vec![],
            LMembers::Positional(_, members) | LMembers::Named(_, members, _) =>
                self.initialize_tys(&members, &substitutions)?,
        };

        Ok((
            TType::Object(obj, generics.to_owned()).intern(self.ctx),
            members,
            self.initialize_restrictions(&info.restrictions, &substitutions)?,
        ))
    }

    pub fn instantiate_enum_variant(
        &mut self,
        enumerable: Id<LEnum>,
        generics: &[Id<TType>],
        name: Id<str>,
    ) -> AResult<(Id<TType>, Vec<Id<TType>>, Vec<TRestriction>)> {
        let info = enumerable.lookup(self.ctx);

        let substitutions = info
            .generics
            .iter()
            .zip_exact(generics.iter())
            .map(|(g, t)| (g.id, *t))
            .collect();

        let members = match &info.variants[&name] {
            LMembers::Empty(_) => vec![],
            LMembers::Positional(_, members) | LMembers::Named(_, members, _) =>
                self.initialize_tys(&members, &substitutions)?,
        };

        Ok((
            TType::Enum(enumerable, generics.to_owned()).intern(self.ctx),
            members,
            self.initialize_restrictions(&info.restrictions, &substitutions)?,
        ))
    }

    pub fn instantiate_object_restrictions(
        &mut self,
        obj: Id<LObject>,
        generics: &[Id<TType>],
    ) -> AResult<Vec<TRestriction>> {
        let info = obj.lookup(self.ctx);

        self.initialize_restrictions(
            &info.restrictions,
            &info
                .generics
                .iter()
                .zip_exact(generics.iter())
                .map(|(g, t)| (g.id, *t))
                .collect(),
        )
    }

    pub fn instantiate_enum_restrictions(
        &mut self,
        enumerable: Id<LEnum>,
        generics: &[Id<TType>],
    ) -> AResult<Vec<TRestriction>> {
        let info = enumerable.lookup(self.ctx);

        self.initialize_restrictions(
            &info.restrictions,
            &info
                .generics
                .iter()
                .zip_exact(generics.iter())
                .map(|(g, t)| (g.id, *t))
                .collect(),
        )
    }

    pub fn instantiate_trait_ty_restrictions(
        &mut self,
        self_ty: Id<TType>,
        tr: Id<LTrait>,
        generics: &[Id<TType>],
        name: Id<str>,
    ) -> AResult<Vec<TTraitTypeWithBindings>> {
        let info = tr.lookup(self.ctx);

        let substitutions = &info
            .generics
            .iter()
            .zip_exact(generics.iter())
            .map(|(g, t)| (g.id, *t))
            .chain(once((info.self_skolem.id, self_ty)))
            .collect();

        info.types[&name]
            .iter()
            .map(|trait_ty| self.initialize_trait_ty_with_bindings(*trait_ty, substitutions))
            .try_collect_vec()
    }

    pub fn instantiate_ty_from_impl(
        &mut self,
        imp: &TImplWitness,
        name: Id<str>,
        span: Span,
    ) -> AResult<Id<TType>> {
        match imp {
            TImplWitness::Impl(i, gs) => {
                let info = i.lookup(self.ctx);
                let assoc_ty = info.types[&name];

                self.initialize_ty(assoc_ty, gs)
            },
            TImplWitness::Assumption(tr, ty, trait_ty) => self.normalize_ty(
                // Just get the ty directly from the assumptions. There really should be no need to
                // normalize it, but let's just normalize it for good measure!
                self.facts().assumptions[&(*tr, *ty, *trait_ty)].0[&name],
                span,
            ),
            TImplWitness::Dynamic(_, trait_ty) => self.normalize_ty(
                // Just get the ty directly from the dyn type. There really should be no need to
                // normalize it, but let's just normalize it for good measure!
                trait_ty.1[&name],
                span,
            ),
        }
    }

    pub fn instantiate_method_from_impl(
        &mut self,
        imp: &TImplWitness,
        name: Id<str>,
        fn_generics: &[Id<TType>],
        span: Span,
    ) -> AResult<(Vec<Id<TType>>, Id<TType>, Vec<TRestriction>)> {
        match imp {
            TImplWitness::Impl(i, gs) => {
                let method_info = &i.lookup(self.ctx).methods[&name];

                let mut substitutions = gs.clone();
                substitutions.extend(
                    method_info
                        .generics
                        .iter()
                        .map(|g| g.id)
                        .zip_exact(fn_generics.iter().cloned()),
                );

                Ok((
                    method_info
                        .parameters
                        .iter()
                        .map(|v| self.initialize_ty(v.ty, &substitutions))
                        .try_collect_vec()?,
                    self.initialize_ty(method_info.return_ty, &substitutions)?,
                    self.initialize_restrictions(&method_info.restrictions, &substitutions)?,
                ))
            },
            TImplWitness::Assumption(tr, ty, trait_ty) =>
                self.instantiate_trait_fn(*ty, *tr, &trait_ty.lookup(self.ctx).1, name, fn_generics),
            TImplWitness::Dynamic(ty, trait_ty) => {
                let TTraitType(tr, trait_generics) = &*trait_ty.0.lookup(self.ctx);
                self.instantiate_trait_fn(*ty, *tr, &trait_generics, name, fn_generics)
            },
        }
    }
}
