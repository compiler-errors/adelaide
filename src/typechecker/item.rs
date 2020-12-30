use std::collections::BTreeMap;

use crate::{
    lexer::Span,
    lowering::{
        GenericId, LEnum, LFunction, LGlobal, LImpl, LModule, LObject, LTrait, LTraitType,
        LTraitTypeWithBindings, LType, LTypeData, LVariableContext,
    },
    util::{AResult, Id, Intern, Pretty, TryCollectVec, ZipExact},
};

use super::{
    ty::{TTraitType, TTraitTypeWithBindings, TType, UnifyMode},
    Typechecker,
};

impl Typechecker<'_> {
    pub fn typecheck_module(&mut self, module: Id<LModule>) -> AResult<()> {
        let LModule {
            source: _,
            parent: _,
            modules: _,
            globals,
            functions,
            objects,
            enums,
            traits,
            impls,
        } = &*module.lookup(self.ctx);

        self.module = Some(module);

        for g in globals.values() {
            self.typecheck_loop(|self_| self_.typecheck_global(*g))?;
        }

        for f in functions.values() {
            self.typecheck_loop(|self_| self_.typecheck_function(*f))?;
        }

        for o in objects.values() {
            self.typecheck_loop(|self_| self_.typecheck_object(*o))?;
        }

        for e in enums.values() {
            self.typecheck_loop(|self_| self_.typecheck_enum(*e))?;
        }

        for t in traits.values() {
            self.typecheck_loop(|self_| self_.typecheck_trait(*t))?;
        }

        for i in impls.values() {
            self.typecheck_loop(|self_| self_.typecheck_impl(*i))?;
        }

        self.module = None;

        Ok(())
    }

    pub fn typecheck_global(&mut self, g: Id<LGlobal>) -> AResult<()> {
        let info = g.lookup(self.ctx);

        self.satisfy_vcx(&info.vcx)?;
        let ty = self.satisfy_ty(info.ty)?;
        let expr_ty = self.satisfy_expr(info.expr)?;

        let _ = self.unify_ty(
            UnifyMode::Normal,
            ty,
            info.ty.lookup(self.ctx).span,
            expr_ty,
            info.expr.lookup(self.ctx).span,
        )?;

        Ok(())
    }

    pub fn typecheck_function(&mut self, f: Id<LFunction>) -> AResult<()> {
        let info = f.lookup(self.ctx);

        // Also satisfies all the parameters
        self.satisfy_vcx(&info.vcx)?;

        let return_ty = self.satisfy_ty(info.return_ty)?;

        if let Some(body) = info.body {
            self.return_tys
                .push((return_ty, info.return_ty.lookup(self.ctx).span));

            let body_ty = self.satisfy_expr(body)?;

            let _ = self.unify_ty(
                UnifyMode::Normal,
                return_ty,
                info.return_ty.lookup(self.ctx).span,
                body_ty,
                body.lookup(self.ctx).span,
            )?;

            self.return_tys.pop();
        }

        self.satisfy_restrictions(&info.restrictions)?;

        Ok(())
    }

    fn typecheck_object(&mut self, o: Id<LObject>) -> AResult<()> {
        let info = o.lookup(self.ctx);

        match &info.members {
            crate::lowering::LMembers::Empty(_) => {},
            crate::lowering::LMembers::Positional(_, member_tys)
            | crate::lowering::LMembers::Named(_, member_tys, _) =>
                for ty in member_tys {
                    let _ = self.satisfy_ty(*ty)?;
                },
        }

        self.satisfy_restrictions(&info.restrictions)?;

        Ok(())
    }

    fn typecheck_enum(&mut self, e: Id<LEnum>) -> AResult<()> {
        let info = e.lookup(self.ctx);

        for v in info.variants.values() {
            match v {
                crate::lowering::LMembers::Empty(_) => {},
                crate::lowering::LMembers::Positional(_, member_tys)
                | crate::lowering::LMembers::Named(_, member_tys, _) =>
                    for ty in member_tys {
                        let _ = self.satisfy_ty(*ty)?;
                    },
            }
        }

        self.satisfy_restrictions(&info.restrictions)?;

        Ok(())
    }

    fn typecheck_trait(&mut self, t: Id<LTrait>) -> AResult<()> {
        let info = t.lookup(self.ctx);

        let self_skolem = LType {
            span: info.self_skolem.span,
            data: LTypeData::SelfSkolem(info.self_skolem),
        }
        .intern(self.ctx);
        let lowered_self_skolem = TType::Skolem(info.self_skolem).intern(self.ctx);
        let self_trait_ty = TTraitType(
            t,
            info.generics
                .iter()
                .map(|g| TType::Skolem(*g).intern(self.ctx))
                .collect(),
        )
        .intern(self.ctx);

        for (name, restrictions) in &info.types {
            let assoc_ty =
                TType::Associated(self_skolem, lowered_self_skolem, Some(self_trait_ty), *name)
                    .intern(self.ctx);

            for restriction in restrictions {
                let span = restriction.lookup(self.ctx).span;
                let restriction = self.satisfy_trait_ty_with_bindings(*restriction)?;

                self.do_goal_restriction(TRestriction(assoc_ty, restriction), &hashset! {}, span)?;
            }
        }

        for info in info.methods.values() {
            for p in &info.parameters {
                let _ = self.satisfy_ty(p.ty)?;
            }

            let _ = self.satisfy_ty(info.return_ty)?;

            self.satisfy_restrictions(&info.restrictions)?;
        }

        self.satisfy_restrictions(&info.restrictions)?;

        Ok(())
    }

    fn typecheck_impl(&mut self, i: Id<LImpl>) -> AResult<()> {
        let info = i.lookup(self.ctx);

        let self_ty = self.satisfy_ty(info.ty)?;

        let trait_ty = info
            .trait_ty
            .map(|t| self.satisfy_trait_ty(t))
            .transpose()?;

        for (name, ty) in &info.types {
            let span = ty.lookup(self.ctx).span;
            let ty = self.satisfy_ty(*ty)?;

            if let Some(trait_ty) = trait_ty {
                let trait_ty = trait_ty.lookup(self.ctx);

                let restrictions = self
                    .instantiate_trait_ty_restrictions(self_ty, trait_ty.0, &trait_ty.1, *name)?
                    .into_iter()
                    .map(|trait_ty| TRestriction(ty, trait_ty))
                    .collect();

                self.satisfy_instantiated_restrictions(restrictions, span)?;
            }
        }

        for method_info in info.methods.values() {
            // Also satisfies all the parameters
            self.satisfy_vcx(&method_info.vcx)?;

            let return_ty = self.satisfy_ty(method_info.return_ty)?;
            self.return_tys
                .push((return_ty, method_info.return_ty.lookup(self.ctx).span));

            let body_ty = self.satisfy_expr(method_info.body)?;
            let _ = self.unify_ty(
                UnifyMode::Normal,
                return_ty,
                method_info.return_ty.lookup(self.ctx).span,
                body_ty,
                method_info.body.lookup(self.ctx).span,
            )?;

            self.return_tys.pop();

            self.satisfy_restrictions(&method_info.restrictions)?;

            if let Some(trait_ty) = trait_ty {
                let trait_ty = trait_ty.lookup(self.ctx);
                let trait_info = trait_ty.0.lookup(self.ctx);
                let trait_method_info = &trait_info.methods[&method_info.name];

                let fn_generics: Vec<_> = method_info
                    .generics
                    .iter()
                    .map(|g| TType::Skolem(*g).intern(self.ctx))
                    .collect();

                let (expected_param_tys, expected_return_ty, restrictions) = self
                    .instantiate_trait_fn(
                        self_ty,
                        trait_ty.0,
                        &trait_ty.1,
                        method_info.name,
                        &fn_generics,
                    )?;

                let _ = self.unify_ty(
                    UnifyMode::Nothing,
                    expected_return_ty,
                    trait_method_info.return_ty.lookup(self.ctx).span,
                    return_ty,
                    method_info.return_ty.lookup(self.ctx).span,
                )?;

                // Just for good measure...
                assert_eq!(expected_param_tys.len(), method_info.parameters.len());

                for (i, expected_param_ty) in expected_param_tys.into_iter().enumerate() {
                    let _ = self.unify_ty(
                        UnifyMode::Nothing,
                        expected_param_ty,
                        trait_method_info.parameters[i].span,
                        // We already lowered the parameter while satisfying the vcx
                        self.variables[&method_info.parameters[i].id],
                        method_info.parameters[i].span,
                    )?;
                }

                // TODO: If either of these fail, probably better just to emit a general "Impl
                // and trait fn restrictions are not compatible" kind of error

                self.satisfy_instantiated_restrictions(restrictions, method_info.span)?;

                // Finally, and definitely not least: Instantiate the
                // restrictions "backwards" -- i.e., try to satisfy the impl fn
                // restrictions GIVEN the trait fn generics.
                // This ensures that the restrictions on the functions are not
                // more restrictive than the ones on the trait.
                let rev_restrictions = self.initialize_restrictions(
                    &method_info.restrictions,
                    &method_info
                        .generics
                        .iter()
                        .zip_exact(&trait_method_info.generics)
                        .map(|(g, ty)| (g.id, TType::MethodSkolem(i, *ty).intern(self.ctx)))
                        .collect(),
                )?;

                self.satisfy_instantiated_restrictions(rev_restrictions, method_info.span)?;
            }
        }

        if let Some(trait_ty) = trait_ty {
            let TTraitType(tr, trait_generics) = &*trait_ty.lookup(self.ctx);
            let trait_restrictions =
                self.instantiate_trait_restrictions(self_ty, *tr, &trait_generics)?;
            self.satisfy_instantiated_restrictions(trait_restrictions, info.span)?;
        }

        self.satisfy_restrictions(&info.restrictions)?;

        Ok(())
    }

    pub fn typecheck_method_oneoff(&mut self, imp: Id<LImpl>, name: Id<str>) -> AResult<()> {
        let info = imp.lookup(self.ctx);

        let self_ty = self.satisfy_ty(info.ty)?;

        let trait_ty = info
            .trait_ty
            .map(|t| self.satisfy_trait_ty(t))
            .transpose()?;

        debug!(
            "Instantiated self_ty = {:?}, trait_ty = {:?}",
            Pretty(self_ty, self.ctx),
            Pretty(trait_ty, self.ctx)
        );

        let method_info = &info.methods[&name];

        // Also satisfies all the parameters
        self.satisfy_vcx(&method_info.vcx)?;

        let return_ty = self.satisfy_ty(method_info.return_ty)?;
        self.return_tys
            .push((return_ty, method_info.return_ty.lookup(self.ctx).span));

        let body_ty = self.satisfy_expr(method_info.body)?;
        let _ = self.unify_ty(
            UnifyMode::Normal,
            return_ty,
            method_info.return_ty.lookup(self.ctx).span,
            body_ty,
            method_info.body.lookup(self.ctx).span,
        )?;

        self.return_tys.pop();

        self.satisfy_restrictions(&method_info.restrictions)?;

        if let Some(trait_ty) = trait_ty {
            let trait_ty = trait_ty.lookup(self.ctx);
            let trait_info = trait_ty.0.lookup(self.ctx);
            let trait_method_info = &trait_info.methods[&method_info.name];

            let fn_generics: Vec<_> = method_info
                .generics
                .iter()
                .map(|g| self.global_substitutions.as_ref().unwrap()[&g.id])
                .collect();

            let (expected_param_tys, expected_return_ty, restrictions) = self
                .instantiate_trait_fn(
                    self_ty,
                    trait_ty.0,
                    &trait_ty.1,
                    method_info.name,
                    &fn_generics,
                )?;

            let _ = self.unify_ty(
                UnifyMode::Nothing,
                expected_return_ty,
                trait_method_info.return_ty.lookup(self.ctx).span,
                return_ty,
                method_info.return_ty.lookup(self.ctx).span,
            )?;

            // Just for good measure...
            assert_eq!(expected_param_tys.len(), method_info.parameters.len());

            for (i, expected_param_ty) in expected_param_tys.into_iter().enumerate() {
                let _ = self.unify_ty(
                    UnifyMode::Nothing,
                    expected_param_ty,
                    trait_method_info.parameters[i].span,
                    // We already lowered the parameter while satisfying the vcx
                    self.variables[&method_info.parameters[i].id],
                    method_info.parameters[i].span,
                )?;
            }

            self.satisfy_instantiated_restrictions(restrictions, method_info.span)?;
        }

        Ok(())
    }

    pub fn satisfy_ty(&mut self, ty: Id<LType>) -> AResult<Id<TType>> {
        let span = ty.lookup(self.ctx).span;
        let ty = self.initialize_ty(ty, &btreemap! {})?;

        self.do_goal_well_formed(ty, span)?;

        Ok(ty)
    }

    pub fn satisfy_tys(&mut self, tys: &[Id<LType>]) -> AResult<Vec<Id<TType>>> {
        tys.iter().map(|ty| self.satisfy_ty(*ty)).try_collect_vec()
    }

    pub fn satisfy_trait_ty(&mut self, trait_ty: Id<LTraitType>) -> AResult<Id<TTraitType>> {
        let span = trait_ty.lookup(self.ctx).span;
        let trait_ty = self.initialize_trait_ty(trait_ty, &btreemap! {})?;

        for ty in &trait_ty.lookup(self.ctx).1 {
            self.do_goal_well_formed(*ty, span)?;
        }

        Ok(trait_ty)
    }

    fn satisfy_trait_ty_with_bindings(
        &mut self,
        trait_ty: Id<LTraitTypeWithBindings>,
    ) -> AResult<Id<TTraitTypeWithBindings>> {
        let span = trait_ty.lookup(self.ctx).span;
        let trait_ty = self.initialize_trait_ty_with_bindings(trait_ty, &btreemap! {})?;

        // Sorry for two lookups in this fn, I'm Lazy(TM)
        for ty in &trait_ty.lookup(self.ctx).0.lookup(self.ctx).1 {
            self.do_goal_well_formed(*ty, span)?;
        }

        for ty in trait_ty.lookup(self.ctx).1.values() {
            self.do_goal_well_formed(*ty, span)?;
        }

        Ok(trait_ty)
    }

    pub fn satisfy_vcx(&mut self, vcx: &LVariableContext) -> AResult<()> {
        for (id, variable) in &vcx.variables {
            let mut ty = self.satisfy_ty(variable.ty)?;

            if let Some(old_variable) = vcx.captures.get(id) {
                let old_ty = self.satisfy_ty(old_variable.ty)?;

                ty = self.unify_ty(
                    UnifyMode::Normal,
                    ty,
                    variable.span,
                    old_ty,
                    old_variable.span,
                )?;
            }

            self.variables.insert(*id, ty);
        }

        Ok(())
    }

    fn satisfy_restrictions(
        &mut self,
        restrictions: &[(Id<LType>, Id<LTraitTypeWithBindings>)],
    ) -> AResult<()> {
        for (ty, trait_ty) in restrictions {
            let span = ty.lookup(self.ctx).span;
            let ty = self.satisfy_ty(*ty)?;
            let trait_ty = self.satisfy_trait_ty_with_bindings(*trait_ty)?;

            self.do_goal_restriction(TRestriction(ty, trait_ty), &hashset! {}, span)?;
        }

        Ok(())
    }

    pub fn satisfy_instantiated_restrictions(
        &mut self,
        restrictions: Vec<TRestriction>,
        span: Span,
    ) -> AResult<()> {
        for restriction in restrictions {
            let TRestriction(ty, trait_ty) = &restriction;
            let TTraitTypeWithBindings(trait_ty, bindings) = &*trait_ty.lookup(self.ctx);

            self.do_goal_well_formed(*ty, span)?;

            for ty in &trait_ty.lookup(self.ctx).1 {
                self.do_goal_well_formed(*ty, span)?;
            }

            for ty in bindings.values() {
                self.do_goal_well_formed(*ty, span)?;
            }

            self.do_goal_restriction(restriction, &hashset! {}, span)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TTrait {
    pub generics: Vec<GenericId>,
    pub restrictions: Vec<TRestriction>,
    pub types: BTreeMap<Id<str>, Vec<Id<TTraitTypeWithBindings>>>,
    pub methods: BTreeMap<Id<str>, TMethod>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TImpl {
    pub generics: Vec<GenericId>,
    pub restrictions: Vec<TRestriction>,
    pub types: BTreeMap<Id<str>, Id<TType>>,
    pub methods: BTreeMap<Id<str>, TMethod>,
}

#[derive(Debug, Clone, PartialEq, Eq, PrettyPrint)]
pub struct TRestriction(pub Id<TType>, pub Id<TTraitTypeWithBindings>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TMethod {
    pub has_self: bool,
    pub generics: Vec<GenericId>,
    pub parameters: Vec<Id<TType>>,
    pub restrictions: Vec<TRestriction>,
    pub return_ty: Id<TType>,
}
