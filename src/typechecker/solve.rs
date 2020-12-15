use std::collections::{BTreeMap, HashSet};

use either::Either;

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    lowering::{fresh_id, GenericId, InferId, LExpression, LImpl, LMembers, LTrait, LTypeData},
    util::{AError, AResult, Id, Intern, Pretty, PrettyPrint, TryCollectBTreeMap, ZipExact},
};

use super::{
    item::TRestriction,
    ty::{TTraitTypeWithBindings, UnifyMode},
    TEpoch, TGoal, TTraitType, TType, Typechecker,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TImplWitness {
    Impl(Id<LImpl>, BTreeMap<GenericId, Id<TType>>),
    Assumption(Id<LTrait>, Id<TType>, Id<TTraitType>),
    Dynamic(Id<TType>, TTraitTypeWithBindings),
    DynamicCoersion(Id<TType>, TTraitTypeWithBindings),
    Concrete,
}

impl PrettyPrint for TImplWitness {
    fn fmt(&self, f: &mut std::fmt::Formatter, ctx: &dyn AdelaideContext) -> std::fmt::Result {
        match self {
            TImplWitness::Impl(imp, _) => {
                let info = imp.lookup(ctx);

                if let Some(trait_ty) = info.trait_ty {
                    write!(
                        f,
                        "[impl {:?} for {:?}]",
                        Pretty(trait_ty.lookup(ctx).tr.lookup(ctx).name, ctx),
                        Pretty(info.ty, ctx)
                    )?;
                } else {
                    write!(f, "[impl Self for {:?}]", Pretty(info.ty, ctx))?;
                }
            },
            TImplWitness::Assumption(_, ty, trait_ty) => {
                write!(
                    f,
                    "[assumption, impl {:?} for {:?}]",
                    Pretty(trait_ty, ctx),
                    Pretty(ty, ctx)
                )?;
            },
            TImplWitness::Dynamic(ty, trait_ty) => {
                write!(
                    f,
                    "[automatic impl {:?} for {:?}]",
                    Pretty(trait_ty, ctx),
                    Pretty(ty, ctx)
                )?;
            },
            TImplWitness::Concrete => {
                write!(f, "[automatic impl Concrete]")?;
            },
            TImplWitness::DynamicCoersion(ty, trait_ty) => {
                write!(
                    f,
                    "[automatic impl Into<Dyn<{:?}>> for {:?}]",
                    Pretty(trait_ty, ctx),
                    Pretty(ty, ctx)
                )?;
            },
        }

        Ok(())
    }
}

impl Typechecker<'_> {
    pub fn do_goal_trait(
        &mut self,
        ty: Id<TType>,
        ty_span: Span,
        trait_ty: Id<TTraitType>,
        trait_ty_span: Span,
        ambiguous_ok: bool,
    ) -> AResult<Option<TImplWitness>> {
        let ty = self.normalize_ty(ty, ty_span)?;
        let trait_ty = self.normalize_trait_ty(trait_ty, trait_ty_span)?;

        let is_concrete = self.is_concrete_ty(ty) && self.is_concrete_trait_ty(trait_ty);

        debug!(
            "{}Trying goal {:?} implements {:?}",
            "  ".repeat(self.epochs.len() - 1),
            Pretty(ty, self.ctx),
            Pretty(trait_ty, self.ctx)
        );

        let goal = TGoal::Implements(ty, trait_ty);

        if let Some((witness, _)) = self.solved.get(&goal) {
            debug!(
                "{}We already know that {:?} implements {:?}",
                "  ".repeat(self.epochs.len() - 1),
                Pretty(ty, self.ctx),
                Pretty(trait_ty, self.ctx)
            );

            return Ok(Some(witness.clone()));
        }

        if self.within_goal(&goal) {
            debug!(
                "We're within the goal {:?}, bailing",
                Pretty(goal, self.ctx)
            );

            if !ambiguous_ok {
                self.set_ambiguity(AError::NoSolution {
                    ty,
                    ty_span,
                    trait_ty,
                    trait_ty_span,
                });
            }

            return Ok(None);
        }

        let mut solution_and_epoch = None;

        let tr = trait_ty.lookup(self.ctx).0;

        // Concrete implementation
        if tr == self.ctx.lower_concrete_item()? {
            match &*ty.lookup(self.ctx) {
                TType::Infer(_)
                | TType::GenericInfer(_)
                | TType::Associated(_, _, _)
                | TType::ObjectAccess(_, _) => {
                    if !ambiguous_ok {
                        self.set_ambiguity(AError::AmbiguousType {
                            ty,
                            use_span: ty_span,
                        });
                    }

                    return Ok(None);
                },
                TType::Skolem(_)
                | TType::MethodSkolem(_, _)
                | TType::AssociatedSkolem(_, _, _, _) => {
                    // Actually, we don't know!
                },
                TType::Int
                | TType::Float
                | TType::Char
                | TType::Bool
                | TType::String
                | TType::Never
                | TType::Array(_)
                | TType::Tuple(_)
                | TType::Closure(_, _)
                | TType::FnPtr(_, _)
                | TType::Object(_, _)
                | TType::Enum(_, _) => {
                    debug!(
                        "{}!!We proved goal {:?} implements {:?}",
                        "  ".repeat(self.epochs.len() - 1),
                        Pretty(ty, self.ctx),
                        Pretty(trait_ty, self.ctx)
                    );

                    self.epoch().progress = true;

                    self.solved.insert(goal, (TImplWitness::Concrete, ty_span));
                    return Ok(Some(TImplWitness::Concrete));
                },
                TType::Dynamic(_) => {
                    return Err(AError::NoSolution {
                        ty,
                        ty_span,
                        trait_ty,
                        trait_ty_span,
                    });
                },
            }
        }

        if tr == self.ctx.lower_into_item()? {
            let into_ty = trait_ty.lookup(self.ctx).1[0];

            match &*into_ty.lookup(self.ctx) {
                TType::Infer(_)
                | TType::GenericInfer(_)
                | TType::Associated(_, _, _)
                | TType::ObjectAccess(_, _) => {
                    if !ambiguous_ok {
                        self.set_ambiguity(AError::AmbiguousType {
                            ty,
                            use_span: ty_span,
                        });
                    }

                    return Ok(None);
                },
                TType::Dynamic(dyn_trait_ty) => {
                    let result = self.in_epoch(goal.clone(), |self_| {
                        self_.do_goal_restriction(
                            TRestriction(ty, dyn_trait_ty.clone()),
                            &hashset! {},
                            ty_span,
                        )?;
                        Ok(TImplWitness::DynamicCoersion(ty, dyn_trait_ty.clone()))
                    });

                    if let Ok((new_solution, new_epoch)) = result {
                        if let Some(ambiguity) = new_epoch.ambiguity {
                            if !ambiguous_ok {
                                self.set_ambiguity(ambiguity);
                            }

                            return Ok(None);
                        }

                        if !merge_solution(
                            is_concrete,
                            &mut solution_and_epoch,
                            new_solution,
                            new_epoch,
                        )? {
                            if !ambiguous_ok {
                                self.set_ambiguity(AError::NoSolution {
                                    ty,
                                    ty_span,
                                    trait_ty,
                                    trait_ty_span,
                                });
                            }

                            return Ok(None);
                        }
                    }
                },
                _ => {
                    // Not trying to look for Into<Dyn<_>>
                },
            }
        }

        if let TType::Dynamic(dyn_trait_ty) = &*ty.lookup(self.ctx) {
            let result = self.in_epoch(goal.clone(), |self_| {
                let trait_ty = self_.unify_trait_ty(
                    UnifyMode::GenericInference(&hashset! {}),
                    dyn_trait_ty.0,
                    ty_span,
                    trait_ty,
                    trait_ty_span,
                )?;

                Ok(TImplWitness::Dynamic(
                    ty,
                    TTraitTypeWithBindings(trait_ty, dyn_trait_ty.1.clone()),
                ))
            });

            if let Ok((new_solution, new_epoch)) = result {
                if let Some(ambiguity) = new_epoch.ambiguity {
                    if !ambiguous_ok {
                        self.set_ambiguity(ambiguity);
                    }

                    return Ok(None);
                }

                if !merge_solution(
                    is_concrete,
                    &mut solution_and_epoch,
                    new_solution,
                    new_epoch,
                )? {
                    if !ambiguous_ok {
                        self.set_ambiguity(AError::NoSolution {
                            ty,
                            ty_span,
                            trait_ty,
                            trait_ty_span,
                        });
                    }

                    return Ok(None);
                }
            }
        }

        for id in &*self.ctx.get_impls_for_trait(tr)? {
            let result = self.in_epoch(goal.clone(), move |self_| {
                self_.do_goal_impl(*id, ty, ty_span, Some((trait_ty, trait_ty_span)))
            });

            if let Ok((new_solution, new_epoch)) = result {
                if let Some(ambiguity) = new_epoch.ambiguity {
                    if !ambiguous_ok {
                        self.set_ambiguity(ambiguity);
                    }

                    return Ok(None);
                }

                if !merge_solution(
                    is_concrete,
                    &mut solution_and_epoch,
                    new_solution.expect("Should not be None if not ambiguous"),
                    new_epoch,
                )? {
                    if !ambiguous_ok {
                        self.set_ambiguity(AError::NoSolution {
                            ty,
                            ty_span,
                            trait_ty,
                            trait_ty_span,
                        });
                    }

                    return Ok(None);
                }
            }
        }

        if let Some((solution, epoch)) = solution_and_epoch {
            debug!(
                "{}!!We proved goal {:?} implements {:?}",
                "  ".repeat(self.epochs.len() - 1),
                Pretty(ty, self.ctx),
                Pretty(trait_ty, self.ctx)
            );

            self.commit_epoch(epoch);
            self.epoch().progress = true;

            self.solved.insert(goal, (solution.clone(), ty_span));
            return Ok(Some(solution));
        }

        if let Some(facts) = self.type_facts.clone() {
            for ((assumed_tr, assumed_ty, assumed_trait_ty), (_, assumed_span)) in
                &facts.assumptions
            {
                if *assumed_tr != tr {
                    continue;
                }

                let result = self.in_epoch(goal.clone(), |self_| {
                    let _ = self_.unify_ty(
                        UnifyMode::GenericInference(&hashset! {}),
                        *assumed_ty,
                        *assumed_span,
                        ty,
                        ty_span,
                    )?;
                    let _ = self_.unify_trait_ty(
                        UnifyMode::GenericInference(&hashset! {}),
                        *assumed_trait_ty,
                        *assumed_span,
                        trait_ty,
                        trait_ty_span,
                    )?;

                    Ok(TImplWitness::Assumption(
                        *assumed_tr,
                        *assumed_ty,
                        *assumed_trait_ty,
                    ))
                });

                if let Ok((new_solution, new_epoch)) = result {
                    if let Some(ambiguity) = new_epoch.ambiguity {
                        if !ambiguous_ok {
                            self.set_ambiguity(ambiguity);
                        }

                        return Ok(None);
                    }

                    if !merge_solution(
                        is_concrete,
                        &mut solution_and_epoch,
                        new_solution,
                        new_epoch,
                    )? {
                        if !ambiguous_ok {
                            self.set_ambiguity(AError::NoSolution {
                                ty,
                                ty_span,
                                trait_ty,
                                trait_ty_span,
                            });
                        }

                        return Ok(None);
                    }
                }
            }
        }

        if let Some((solution, epoch)) = solution_and_epoch {
            debug!(
                "{}!!We proved goal {:?} implements {:?}",
                "  ".repeat(self.epochs.len() - 1),
                Pretty(ty, self.ctx),
                Pretty(trait_ty, self.ctx)
            );

            self.commit_epoch(epoch);
            self.epoch().progress = true;

            self.solved.insert(goal, (solution.clone(), ty_span));
            Ok(Some(solution))
        } else if self.type_facts.is_none() {
            Ok(None)
        } else {
            Err(AError::NoSolution {
                ty,
                ty_span,
                trait_ty,
                trait_ty_span,
            })
        }
    }

    pub fn do_goal_impl(
        &mut self,
        id: Id<LImpl>,
        ty: Id<TType>,
        span: Span,
        maybe_trait: Option<(Id<TTraitType>, Span)>,
    ) -> AResult<Option<TImplWitness>> {
        let info = id.lookup(self.ctx);

        let mut substitutions = btreemap! {};
        let mut allowed = hashset! {};

        for g in &info.generics {
            let id = fresh_id();
            allowed.insert(id);
            substitutions.insert(g.id, TType::GenericInfer(id).intern(self.ctx));
            self.infer_spans.insert(id, span);
        }

        let impl_ty = self.initialize_ty(info.ty, &substitutions)?;

        debug!(
            "Ok, given {:?} substitutions, we're attempting to unify {:?} + {:?}",
            Pretty(&substitutions, self.ctx),
            Pretty(ty, self.ctx),
            Pretty(impl_ty, self.ctx),
        );

        let _ = self.unify_ty(
            UnifyMode::GenericInference(&allowed),
            impl_ty,
            info.span,
            ty,
            span,
        )?;

        if let Some((trait_ty, trait_ty_span)) = maybe_trait {
            let impl_trait_ty = self.initialize_trait_ty(
                info.trait_ty.expect("Expecting impl for a given trait"),
                &substitutions,
            )?;

            debug!(
                "... ALSO (trait) given {:?} substitutions, we're attempting to unify {:?} + {:?}",
                Pretty(&substitutions, self.ctx),
                Pretty(trait_ty, self.ctx),
                Pretty(impl_trait_ty, self.ctx),
            );

            let _ = self.unify_trait_ty(
                UnifyMode::GenericInference(&allowed),
                impl_trait_ty,
                info.span,
                trait_ty,
                trait_ty_span,
            )?;
        }

        for restriction in self.initialize_restrictions(&info.restrictions, &substitutions)? {
            self.do_goal_restriction(restriction, &allowed, span)?;
        }

        for ty in substitutions.values() {
            if let TType::GenericInfer(id) = &*self.normalize_ty(*ty, span)?.lookup(self.ctx) {
                if allowed.contains(id) {
                    self.set_ambiguity(AError::AmbiguousType {
                        use_span: span,
                        ty: *ty,
                    });
                    return Ok(None);
                }
            }
        }

        let substitutions = substitutions
            .into_iter()
            .map(|(g, ty)| -> AResult<_> { Ok((g, self.normalize_ty(ty, span)?)) })
            .try_collect_btreemap()?;

        debug!("Mapping = {:?}", Pretty(&substitutions, self.ctx));

        Ok(Some(TImplWitness::Impl(id, substitutions)))
    }

    pub fn do_goal_associated_type(
        &mut self,
        ty: Id<TType>,
        name: Id<str>,
        span: Span,
    ) -> AResult<Id<TType>> {
        let ty = self.normalize_ty(ty, span)?;
        let is_concrete = self.is_concrete_ty(ty);

        debug!(
            "{}Trying goal {:?} has associated type {:?}",
            "  ".repeat(self.epochs.len() - 1),
            Pretty(ty, self.ctx),
            Pretty(name, self.ctx)
        );

        let goal = TGoal::AssociatedType(self.module.unwrap(), ty, name);

        if let Some((witness, _)) = self.solved.get(&goal).cloned() {
            return self.instantiate_ty_from_impl(&witness, name, span);
        }

        if self.within_goal(&goal) {
            panic!("We should never get here... right?");
            // return Ok(TType::Associated(ty, None, name).intern(self.ctx));
        }

        let mut solution_and_epoch = None;

        for id in &*self.ctx.get_inherent_impls()? {
            // We only care about impls that provide the associated type we're looking for.
            if !id.lookup(self.ctx).types.contains_key(&name) {
                continue;
            }

            let result = self.in_epoch(goal.clone(), move |self_| {
                self_.do_goal_impl(*id, ty, span, None)
            });

            if let Ok((new_solution, new_epoch)) = result {
                if let Some(ambiguity) = new_epoch.ambiguity {
                    debug!(
                        "Hit an ambiguity {:?}, bailing",
                        Pretty(&ambiguity, self.ctx)
                    );

                    return Ok(TType::Associated(ty, None, name).intern(self.ctx));
                }

                if !merge_solution(
                    is_concrete,
                    &mut solution_and_epoch,
                    new_solution.expect("Should not be None if not ambiguous"),
                    new_epoch,
                )? {
                    return Ok(TType::Associated(ty, None, name).intern(self.ctx));
                }
            }
        }

        if let Some((solution, epoch)) = solution_and_epoch {
            debug!(
                "{}!!We proved goal {:?} has associated type {:?}",
                "  ".repeat(self.epochs.len() - 1),
                Pretty(ty, self.ctx),
                Pretty(name, self.ctx)
            );

            self.commit_epoch(epoch);
            self.epoch().progress = true;

            self.solved.insert(goal, (solution.clone(), span));
            return self.instantiate_ty_from_impl(&solution, name, span);
        }

        let mut candidate_tr = None;

        for tr in &*self
            .ctx
            .get_traits_accessible_in_module(self.module.unwrap())?
        {
            if tr.lookup(self.ctx).types.contains_key(&name) {
                if let Some(other_tr) = candidate_tr {
                    todo!("Die");
                }

                candidate_tr = Some(*tr);
            }
        }

        if let Some(tr) = candidate_tr {
            if tr.lookup(self.ctx).generics.len() > 0 {
                todo!("Die");
            }

            let trait_ty = TTraitType(tr, vec![]).intern(self.ctx);

            if let Some(witness) = self.do_goal_trait(ty, span, trait_ty, span, true)? {
                self.epoch().progress = true;
                self.solved.insert(goal, (witness.clone(), span));
                self.instantiate_ty_from_impl(&witness, name, span)
            } else {
                Ok(TType::Associated(ty, Some(trait_ty), name).intern(self.ctx))
            }
        } else {
            todo!("Die");
        }
    }

    pub fn do_goal_method_selection(
        &mut self,
        key: Id<LExpression>,
        has_self: bool,
        call_ty: Id<TType>,
        name: Id<str>,
        fn_generic_tys: &[Id<TType>],
        param_tys: &[Id<TType>],
        return_ty: Id<TType>,
        param_spans: &[Span],
        span: Span,
    ) -> AResult<Option<(TImplWitness, Vec<Id<TType>>)>> {
        let call_ty = self.normalize_ty(call_ty, span)?;
        let is_concrete = self.is_concrete_ty(call_ty)
            && self.is_concrete_tys(&fn_generic_tys)
            && self.is_concrete_tys(&param_tys)
            && self.is_concrete_ty(return_ty);

        debug!(
            "{}Trying goal {:?} has method {:?}",
            "  ".repeat(self.epochs.len() - 1),
            Pretty(call_ty, self.ctx),
            Pretty(name, self.ctx)
        );

        let goal = TGoal::Method(key);

        if let Some((witness, _)) = self.solved.get(&goal).cloned() {
            let (expected, expected_span) = self.get_generics_parity(&witness, name);
            let fn_generic_tys =
                self.check_generics_parity(key, fn_generic_tys, span, expected, expected_span)?;

            return Ok(Some((witness, fn_generic_tys)));
        }

        if self.within_goal(&goal) {
            panic!("We should never get here... right?");
            // self.set_ambiguity(AError::AmbiguousMethod { call_ty, name, span
            // }); return Ok(None);
        }

        let mut solution_and_epoch = None;

        for id in &*self.ctx.get_inherent_impls()? {
            // We only care about impls that provide the associated type we're looking for.
            match id.lookup(self.ctx).methods.get(&name) {
                None => {
                    continue;
                },
                Some(method) if has_self && !method.has_self => {
                    continue;
                },
                _ => {},
            }

            let result = self.in_epoch(goal.clone(), move |self_| {
                self_.do_goal_impl(*id, call_ty, span, None)
            });

            if let Ok((new_solution, new_epoch)) = result {
                if let Some(ambiguity) = new_epoch.ambiguity {
                    debug!(
                        "Hit an ambiguity {:?}, bailing",
                        Pretty(&ambiguity, self.ctx)
                    );

                    self.set_ambiguity(AError::AmbiguousMethod {
                        call_ty,
                        name,
                        span,
                    });

                    return Ok(None);
                }

                if !merge_solution(
                    is_concrete,
                    &mut solution_and_epoch,
                    new_solution.expect("Should not be None if not ambiguous"),
                    new_epoch,
                )? {
                    self.set_ambiguity(AError::AmbiguousMethod {
                        call_ty,
                        name,
                        span,
                    });

                    return Ok(None);
                }
            }
        }

        if let Some((solution, epoch)) = solution_and_epoch {
            let (expected, expected_span) = self.get_generics_parity(&solution, name);
            let fn_generic_tys =
                self.check_generics_parity(key, fn_generic_tys, span, expected, expected_span)?;

            debug!(
                "{}!!We proved goal {:?} has method {:?}",
                "  ".repeat(self.epochs.len() - 1),
                Pretty(call_ty, self.ctx),
                Pretty(name, self.ctx)
            );

            self.commit_epoch(epoch);
            self.epoch().progress = true;

            self.solved.insert(goal, (solution.clone(), span));
            return Ok(Some((solution, fn_generic_tys)));
        }

        let mut candidate_tr = None;

        for tr in &*self
            .ctx
            .get_traits_accessible_in_module(self.module.unwrap())?
        {
            debug!(
                "Trying trait {:?}",
                Pretty(tr.lookup(self.ctx).name, self.ctx)
            );

            match tr.lookup(self.ctx).methods.get(&name) {
                None => {
                    continue;
                },
                Some(method) if has_self && !method.has_self => {
                    debug!(
                        "Skipping because has_self={}, method.has_self={}",
                        has_self, method.has_self
                    );
                    continue;
                },
                _ => {},
            }

            if let Some(_) = candidate_tr {
                debug!("Candidate conflicted");
                return Err(AError::AmbiguousMethod {
                    call_ty,
                    name,
                    span,
                });
            }

            candidate_tr = Some(*tr);
        }

        if let Some(tr) = candidate_tr {
            let info = tr.lookup(self.ctx);

            // Either get cached trait types, or make N new ones
            // (for N generics in the trait)
            let trait_generic_tys = if let Some(trait_generic_tys) = self.trait_generics.get(&key) {
                trait_generic_tys.clone()
            } else {
                let generics: Vec<_> = (0..info.generics.len())
                    .map(|_| self.fresh_infer_ty(span))
                    .collect();
                self.trait_generics.insert(key, generics.clone());
                generics
            };
            let trait_ty = TTraitType(tr, trait_generic_tys).intern(self.ctx);

            let method_info = &info.methods[&name];
            let fn_generic_tys = self.check_generics_parity(
                key,
                fn_generic_tys,
                span,
                method_info.generics.len(),
                method_info.span,
            )?;

            self.do_goal_trait_call_unification(
                call_ty,
                trait_ty,
                name,
                &fn_generic_tys,
                &param_tys,
                return_ty,
                &param_spans,
                span,
            )?;

            if let Some(witness) = self.do_goal_trait(call_ty, span, trait_ty, span, true)? {
                self.epoch().progress = true;
                self.solved.insert(goal, (witness.clone(), span));
                return Ok(Some((witness, fn_generic_tys)));
            } else {
                self.set_ambiguity(AError::AmbiguousMethod {
                    call_ty,
                    name,
                    span,
                });

                Ok(None)
            }
        } else {
            debug!("No candidate");
            Err(AError::AmbiguousMethod {
                call_ty,
                name,
                span,
            })
        }
    }

    pub fn do_goal_trait_call_unification(
        &mut self,
        call_ty: Id<TType>,
        trait_ty: Id<TTraitType>,
        name: Id<str>,
        fn_generic_tys: &[Id<TType>],
        param_tys: &[Id<TType>],
        return_ty: Id<TType>,
        param_spans: &[Span],
        span: Span,
    ) -> AResult<()> {
        let TTraitType(tr, tr_generics) = &*trait_ty.lookup(self.ctx);

        let (expected_param_tys, expected_return_ty, restrictions) =
            self.instantiate_trait_fn(call_ty, *tr, &tr_generics, name, &fn_generic_tys)?;

        for ((expected_param_ty, param_ty), param_span) in expected_param_tys
            .iter()
            .zip_exact(param_tys)
            .zip_exact(param_spans)
        {
            let _ = self.unify_ty(
                UnifyMode::Normal,
                *expected_param_ty,
                *param_span,
                *param_ty,
                *param_span,
            )?;
        }
        let _ = self.unify_ty(UnifyMode::Normal, expected_return_ty, span, return_ty, span)?;

        for restriction in restrictions {
            self.do_goal_restriction(restriction, &hashset! {}, span)?;
        }

        Ok(())
    }

    pub fn do_goal_impl_call_unification(
        &mut self,
        witness: &TImplWitness,
        name: Id<str>,
        fn_generic_tys: &[Id<TType>],
        param_tys: &[Id<TType>],
        return_ty: Id<TType>,
        param_spans: &[Span],
        span: Span,
    ) -> AResult<Id<TType>> {
        let (expected_param_tys, expected_return_ty, restrictions) =
            self.instantiate_method_from_impl(witness, name, &fn_generic_tys, span)?;

        for ((expected_param_ty, param_ty), param_span) in expected_param_tys
            .iter()
            .zip_exact(param_tys)
            .zip_exact(param_spans)
        {
            let _ = self.unify_ty(
                UnifyMode::Normal,
                *expected_param_ty,
                *param_span,
                *param_ty,
                *param_span,
            )?;
        }
        let return_ty =
            self.unify_ty(UnifyMode::Normal, expected_return_ty, span, return_ty, span)?;

        for restriction in restrictions {
            self.do_goal_restriction(restriction, &hashset! {}, span)?;
        }

        Ok(return_ty)
    }

    pub fn do_goal_restriction(
        &mut self,
        TRestriction(r_ty, TTraitTypeWithBindings(r_trait_ty, extra_bindings)): TRestriction,
        allowed: &HashSet<InferId>,
        span: Span,
    ) -> AResult<()> {
        if let Some(witness) = self.do_goal_trait(r_ty, span, r_trait_ty, span, false)? {
            for (name, expected_ty) in extra_bindings {
                let found_ty = self.instantiate_ty_from_impl(&witness, name, span)?;

                let _ = self.unify_ty(
                    UnifyMode::GenericInference(allowed),
                    found_ty,
                    span,
                    expected_ty,
                    span,
                )?;
            }
        }

        Ok(())
    }

    pub fn do_goal_well_formed(&mut self, ty: Id<TType>, span: Span) -> AResult<()> {
        let ty = self.normalize_ty(ty, span)?;

        match &*ty.lookup(self.ctx) {
            TType::Infer(_)
            | TType::GenericInfer(_)
            | TType::Associated(_, _, _)
            | TType::ObjectAccess(_, _) => {
                self.set_ambiguity(AError::AmbiguousType { use_span: span, ty });
            },
            TType::Skolem(_)
            | TType::AssociatedSkolem(..)
            | TType::MethodSkolem(..)
            | TType::Int
            | TType::Float
            | TType::Char
            | TType::Bool
            | TType::String
            | TType::Never => { /* Well-formed */ },
            TType::Array(e) => {
                self.do_goal_well_formed(*e, span)?;
            },
            TType::Tuple(es) =>
                for e in es {
                    self.do_goal_well_formed(*e, span)?;
                },
            TType::Closure(ps, r) | TType::FnPtr(ps, r) => {
                for p in ps {
                    self.do_goal_well_formed(*p, span)?;
                }

                self.do_goal_well_formed(*r, span)?;
            },
            TType::Dynamic(trait_ty) => {
                // The Dyn type has to, de novo, satisfy the given trait restrictions
                let TTraitType(tr, trait_generics) = &*trait_ty.0.lookup(self.ctx);

                self.do_goal_object_safety(*tr, span)?;

                for ty in trait_generics {
                    self.do_goal_well_formed(*ty, span)?;
                }

                for (name, assoc_ty) in &trait_ty.1 {
                    self.do_goal_well_formed(*assoc_ty, span)?;

                    let ty_restrictions =
                        self.instantiate_trait_ty_restrictions(ty, *tr, &trait_generics, *name)?;

                    for restriction in ty_restrictions {
                        self.do_goal_restriction(
                            TRestriction(*assoc_ty, restriction),
                            &hashset! {},
                            span,
                        )?;
                    }
                }

                let restrictions = self.instantiate_trait_restrictions(ty, *tr, &trait_generics)?;
                for restriction in restrictions {
                    self.do_goal_restriction(restriction, &hashset! {}, span)?;
                }
            },
            TType::Object(o, gs) => {
                let restrictions = self.instantiate_object_restrictions(*o, &gs)?;

                for restriction in restrictions {
                    self.do_goal_restriction(restriction, &hashset! {}, span)?;
                }

                for g in gs {
                    self.do_goal_well_formed(*g, span)?;
                }
            },
            TType::Enum(e, gs) => {
                let restrictions = self.instantiate_enum_restrictions(*e, &gs)?;

                for restriction in restrictions {
                    self.do_goal_restriction(restriction, &hashset! {}, span)?;
                }

                for g in gs {
                    self.do_goal_well_formed(*g, span)?;
                }
            },
        }

        Ok(())
    }

    pub fn do_goal_access(
        &mut self,
        ty: Id<TType>,
        name: Id<str>,
        span: Span,
    ) -> AResult<Id<TType>> {
        let ty = self.normalize_ty(ty, span)?;

        match &*ty.lookup(self.ctx) {
            TType::GenericInfer(_)
            | TType::Infer(_)
            | TType::Associated(_, _, _)
            | TType::ObjectAccess(_, _) => {
                self.set_ambiguity(AError::CannotAccess { ty, name, span });
                Ok(TType::ObjectAccess(ty, Either::Left(name)).intern(self.ctx))
            },
            TType::Object(o, gs) => {
                let info = o.lookup(self.ctx);

                self.get_member(
                    ty,
                    &info.members,
                    Either::Left(name),
                    &ZipExact::zip_exact(info.generics.iter().map(|g| g.id), gs.iter().copied())
                        .collect(),
                    span,
                )
            },
            TType::AssociatedSkolem(_, _, _, _)
            | TType::Skolem(_)
            | TType::MethodSkolem(..)
            | TType::Int
            | TType::Float
            | TType::Char
            | TType::Bool
            | TType::String
            | TType::Never
            | TType::Array(_)
            | TType::Tuple(_)
            | TType::Closure(_, _)
            | TType::FnPtr(_, _)
            | TType::Dynamic(_)
            | TType::Enum(_, _) => Err(AError::CannotAccess { ty, name, span }),
        }
    }

    pub fn do_goal_index_access(
        &mut self,
        ty: Id<TType>,
        idx: usize,
        span: Span,
    ) -> AResult<Id<TType>> {
        let ty = self.normalize_ty(ty, span)?;

        match &*ty.lookup(self.ctx) {
            TType::GenericInfer(_)
            | TType::Infer(_)
            | TType::Associated(_, _, _)
            | TType::ObjectAccess(_, _) => {
                self.set_ambiguity(AError::CannotAccessIdx { ty, idx, span });
                Ok(TType::ObjectAccess(ty, Either::Right(idx)).intern(self.ctx))
            },
            TType::Tuple(tys) if idx < tys.len() => Ok(tys[idx]),
            TType::Object(o, gs) => {
                let info = o.lookup(self.ctx);

                self.get_member(
                    ty,
                    &info.members,
                    Either::Right(idx),
                    &ZipExact::zip_exact(info.generics.iter().map(|g| g.id), gs.iter().copied())
                        .collect(),
                    span,
                )
            },
            TType::AssociatedSkolem(_, _, _, _)
            | TType::Skolem(_)
            | TType::MethodSkolem(..)
            | TType::Int
            | TType::Float
            | TType::Char
            | TType::Bool
            | TType::String
            | TType::Never
            | TType::Array(_)
            | TType::Closure(_, _)
            | TType::FnPtr(_, _)
            | TType::Dynamic(_)
            | TType::Enum(_, _)
            | TType::Tuple(_) => Err(AError::CannotAccessIdx { ty, idx, span }),
        }
    }

    fn get_member(
        &mut self,
        ty: Id<TType>,
        members: &LMembers,
        name_or_idx: Either<Id<str>, usize>,
        substitutions: &BTreeMap<GenericId, Id<TType>>,
        span: Span,
    ) -> AResult<Id<TType>> {
        match (name_or_idx, members) {
            (Either::Left(name), LMembers::Named(_, tys, members))
                if members.contains_key(&name) =>
                self.initialize_ty(tys[members[&name].0], &substitutions),
            (Either::Right(idx), LMembers::Positional(_, tys)) if idx < tys.len() =>
                self.initialize_ty(tys[idx], &substitutions),
            (Either::Left(name), _) => Err(AError::CannotAccess { ty, name, span }),
            (Either::Right(idx), _) => Err(AError::CannotAccessIdx { ty, idx, span }),
        }
    }

    fn get_generics_parity(&self, witness: &TImplWitness, name: Id<str>) -> (usize, Span) {
        match witness {
            TImplWitness::Impl(i, _) => {
                let info = i.lookup(self.ctx);
                (info.methods[&name].generics.len(), info.methods[&name].span)
            },
            TImplWitness::Assumption(t, _, _) => {
                let info = t.lookup(self.ctx);
                (info.methods[&name].generics.len(), info.methods[&name].span)
            },
            TImplWitness::Dynamic(_, trait_ty) => {
                let info = trait_ty.0.lookup(self.ctx).0.lookup(self.ctx);
                (info.methods[&name].generics.len(), info.methods[&name].span)
            },
            TImplWitness::DynamicCoersion(..) => (
                0,
                // Get the span of `Into::into`... that unwrap is not cute :/
                self.ctx.lower_into_item().unwrap().lookup(self.ctx).methods
                    [&self.ctx.static_name("into")]
                    .span,
            ),
            TImplWitness::Concrete => {
                unreachable!()
            },
        }
    }

    fn check_generics_parity(
        &mut self,
        key: Id<LExpression>,
        given: &[Id<TType>],
        given_span: Span,
        expected: usize,
        expected_span: Span,
    ) -> AResult<Vec<Id<TType>>> {
        if let Some(generics) = self.function_generics.get(&key) {
            Ok(generics.clone())
        } else if given.len() == expected {
            self.function_generics.insert(key, given.to_owned());
            Ok(given.to_owned())
        } else if given.is_empty() {
            let generics: Vec<_> = (0..expected)
                .map(|_| self.fresh_infer_ty(given_span))
                .collect();
            self.function_generics.insert(key, generics.clone());
            Ok(generics)
        } else {
            Err(AError::ParityDisparity {
                kind: "generic types",
                expected,
                expected_span,
                given: given.len(),
                given_span,
            })
        }
    }

    pub fn do_goal_object_safety(&mut self, tr: Id<LTrait>, use_span: Span) -> AResult<()> {
        if self.object_safe_traits.contains(&tr) {
            return Ok(());
        }

        let info = tr.lookup(self.ctx);

        let self_ty = TType::Skolem(info.self_skolem).intern(self.ctx);
        let self_trait_ty = TTraitType(
            tr,
            info.generics
                .iter()
                .map(|g| TType::Skolem(*g).intern(self.ctx))
                .collect(),
        )
        .intern(self.ctx);

        for (TRestriction(ty, trait_ty), (t, tr)) in self
            .initialize_restrictions(&info.restrictions, &btreemap! {})?
            .into_iter()
            .zip(&info.restrictions)
        {
            self.check_object_safety_ty(
                ty,
                self_ty,
                self_trait_ty,
                t.lookup(self.ctx).span,
                use_span,
            )?;
            self.check_object_safety_trait_ty_with_bindings(
                &trait_ty,
                self_ty,
                self_trait_ty,
                tr.lookup(self.ctx).span,
                use_span,
            )?;
        }

        for trait_tys in info.types.values() {
            for trait_ty in trait_tys {
                let trait_ty_span = trait_ty.lookup(self.ctx).span;
                let trait_ty = self.initialize_trait_ty_with_bindings(*trait_ty, &btreemap! {})?;
                self.check_object_safety_trait_ty_with_bindings(
                    &trait_ty,
                    self_ty,
                    self_trait_ty,
                    trait_ty_span,
                    use_span,
                )?;
            }
        }

        for method in info.methods.values() {
            let mut is_concrete = false;

            // This method is object-safe if it has `Self: Concrete`
            for (ty, restriction) in &method.restrictions {
                debug!("self_skolem={:?}, ty = {:?}, restriction = {:?}", Pretty(info.self_skolem, self.ctx), Pretty(ty, self.ctx), Pretty(restriction, self.ctx));

                match (&ty.lookup(self.ctx).data, &restriction.lookup(self.ctx)) {
                    (LTypeData::SelfSkolem(self_skolem), trait_ty)
                        if self_skolem.id == info.self_skolem.id
                            && trait_ty.tr.get(self.ctx) == self.ctx.lower_concrete_item()? =>
                    {
                        is_concrete = true;
                        break;
                    }
                    _ => { /* Do nothing */ },
                }
            }

            if is_concrete {
                continue;
            }

            if !method.has_self || !method.generics.is_empty() {
                return Err(AError::NotObjectSafeMethod {
                    trait_name: info.name,
                    method_name: method.name,
                    method_span: method.span,
                    use_span,
                });
            }

            for param in &method.parameters[1..] {
                let ty = self.initialize_ty(param.ty, &btreemap! {})?;
                self.check_object_safety_ty(ty, self_ty, self_trait_ty, param.span, use_span)?;
            }

            for (TRestriction(ty, trait_ty), (t, tr)) in self
                .initialize_restrictions(&info.restrictions, &btreemap! {})?
                .into_iter()
                .zip(&info.restrictions)
            {
                self.check_object_safety_ty(
                    ty,
                    self_ty,
                    self_trait_ty,
                    t.lookup(self.ctx).span,
                    use_span,
                )?;
                self.check_object_safety_trait_ty_with_bindings(
                    &trait_ty,
                    self_ty,
                    self_trait_ty,
                    tr.lookup(self.ctx).span,
                    use_span,
                )?;
            }

            let return_ty = self.initialize_ty(method.return_ty, &btreemap! {})?;
            self.check_object_safety_ty(
                return_ty,
                self_ty,
                self_trait_ty,
                method.return_ty.lookup(self.ctx).span,
                use_span,
            )?;
        }

        self.object_safe_traits.insert(tr);

        Ok(())
    }

    fn check_object_safety_ty(
        &mut self,
        ty: Id<TType>,
        self_ty: Id<TType>,
        self_trait_ty: Id<TTraitType>,
        def_span: Span,
        use_span: Span,
    ) -> AResult<()> {
        match &*ty.lookup(self.ctx) {
            TType::MethodSkolem(_, _)
            | TType::GenericInfer(_)
            | TType::Infer(_)
            | TType::ObjectAccess(_, _)
            | TType::Associated(_, None, _) => unreachable!(),
            TType::Associated(ty, Some(trait_ty), _)
            | TType::AssociatedSkolem(_, ty, trait_ty, _)
                if *ty == self_ty && *trait_ty == self_trait_ty =>
            { /* Always object-safe */ }
            TType::Skolem(_)
            | TType::Int
            | TType::Float
            | TType::Char
            | TType::Bool
            | TType::String
            | TType::Never => { /* Always object-safe */ },
            TType::Array(e) => {
                self.check_object_safety_ty(*e, self_ty, self_trait_ty, def_span, use_span)?;
            },
            TType::Tuple(es) => {
                self.check_object_safety_tys(&es, self_ty, self_trait_ty, def_span, use_span)?;
            },
            TType::Closure(ps, r) | TType::FnPtr(ps, r) => {
                self.check_object_safety_tys(&ps, self_ty, self_trait_ty, def_span, use_span)?;
                self.check_object_safety_ty(*r, self_ty, self_trait_ty, def_span, use_span)?;
            },
            TType::Dynamic(tr) => {
                self.check_object_safety_trait_ty_with_bindings(
                    tr,
                    self_ty,
                    self_trait_ty,
                    def_span,
                    use_span,
                )?;
            },
            TType::Object(_, gs) | TType::Enum(_, gs) => {
                self.check_object_safety_tys(&gs, self_ty, self_trait_ty, def_span, use_span)?;
            },
            TType::Associated(..) | TType::AssociatedSkolem(..) => {
                let trait_info = self_trait_ty.lookup(self.ctx).0.lookup(self.ctx);
                return Err(AError::NotObjectSafeType {
                    trait_name: trait_info.name,
                    ty,
                    def_span,
                    use_span,
                });
            },
        }

        Ok(())
    }

    fn check_object_safety_tys(
        &mut self,
        tys: &[Id<TType>],
        self_ty: Id<TType>,
        self_trait_ty: Id<TTraitType>,
        def_span: Span,
        use_span: Span,
    ) -> AResult<()> {
        tys.iter().try_for_each(|ty| {
            self.check_object_safety_ty(*ty, self_ty, self_trait_ty, def_span, use_span)
        })
    }

    fn check_object_safety_trait_ty_with_bindings(
        &mut self,
        TTraitTypeWithBindings(trait_ty, bindings): &TTraitTypeWithBindings,
        self_ty: Id<TType>,
        self_trait_ty: Id<TTraitType>,
        def_span: Span,
        use_span: Span,
    ) -> AResult<()> {
        for ty in &trait_ty.lookup(self.ctx).1 {
            self.check_object_safety_ty(*ty, self_ty, self_trait_ty, def_span, use_span)?;
        }

        for ty in bindings.values() {
            self.check_object_safety_ty(*ty, self_ty, self_trait_ty, def_span, use_span)?;
        }

        Ok(())
    }

    fn fresh_infer_ty(&mut self, span: Span) -> Id<TType> {
        let id = fresh_id();
        self.infer_spans.insert(id, span);
        TType::Infer(id).intern(self.ctx)
    }
}

fn merge_solution(
    is_concrete: bool,
    old_solution_and_epoch: &mut Option<(TImplWitness, TEpoch)>,
    new_solution: TImplWitness,
    new_epoch: TEpoch,
) -> AResult<bool> {
    match old_solution_and_epoch {
        Some((old_solution, _)) =>
            if is_concrete {
                Err(AError::ConflictingSolutions {
                    solution: old_solution.clone(),
                    other_solution: new_solution,
                })
            } else {
                return Ok(false);
            },
        _ => {
            *old_solution_and_epoch = Some((new_solution, new_epoch));

            Ok(true)
        },
    }
}
