mod expr;
mod facts;
mod impls;
mod instantiate;
mod item;
mod solve;
mod ty;

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::Arc,
};

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    lowering::{GenericId, InferId, LExpression, LModule, LTrait, LType, LoopId, VariableId},
    util::{AError, AResult, Id, Intern, TryCollectBTreeMap, TryCollectHashMap},
};

use self::{facts::TFacts, solve::TProvider};
pub use impls::{get_impls_for_trait, get_inherent_impls, get_traits_accessible_in_module};
pub use solve::TImplWitness;
pub use ty::{TTraitType, TTraitTypeWithBindings, TType};

pub fn typecheck_program_result(ctx: &dyn AdelaideContext) -> AResult<()> {
    let mut t = Typechecker::new(ctx);

    t.initialize_facts()?;

    for m in &*ctx.lower_mods()? {
        t.typecheck_module(*m)?;
    }

    Ok(())
}

pub fn typecheck_program(ctx: &dyn AdelaideContext) -> AResult<Typechecker> {
    let mut t = Typechecker::new(ctx);

    t.initialize_facts()?;

    for m in &*ctx.lower_mods()? {
        t.typecheck_module(*m)?;
    }

    Ok(t)
}

pub struct Typechecker<'ctx> {
    ctx: &'ctx dyn AdelaideContext,
    /// The current module we're typechecking inside
    module: Option<Id<LModule>>,
    /// The "facts" of the program, currently consisting of assumptions made by
    /// the program.
    type_facts: Option<Arc<TFacts>>,
    /// Global substitutions that apply during monomorphization
    global_substitutions: Option<BTreeMap<GenericId, Id<TType>>>,

    // -- Facts that we learn from the generic program, that we can share for later -- //
    object_safe_traits: HashMap<Id<LTrait>, Arc<HashMap<Id<str>, usize>>>,
    assoc_traits: HashMap<Id<LType>, TProvider>,
    method_traits: HashMap<Id<LExpression>, TProvider>,

    /// Span from which an inference originates, for error checking purposes
    infer_spans: HashMap<InferId, Span>,
    /// Variables which are used for typechecking, known to exist
    variables: HashMap<VariableId, Id<TType>>,
    /// Return types which are used for typechecking, known to exist
    return_tys: Vec<(Id<TType>, Span)>,
    /// Yield types which are used for typechecking, known to exist
    yield_tys: Vec<(Id<TType>, Id<TType>, Span)>,
    /// Loop "return" types which are used for typechecking, known to exist
    loop_tys: HashMap<LoopId, (Id<TType>, Span)>,

    // Epochs of nested type assumptions that are used for complex type inference when traits are
    // involved
    epochs: Vec<TEpoch>,

    solved: HashMap<TGoal, (TImplWitness, Span)>,
    function_generics: HashMap<Id<LExpression>, Vec<Id<TType>>>,
    trait_generics: HashMap<Id<LExpression>, Vec<Id<TType>>>,
}

impl<'ctx> Typechecker<'ctx> {
    fn new(ctx: &'ctx dyn AdelaideContext) -> Self {
        Typechecker {
            ctx,
            module: None,
            type_facts: None,
            global_substitutions: None,

            object_safe_traits: hashmap! {},
            assoc_traits: hashmap! {},
            method_traits: hashmap! {},

            infer_spans: hashmap! {},
            variables: hashmap! {},
            return_tys: vec![],
            yield_tys: vec![],
            loop_tys: hashmap! {},

            epochs: vec![],

            solved: hashmap! {},
            function_generics: hashmap! {},
            trait_generics: hashmap! {},
        }
    }

    pub fn new_concrete(
        parent: &Typechecker<'ctx>,
        substitutions: BTreeMap<GenericId, Id<TType>>,
    ) -> Self {
        Typechecker {
            ctx: parent.ctx,
            module: None,
            type_facts: Some(Arc::new(TFacts::empty())),
            global_substitutions: Some(substitutions),

            object_safe_traits: parent.object_safe_traits.clone(),
            assoc_traits: parent.assoc_traits.clone(),
            method_traits: parent.method_traits.clone(),

            infer_spans: hashmap! {},
            variables: hashmap! {},
            return_tys: vec![],
            yield_tys: vec![],
            loop_tys: hashmap! {},

            epochs: vec![TEpoch {
                goal: TGoal::Monomorphization,
                infers: hashmap! {},
                never_candidates: hashset! {},
                ambiguity: None,
                progress: false,
            }],

            solved: hashmap! {},
            function_generics: hashmap! {},
            trait_generics: hashmap! {},
        }
    }

    fn typecheck_loop(&mut self, inner: impl Fn(&mut Self) -> AResult<()>) -> AResult<()> {
        loop {
            let epoch = self.epoch();
            epoch.progress = false;
            epoch.ambiguity = None;

            inner(self)?;

            self.normalize_typechecker()?;

            let ctx = self.ctx;
            let epoch = self.epoch();

            if epoch.progress {
                // Do nothing
            } else if let Some(ambiguity) = std::mem::take(&mut epoch.ambiguity) {
                let mut progress = false;

                for id in std::mem::take(&mut epoch.never_candidates) {
                    if !epoch.infers.contains_key(&id) {
                        progress = true;
                        epoch.infers.insert(id, ctx.static_ty(&TType::Never));
                    }
                }

                if !progress {
                    debug!("No progress, therefore bailing...");
                    return Err(ambiguity);
                }
            } else {
                return Ok(());
            }
        }
    }

    fn normalize_typechecker(&mut self) -> AResult<()> {
        let mut solved = hashmap! {};
        for (goal, (witness, span)) in self.solved.clone() {
            let goal = match goal {
                TGoal::TheProgram => TGoal::TheProgram,
                TGoal::Monomorphization => TGoal::Monomorphization,
                TGoal::Implements(ty, trait_ty) => TGoal::Implements(
                    self.normalize_ty(ty, span)?,
                    self.normalize_trait_ty(trait_ty, span)?,
                ),
                TGoal::Method(e) => TGoal::Method(e),
                TGoal::AssociatedType(t) => TGoal::AssociatedType(t),
            };

            let witness = match witness {
                TImplWitness::Impl(i, bindings) => TImplWitness::Impl(
                    i,
                    bindings
                        .into_iter()
                        .map(|(n, ty)| -> AResult<_> { Ok((n, self.normalize_ty(ty, span)?)) })
                        .try_collect_btreemap()?,
                ),
                TImplWitness::Assumption(tr, ty, trait_ty) => TImplWitness::Assumption(
                    tr,
                    self.normalize_ty(ty, span)?,
                    self.normalize_trait_ty(trait_ty, span)?,
                ),
                TImplWitness::Dynamic(ty, trait_ty) => {
                    let TTraitTypeWithBindings(trait_ty, bindings) = &*trait_ty.lookup(self.ctx);

                    TImplWitness::Dynamic(
                        self.normalize_ty(ty, span)?,
                        TTraitTypeWithBindings(
                            self.normalize_trait_ty(*trait_ty, span)?,
                            bindings
                                .iter()
                                .map(|(n, ty)| AResult::Ok((*n, self.normalize_ty(*ty, span)?)))
                                .try_collect_btreemap()?,
                        )
                        .intern(self.ctx),
                    )
                },
                TImplWitness::DynamicCoersion(ty, trait_ty) => {
                    let TTraitTypeWithBindings(trait_ty, bindings) = &*trait_ty.lookup(self.ctx);
                    TImplWitness::DynamicCoersion(
                        self.normalize_ty(ty, span)?,
                        TTraitTypeWithBindings(
                            self.normalize_trait_ty(*trait_ty, span)?,
                            bindings
                                .iter()
                                .map(|(n, ty)| -> AResult<_> {
                                    Ok((*n, self.normalize_ty(*ty, span)?))
                                })
                                .try_collect_btreemap()?,
                        )
                        .intern(self.ctx),
                    )
                },
                TImplWitness::Concrete => TImplWitness::Concrete,
            };

            if let Some((other_witness, _)) = solved.get(&goal) {
                if witness != *other_witness {
                    return Err(AError::ConflictingSolutions {
                        solution: witness,
                        other_solution: other_witness.clone(),
                    });
                }
            }

            solved.insert(goal, (witness, span));
        }
        self.solved = solved;

        let function_generics = self
            .function_generics
            .clone()
            .into_iter()
            .map(|(key, tys)| -> AResult<_> {
                Ok((key, self.normalize_tys(&tys, key.lookup(self.ctx).span)?))
            })
            .try_collect_hashmap()?;
        self.function_generics = function_generics;

        let trait_generics = self
            .trait_generics
            .clone()
            .into_iter()
            .map(|(key, tys)| -> AResult<_> {
                Ok((key, self.normalize_tys(&tys, key.lookup(self.ctx).span)?))
            })
            .try_collect_hashmap()?;
        self.trait_generics = trait_generics;

        Ok(())
    }

    fn in_epoch<T>(
        &mut self,
        goal: TGoal,
        inner: impl FnOnce(&mut Self) -> AResult<T>,
    ) -> AResult<(T, TEpoch)> {
        self.push_epoch(goal);

        let t = inner(self);
        let epoch = self.pop_epoch();

        Ok((t?, epoch))
    }

    pub fn typecheck_loop_then_commit<T>(
        &mut self,
        goal: TGoal,
        typechecking: impl Fn(&mut Self) -> AResult<()>,
        result: impl FnOnce(&mut Self) -> AResult<T>,
    ) -> AResult<T> {
        self.push_epoch(goal);

        self.typecheck_loop(typechecking)?;
        let t = result(self);

        let epoch = self.pop_epoch();
        self.commit_epoch(epoch);

        Ok(t?)
    }

    fn within_goal(&self, goal: &TGoal) -> bool {
        // Reversing it since it's likely the goal on top is the one we're looking
        // for... just a premature optimization
        self.epochs.iter().rev().any(|epoch| epoch.goal == *goal)
    }

    fn facts(&self) -> Arc<TFacts> {
        self.type_facts.clone().unwrap()
    }

    fn epoch(&mut self) -> &mut TEpoch {
        self.epochs.last_mut().unwrap()
    }

    fn push_epoch(&mut self, goal: TGoal) {
        self.epochs.push(TEpoch {
            goal,
            infers: hashmap! {},
            never_candidates: hashset! {},
            ambiguity: None,
            progress: false,
        });
    }

    fn pop_epoch(&mut self) -> TEpoch {
        self.epochs.pop().unwrap()
    }

    fn commit_epoch(&mut self, epoch: TEpoch) {
        assert!(epoch.ambiguity.is_none());

        let last_epoch = self.epoch();

        last_epoch.infers.extend(epoch.infers);
        last_epoch.never_candidates.extend(epoch.never_candidates);
    }

    fn set_ambiguity(&mut self, e: AError) {
        match &mut self.epoch().ambiguity {
            n @ None => {
                *n = Some(e);
            },
            _ => {},
        }
    }
}

pub struct TEpoch {
    goal: TGoal,
    infers: HashMap<InferId, Id<TType>>,
    never_candidates: HashSet<InferId>,
    ambiguity: Option<AError>,
    progress: bool,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PrettyPrint)]
pub enum TGoal {
    TheProgram,
    Monomorphization,
    Implements(Id<TType>, Id<TTraitType>),
    Method(Id<LExpression>),
    AssociatedType(Id<LType>),
}
