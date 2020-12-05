mod expr;
mod facts;
mod impls;
mod instantiate;
mod item;
mod solve;
mod ty;

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    lowering::{InferId, LExpression, LModule, LoopId, VariableId},
    util::{AError, AResult, Id, TryCollectBTreeMap, TryCollectHashMap},
};

use self::facts::TFacts;
pub use impls::{get_impls_for_trait, get_inherent_impls, get_traits_accessible_in_module};
pub use solve::TImplWitness;
use ty::TTraitTypeWithBindings;
pub use ty::{TTraitType, TType};

pub fn typecheck_program(ctx: &dyn AdelaideContext) -> AResult<()> {
    let mut t = Typechecker::new(ctx);

    t.initialize_facts()?;

    for m in &*ctx.lower_mods()? {
        t.typecheck_module(*m)?;
    }

    Ok(())
}

struct Typechecker<'ctx> {
    ctx: &'ctx dyn AdelaideContext,
    module: Option<Id<LModule>>,
    type_facts: Option<Arc<TFacts>>,

    infer_spans: HashMap<InferId, Span>,
    variables: HashMap<VariableId, Id<TType>>,
    return_tys: Vec<(Id<TType>, Span)>,
    loop_tys: HashMap<LoopId, (Id<TType>, Span)>,

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

            infer_spans: hashmap! {},
            variables: hashmap! {},
            return_tys: vec![],
            loop_tys: hashmap! {},

            epochs: vec![],
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
                TGoal::Implements(ty, trait_ty) => TGoal::Implements(
                    self.normalize_ty(ty, span)?,
                    self.normalize_trait_ty(trait_ty, span)?,
                ),
                TGoal::Method(e) => TGoal::Method(e),
                TGoal::AssociatedType(m, ty, n) =>
                    TGoal::AssociatedType(m, self.normalize_ty(ty, span)?, n),
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
                TImplWitness::Dynamic(ty, TTraitTypeWithBindings(trait_ty, bindings)) =>
                    TImplWitness::Dynamic(
                        self.normalize_ty(ty, span)?,
                        TTraitTypeWithBindings(
                            self.normalize_trait_ty(trait_ty, span)?,
                            bindings
                                .into_iter()
                                .map(|(n, ty)| AResult::Ok((n, self.normalize_ty(ty, span)?)))
                                .try_collect_btreemap()?,
                        ),
                    ),
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

        Ok(())
    }

    fn in_epoch<T>(
        &mut self,
        goal: TGoal,
        inner: impl Fn(&mut Self) -> AResult<T>,
    ) -> AResult<(T, TEpoch)> {
        self.push_epoch(goal);

        let t = inner(self);
        let epoch = self.pop_epoch();

        Ok((t?, epoch))
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

struct TEpoch {
    goal: TGoal,
    infers: HashMap<InferId, Id<TType>>,
    never_candidates: HashSet<InferId>,
    ambiguity: Option<AError>,
    progress: bool,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PrettyPrint)]
enum TGoal {
    TheProgram,
    Implements(Id<TType>, Id<TTraitType>),
    Method(Id<LExpression>),
    AssociatedType(Id<LModule>, Id<TType>, Id<str>),
}
