use std::{
    collections::{BTreeMap, HashMap},
    iter::once,
    sync::Arc,
};

use crate::{
    lexer::Span,
    lowering::{fresh_id, LModule, LTrait},
    util::{AResult, Id, Intern, Pretty, TryCollectBTreeMap, ZipExact},
};

use super::{
    item::TRestriction,
    ty::{TTraitTypeWithBindings, UnifyMode},
    TGoal, TTraitType, TType, Typechecker,
};

#[derive(Debug, Clone, PartialEq, Eq, PrettyPrint)]
pub struct TFacts {
    pub assumptions: HashMap<
        // Type implements trait (with LTrait key extracted for easy comparison)
        (Id<LTrait>, Id<TType>, Id<TTraitType>),
        // Binding types (for assoc ty) + spans for ty and trait_ty of declaration
        (BTreeMap<Id<str>, Id<TType>>, Span),
    >,
}

impl TFacts {
    pub fn empty() -> Self {
        TFacts {
            assumptions: hashmap! {},
        }
    }
}

impl Typechecker<'_> {
    pub fn initialize_facts(&mut self) -> AResult<()> {
        self.push_epoch(TGoal::TheProgram);

        debug!("Initializing facts");
        let mut new_facts = TFacts::empty();

        for m in &*self.ctx.lower_mods()? {
            self.initialize_module_facts(*m, &mut new_facts)?;
        }

        self.type_facts = Some(Arc::new(new_facts));
        debug!("Done nitializing facts");

        self.typecheck_loop(|this /* lol */| {
            this.type_facts = Some(this.normalize_facts()?);

            Ok(())
        })?;

        Ok(())
    }

    pub fn initialize_module_facts(
        &mut self,
        module: Id<LModule>,
        new_facts: &mut TFacts,
    ) -> AResult<()> {
        let LModule {
            source: _,
            parent: _,
            modules: _,
            globals: _, // No restrictions on globals
            functions,
            objects,
            enums,
            traits,
            impls,
        } = &*module.lookup(self.ctx);

        for t in traits.values() {
            let info = t.lookup(self.ctx);

            let self_skolem = TType::Skolem(info.self_skolem).intern(self.ctx);
            // Assume `Self: Trait<A, B, C..>`
            self.assume_restriction(
                new_facts,
                self_skolem,
                TTraitType(
                    *t,
                    info.generics
                        .iter()
                        .map(|g| TType::Skolem(*g).intern(self.ctx))
                        .collect(),
                )
                .intern(self.ctx),
                btreemap! {},
                info.self_skolem.span,
            )?;

            let restrictions = self.initialize_restrictions(&info.restrictions, &btreemap! {})?;
            // Assume the restrictions of the trait itself
            self.assume_restrictions(new_facts, restrictions, info.span)?;

            for method in info.methods.values() {
                let restrictions =
                    self.initialize_restrictions(&method.restrictions, &btreemap! {})?;
                self.assume_restrictions(new_facts, restrictions, method.span)?;
            }
        }

        for i in impls.values() {
            let info = i.lookup(self.ctx);

            let restrictions = self.initialize_restrictions(&info.restrictions, &btreemap! {})?;
            debug!("Impl\n:{:#?}\n", Pretty(i, self.ctx));
            debug!("Restrictions\n:{:?}\n\n", Pretty(&restrictions, self.ctx));
            // Assume the restrictions of the trait itself
            self.assume_restrictions(new_facts, restrictions, info.span)?;

            let self_ty = self.initialize_ty(info.ty, &btreemap! {})?;
            let trait_ty = info
                .trait_ty
                .map(|trait_ty| self.initialize_trait_ty(trait_ty, &btreemap! {}))
                .transpose()?;

            for method in info.methods.values() {
                let restrictions =
                    self.initialize_restrictions(&method.restrictions, &btreemap! {})?;

                self.assume_restrictions(new_facts, restrictions, method.span)?;

                // Assume the trait method's facts, but for a separate MethodSkolem so we don't
                // have conflicts
                if let Some(trait_ty) = trait_ty {
                    let trait_info = trait_ty.lookup(self.ctx).0.lookup(self.ctx);
                    let trait_method_info = &trait_info.methods[&method.name];

                    let substitutions = Iterator::chain(
                        // Instantiate the trait like normal
                        trait_info
                            .generics
                            .iter()
                            .zip_exact(trait_ty.lookup(self.ctx).1.iter())
                            .map(|(g, ty)| (g.id, *ty))
                            // Don't forget Self (:
                            .chain(once((trait_info.self_skolem.id, self_ty))),
                        // Method generics are mapped to MethodSkolems
                        trait_method_info
                            .generics
                            .iter()
                            .map(|g| (g.id, TType::MethodSkolem(*i, *g).intern(self.ctx))),
                    )
                    .collect();

                    let restrictions = self
                        .initialize_restrictions(&trait_method_info.restrictions, &substitutions)?;

                    self.assume_restrictions(new_facts, restrictions, method.span)?;
                }
            }
        }

        for f in functions.values() {
            let info = f.lookup(self.ctx);
            let restrictions = self.initialize_restrictions(&info.restrictions, &btreemap! {})?;
            self.assume_restrictions(new_facts, restrictions, info.span)?;
        }

        for o in objects.values() {
            let info = o.lookup(self.ctx);
            let restrictions = self.initialize_restrictions(&info.restrictions, &btreemap! {})?;
            self.assume_restrictions(new_facts, restrictions, info.span)?;
        }

        for e in enums.values() {
            let info = e.lookup(self.ctx);
            let restrictions = self.initialize_restrictions(&info.restrictions, &btreemap! {})?;
            self.assume_restrictions(new_facts, restrictions, info.span)?;
        }
        Ok(())
    }

    fn assume_restriction(
        &mut self,
        new_facts: &mut TFacts,
        ty: Id<TType>,
        trait_ty: Id<TTraitType>,
        bindings: BTreeMap<Id<str>, Id<TType>>,
        span: Span,
    ) -> AResult<()> {
        debug!(
            "Assuming that {:?} :- {:?} (bindings: {:?})",
            Pretty(ty, self.ctx),
            Pretty(trait_ty, self.ctx),
            Pretty(&bindings, self.ctx),
        );

        let tr = trait_ty.lookup(self.ctx).0;

        let mut bindings = if let Some((old_bindings, old_span)) =
            new_facts.assumptions.get(&(tr, ty, trait_ty))
        {
            // Take the old bindings, normalize them
            let mut new_bindings = old_bindings.clone();

            for (n, ty) in bindings {
                let old_ty = old_bindings[&n];

                // Merge the bindings in Skolem mode.
                new_bindings.insert(
                    n,
                    self.unify_ty(UnifyMode::SkolemInference, old_ty, *old_span, ty, span)?,
                );
            }

            new_bindings
        } else {
            bindings
                .iter()
                .map(|(n, ty)| AResult::Ok((*n, self.normalize_ty(*ty, span)?)))
                .try_collect_btreemap()?
        };

        let tr_info = tr.lookup(self.ctx);

        for (n, restrictions) in &tr_info.types {
            if !bindings.contains_key(n) {
                let skolem_ty =
                    TType::AssociatedSkolem(fresh_id(), ty, trait_ty, *n).intern(self.ctx);

                debug!(
                    "Because our bindings {:?} are missing {:?}, adding as skolem {:?}",
                    Pretty(&bindings, self.ctx),
                    Pretty(n, self.ctx),
                    Pretty(skolem_ty, self.ctx)
                );

                bindings.insert(*n, skolem_ty);

                // Assume restrictions for non-bound associated types. i.e.
                // if we have `Ty: Iterable`, we should also assume that
                // `<Ty as Iterable>::Iterator: Iterator`.
                for restriction in restrictions {
                    let TTraitTypeWithBindings(trait_ty, bindings) = &*self
                        .initialize_trait_ty_with_bindings(*restriction, &btreemap! {
                            tr_info.self_skolem.id => ty
                        })?
                        .lookup(self.ctx);

                    debug!(
                        "Because of binding {:?}, we have to  assume that {:?} :- {:?}",
                        Pretty(n, self.ctx),
                        Pretty(skolem_ty, self.ctx),
                        Pretty(trait_ty, self.ctx)
                    );
                    self.assume_restriction(
                        new_facts,
                        skolem_ty,
                        *trait_ty,
                        bindings.clone(),
                        span,
                    )?;
                }
            }
        }

        new_facts
            .assumptions
            .insert((tr, ty, trait_ty), (bindings, span));

        Ok(())
    }

    fn assume_restrictions(
        &mut self,
        new_facts: &mut TFacts,
        restrictions: Vec<TRestriction>,
        span: Span,
    ) -> AResult<()> {
        for TRestriction(ty, trait_ty) in restrictions {
            let TTraitTypeWithBindings(trait_ty, bindings) = &*trait_ty.lookup(self.ctx);
            self.assume_restriction(new_facts, ty, *trait_ty, bindings.clone(), span)?;
        }

        Ok(())
    }

    pub fn normalize_facts(&mut self) -> AResult<Arc<TFacts>> {
        // Unify things in Skolem mode
        let TFacts { assumptions } = &*self.facts();

        let mut new_facts = TFacts::empty();

        for ((tr, ty, trait_ty), (bindings, span)) in assumptions {
            let ty = self.normalize_ty(*ty, *span)?;
            let trait_ty = self.normalize_trait_ty(*trait_ty, *span)?;

            let bindings = if let Some((old_bindings, old_span)) =
                new_facts.assumptions.get(&(*tr, ty, trait_ty))
            {
                // Take the old bindings, normalize them
                let mut new_bindings = old_bindings.clone();

                for (n, ty) in bindings {
                    let old_ty = old_bindings[n];

                    // Merge the bindings in Skolem mode.
                    new_bindings.insert(
                        *n,
                        self.unify_ty(UnifyMode::SkolemInference, old_ty, *old_span, *ty, *span)?,
                    );
                }

                new_bindings
            } else {
                bindings
                    .iter()
                    .map(|(n, ty)| AResult::Ok((*n, self.normalize_ty(*ty, *span)?)))
                    .try_collect_btreemap()?
            };

            new_facts
                .assumptions
                .insert((*tr, ty, trait_ty), (bindings, *span));
        }

        Ok(Arc::new(new_facts))
    }
}
