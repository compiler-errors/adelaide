mod expr;
mod pattern;
mod ty;

use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    lowering::{LFunction, LGlobal, LImpl, LModule, LTrait},
    typechecker::{typecheck_program, TGoal, TImplWitness, TTraitType, TType, Typechecker},
    util::{AError, AResult, Id, Pretty, TryCollectVec, ZipExact},
};

pub use self::{
    expr::{CExpression, CLiteral, CStackId, CStatement},
    pattern::CPattern,
    ty::CType,
};

pub fn translate_program(ctx: &dyn AdelaideContext) -> AResult<()> {
    let alloc = Bump::new();

    let typechecked = Typechecker::new_concrete(&typecheck_program(ctx)?, btreemap! {});
    let translator = Translator::new(ctx, typechecked, &alloc);
    translator.translate_program()?;

    Ok(())
}

pub struct Translator<'ctx, 'a> {
    ctx: &'ctx dyn AdelaideContext,
    typechecker: Typechecker<'ctx>,
    alloc: &'a Bump,

    strings: HashMap<Id<str>, &'a str>,
    tys: HashMap<Id<TType>, CType<'a>>,
    functions: HashMap<CFunctionId<'a>, Option<CFunction<'a>>>,
    globals: HashMap<Id<LGlobal>, Option<CGlobal<'a>>>,

    type_strings: HashMap<CType<'a>, &'a str>,
    type_ids: HashMap<CType<'a>, usize>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CFunctionId<'a> {
    Function(Id<LFunction>, &'a [CType<'a>]),
    Method(Id<LImpl>, &'a [CType<'a>], Id<str>, &'a [CType<'a>]),
    Dispatch(Id<LTrait>, &'a [CType<'a>], Id<str>),
    DynamicBox(CType<'a>, Id<LTrait>, &'a [CType<'a>]),
}

pub struct CProgram<'a> {
    pub pre_main: CExpression<'a>,
    pub functions: HashMap<CFunctionId<'a>, CFunction<'a>>,
    pub globals: HashMap<Id<LGlobal>, CGlobal<'a>>,
    pub type_strings: HashMap<CType<'a>, &'a str>,
    pub type_ids: HashMap<CType<'a>, usize>,
}

#[derive(Debug, Copy, Clone)]
pub enum CFunction<'a> {
    Simple {
        slots: usize,
        parameters: &'a [CPattern<'a>],
        body: CExpression<'a>,
    },
    // Box up the given value alongside the vtable provided
    DynamicBox(CVTable<'a>),
    // Dispatch to a function in the vtable slot at the given index
    Dispatch(usize),
    // Dispatch to an extern function
    Extern(&'a str, &'a [CType<'a>]),
}

#[derive(Debug, Copy, Clone)]
pub struct CGlobal<'a> {
    pub name: &'a str,
    pub name_id: Id<str>,
    pub span: Span,
    pub slots: usize,
    pub body: CExpression<'a>,
}

#[derive(Debug, Copy, Clone)]
pub struct CVTable<'a>(pub &'a [CFunctionId<'a>]);

impl<'ctx, 'a> Translator<'ctx, 'a> {
    pub fn new(
        ctx: &'ctx dyn AdelaideContext,
        typechecker: Typechecker<'ctx>,
        alloc: &'a Bump,
    ) -> Self {
        Translator {
            ctx,
            typechecker,
            alloc,

            strings: hashmap! {},
            tys: hashmap! {},
            functions: hashmap! {},
            globals: hashmap! {},

            type_strings: hashmap! {},
            type_ids: hashmap! {},
        }
    }

    pub fn translate_program(mut self) -> AResult<&'a CProgram<'a>> {
        let main = self
            .find_main(self.ctx.lower_program()?)?
            .ok_or(AError::MissingMain)?;

        let pre_main = self.translate_main(main)?;

        Ok(self.alloc.alloc_with(|| CProgram {
            pre_main,
            functions: self
                .functions
                .into_iter()
                .map(|(id, f)| (id, f.unwrap()))
                .collect(),
            globals: self
                .globals
                .into_iter()
                .map(|(id, g)| (id, g.unwrap()))
                .collect(),
            type_strings: self.type_strings,
            type_ids: self.type_ids,
        }))
    }

    fn find_main(&self, m: Id<LModule>) -> AResult<Option<Id<LFunction>>> {
        let mut main: Option<Id<LFunction>> = None;

        for f in m.lookup(self.ctx).functions.values() {
            let info = f.lookup(self.ctx);

            if info.name == self.ctx.static_name("main") {
                if let Some(other_main) = main {
                    return Err(AError::ConflictingMain {
                        span: info.span,
                        other_span: other_main.lookup(self.ctx).span,
                    });
                }

                // The signature of main should be `fn main() -> ()`.
                if !info.generics.is_empty() || !info.parameters.is_empty() {
                    return Err(AError::BadMainSignature { span: info.span });
                }

                main = Some(*f);
            }
        }

        for m in m.lookup(self.ctx).modules.values() {
            match (&mut main, self.find_main(*m)?) {
                (Some(main), Some(other_main)) => {
                    return Err(AError::ConflictingMain {
                        span: main.lookup(self.ctx).span,
                        other_span: other_main.lookup(self.ctx).span,
                    });
                },
                (main @ &mut None, Some(other_main)) => {
                    *main = Some(other_main);
                },
                _ => { /* No main found in this module or its submodules */ },
            }
        }

        Ok(main)
    }

    fn translate_main(&mut self, id: Id<LFunction>) -> AResult<CExpression<'a>> {
        let main = self.translate_function(id, &[])?;

        Typechecker::new_concrete(&self.typechecker, btreemap! {}).typecheck_loop_then_commit(
            TGoal::TheProgram,
            |tyck| {
                tyck.do_goal_main(id)?;
                Ok(())
            },
            |tyck| {
                let witness = tyck.do_goal_main(id)?;
                let finally = self.translate_method(
                    &witness,
                    self.ctx.static_name("finally"),
                    &[],
                    id.lookup(self.ctx).span,
                )?;

                Ok(CExpression::Call(
                    finally,
                    self.alloc.alloc_slice_copy(&[CExpression::Call(main, &[])]),
                ))
            },
        )
    }

    fn translate_global(&mut self, id: Id<LGlobal>) -> AResult<()> {
        if !self.globals.contains_key(&id) {
            debug!(
                "Translating global {:?}",
                Pretty(id.lookup(self.ctx).name, self.ctx)
            );

            self.globals.insert(id, None);

            let info = id.lookup(self.ctx);

            let slots: HashMap<_, _> = info
                .vcx
                .variables
                .keys()
                .enumerate()
                .map(|(idx, id)| (*id, CStackId(idx)))
                .collect();

            let mut tyck = Typechecker::new_concrete(&self.typechecker, btreemap! {});
            let global = tyck.typecheck_loop_then_commit(
                TGoal::TheProgram,
                |tyck| tyck.typecheck_global(id),
                |tyck| {
                    Ok(CGlobal {
                        name: self.deintern_string(info.name),
                        name_id: info.name,
                        span: info.span,
                        slots: slots.len(),
                        body: self.translate_expr(info.expr, tyck, &slots)?,
                    })
                },
            )?;

            self.globals.insert(id, Some(global));
        }

        Ok(())
    }

    fn translate_function(
        &mut self,
        fun: Id<LFunction>,
        generics: &'a [CType<'a>],
    ) -> AResult<CFunctionId<'a>> {
        let id = CFunctionId::Function(fun, generics);

        if !self.functions.contains_key(&id) {
            self.functions.insert(id, None);

            let info = fun.lookup(self.ctx);

            let substitutions = info
                .generics
                .iter()
                .zip_exact(generics)
                .map(|(g, t)| (g.id, self.into_ttype(*t)))
                .collect();

            let slots: HashMap<_, _> = info
                .vcx
                .variables
                .keys()
                .enumerate()
                .map(|(idx, id)| (*id, CStackId(idx)))
                .collect();

            let mut tyck = Typechecker::new_concrete(&self.typechecker, substitutions);
            let fun = tyck.typecheck_loop_then_commit(
                TGoal::TheProgram,
                |tyck| tyck.typecheck_function(fun),
                |tyck| {
                    if let Some(body) = info.body {
                        Ok(CFunction::Simple {
                            slots: slots.len(),
                            parameters: self.alloc.alloc_slice_fill_iter(
                                info.parameters
                                    .iter()
                                    .map(|param| CPattern::Variable(slots[&param.id])),
                            ),
                            body: self.translate_expr(body, tyck, &slots)?,
                        })
                    } else {
                        Ok(CFunction::Extern(self.deintern_string(info.name), generics))
                    }
                },
            )?;

            self.functions.insert(id, Some(fun));
        }

        Ok(id)
    }

    fn translate_impl_method(
        &mut self,
        imp: Id<LImpl>,
        impl_generics: &'a [CType<'a>],
        name: Id<str>,
        fn_generics: &'a [CType<'a>],
    ) -> AResult<CFunctionId<'a>> {
        let id = CFunctionId::Method(imp, impl_generics, name, fn_generics);

        if !self.functions.contains_key(&id) {
            self.functions.insert(id, None);

            let impl_info = imp.lookup(self.ctx);
            let method_info = &impl_info.methods[&name];

            let substitutions = Iterator::chain(
                impl_info.generics.iter().zip_exact(impl_generics),
                method_info.generics.iter().zip_exact(fn_generics),
            )
            .map(|(g, t)| (g.id, self.into_ttype(*t)))
            .collect();

            debug!(
                "Translating an impl method {:?}, subs = {:?}",
                Pretty(name, self.ctx),
                Pretty(&substitutions, self.ctx)
            );

            let slots: HashMap<_, _> = method_info
                .vcx
                .variables
                .keys()
                .enumerate()
                .map(|(idx, id)| (*id, CStackId(idx)))
                .collect();

            debug!("SLOTS = {:?}", slots);

            let mut tyck = Typechecker::new_concrete(&self.typechecker, substitutions);
            let fun = tyck.typecheck_loop_then_commit(
                TGoal::TheProgram,
                |tyck| tyck.typecheck_method_oneoff(imp, name),
                |tyck| {
                    Ok(CFunction::Simple {
                        slots: slots.len(),
                        parameters: self.alloc.alloc_slice_fill_iter(
                            method_info
                                .parameters
                                .iter()
                                .map(|param| CPattern::Variable(slots[&param.id])),
                        ),
                        body: self.translate_expr(method_info.body, tyck, &slots)?,
                    })
                },
            )?;

            self.functions.insert(id, Some(fun));
        }

        Ok(id)
    }

    fn translate_method(
        &mut self,
        witness: &TImplWitness,
        name: Id<str>,
        generics: &'a [CType<'a>],
        span: Span,
    ) -> AResult<CFunctionId<'a>> {
        match witness {
            TImplWitness::Impl(id, substitutions) => {
                let impl_generics = id
                    .lookup(self.ctx)
                    .generics
                    .iter()
                    .map(|g| self.translate_tty(substitutions[&g.id]))
                    .try_collect_vec()?;
                self.translate_impl_method(
                    *id,
                    self.alloc.alloc_slice_copy(&impl_generics),
                    name,
                    generics,
                )
            },
            TImplWitness::Dynamic(_, trait_ty) => {
                let trait_ty = trait_ty.lookup(self.ctx).0;
                let TTraitType(tr, generics) = &*trait_ty.lookup(self.ctx);

                let ctrait_generics = self.translate_sub_ttys(&generics)?;
                let id = CFunctionId::Dispatch(*tr, ctrait_generics, name);

                if !self.functions.contains_key(&id) {
                    let ordering = self.typechecker.do_goal_object_safety(*tr, span)?;
                    self.functions
                        .insert(id, Some(CFunction::Dispatch(ordering[&name])));
                }

                Ok(id)
            },
            TImplWitness::DynamicCoersion(ty, trait_ty) => {
                let trait_ty = trait_ty.lookup(self.ctx).0;
                let TTraitType(tr, generics) = &*trait_ty.lookup(self.ctx);

                let cty = self.translate_tty(*ty)?;
                let ctrait_generics = self.translate_sub_ttys(&generics)?;
                let id = CFunctionId::DynamicBox(cty, *tr, ctrait_generics);

                if !self.functions.contains_key(&id) {
                    self.functions.insert(id, None);

                    let witness = self
                        .typechecker
                        .do_goal_trait(*ty, span, trait_ty, span, false)?
                        .expect("Expected an impl");
                    let vtable = self.translate_vtable(*tr, &witness, span)?;

                    self.functions
                        .insert(id, Some(CFunction::DynamicBox(vtable)));
                }

                Ok(id)
            },
            TImplWitness::Assumption(_, _, _) | TImplWitness::Concrete => unreachable!(),
        }
    }

    fn translate_vtable(
        &mut self,
        tr: Id<LTrait>,
        witness: &TImplWitness,
        span: Span,
    ) -> AResult<CVTable<'a>> {
        let methods = self.typechecker.do_goal_object_safety(tr, span)?;

        let mut table = vec![None; methods.len()];
        for (name, idx) in &*methods {
            table[*idx] = Some(self.translate_method(witness, *name, &[], span)?);
        }

        Ok(CVTable(self.alloc.alloc_slice_fill_iter(
            table.into_iter().map(Option::unwrap),
        )))
    }

    fn deintern_string(&mut self, s: Id<str>) -> &'a str {
        if let Some(s) = self.strings.get(&s) {
            *s
        } else {
            let a = self.alloc.alloc_str(&*s.lookup(self.ctx));
            self.strings.insert(s, a);
            a
        }
    }
}
