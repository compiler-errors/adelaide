use std::collections::BTreeMap;

use crate::{
    lowering::{fresh_id, LEnum, LObject, LTrait, LType},
    typechecker::{TTraitType, TTraitTypeWithBindings, TType, Typechecker},
    util::{AResult, Id, Intern, Pretty, TryCollectVec, ZipExact},
};

use super::Translator;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CType<'a> {
    Int,
    Float,
    Char,
    Bool,
    String,
    Never,
    Array(&'a CType<'a>),
    Tuple(&'a [CType<'a>]),
    Closure(&'a [CType<'a>], &'a CType<'a>),
    FnPtr(&'a [CType<'a>], &'a CType<'a>),
    Object(Id<LObject>, &'a [CType<'a>]),
    Enum(Id<LEnum>, &'a [CType<'a>]),
    Dynamic(Id<LTrait>, &'a [CType<'a>]),
}

impl<'a> Translator<'_, 'a> {
    pub fn into_ttype(&self, ty: CType<'a>) -> Id<TType> {
        match ty {
            CType::Int => self.ctx.static_ty(&TType::Int),
            CType::Float => self.ctx.static_ty(&TType::Float),
            CType::Char => self.ctx.static_ty(&TType::Char),
            CType::Bool => self.ctx.static_ty(&TType::Bool),
            CType::String => self.ctx.static_ty(&TType::String),
            CType::Never => self.ctx.static_ty(&TType::Never),
            CType::Array(e) => TType::Array(self.into_ttype(*e)).intern(self.ctx),
            CType::Tuple(es) => TType::Tuple(self.into_ttypes(&es)).intern(self.ctx),
            CType::Closure(ps, r) =>
                TType::Closure(self.into_ttypes(&ps), self.into_ttype(*r)).intern(self.ctx),
            CType::FnPtr(ps, r) =>
                TType::Closure(self.into_ttypes(&ps), self.into_ttype(*r)).intern(self.ctx),
            CType::Object(o, gs) => TType::Object(o, self.into_ttypes(&gs)).intern(self.ctx),
            CType::Enum(e, gs) => TType::Enum(e, self.into_ttypes(&gs)).intern(self.ctx),
            CType::Dynamic(tr, tys) => {
                let info = tr.lookup(self.ctx);

                let generics = self.into_ttypes(&tys[0..info.generics.len()]);
                let bindings = info
                    .types
                    .keys()
                    .zip_exact(&tys[info.generics.len()..])
                    .map(|(n, t)| (*n, self.into_ttype(*t)))
                    .collect();

                TType::Dynamic(
                    TTraitTypeWithBindings(TTraitType(tr, generics).intern(self.ctx), bindings)
                        .intern(self.ctx),
                )
                .intern(self.ctx)
            },
        }
    }

    pub fn into_ttypes(&self, tys: &[CType<'a>]) -> Vec<Id<TType>> {
        tys.iter().map(|t| self.into_ttype(*t)).collect()
    }

    pub fn translate_tty(&mut self, ty: Id<TType>) -> AResult<CType<'a>> {
        if let Some(ty) = self.tys.get(&ty) {
            return Ok(*ty);
        }

        let cty = match &*ty.lookup(self.ctx) {
            TType::Skolem(_)
            | TType::MethodSkolem(_, _)
            | TType::GenericInfer(_)
            | TType::Infer(_)
            | TType::AssociatedSkolem(_, _, _, _)
            | TType::Associated(_, _, _, _) =>
                unreachable!("ICE: Leaking type {:?}", Pretty(ty, self.ctx)),
            TType::Int => CType::Int,
            TType::Float => CType::Float,
            TType::Char => CType::Char,
            TType::Bool => CType::Bool,
            TType::String => CType::String,
            TType::Never => CType::Never,
            TType::Array(e) => CType::Array(self.translate_sub_tty(*e)?),
            TType::Tuple(es) => CType::Tuple(self.translate_sub_ttys(&es)?),
            TType::Closure(ps, r) =>
                CType::Closure(self.translate_sub_ttys(&ps)?, self.translate_sub_tty(*r)?),
            TType::FnPtr(ps, r) =>
                CType::FnPtr(self.translate_sub_ttys(&ps)?, self.translate_sub_tty(*r)?),
            TType::Dynamic(trait_ty) => {
                let TTraitTypeWithBindings(trait_ty, bindings) = &*trait_ty.lookup(self.ctx);
                let TTraitType(tr, generics) = &*trait_ty.lookup(self.ctx);

                // "concretes" are generics, then all the bindings in order of their Id<str>
                // ordering
                let concretes = generics
                    .iter()
                    .chain(bindings.values())
                    .map(|t| self.translate_tty(*t))
                    .try_collect_vec()?;

                CType::Dynamic(*tr, self.alloc.alloc_slice_copy(&concretes))
            },
            TType::Object(o, gs) => CType::Object(*o, self.translate_sub_ttys(&gs)?),
            TType::Enum(e, gs) => CType::Enum(*e, self.translate_sub_ttys(&gs)?),
        };

        self.tys.insert(ty, cty);
        self.type_strings.insert(
            cty,
            self.alloc.alloc_str(&format!("{:?}", Pretty(ty, self.ctx))),
        );
        self.type_ids.insert(cty, fresh_id());

        Ok(cty)
    }

    pub fn translate_sub_tty(&mut self, ty: Id<TType>) -> AResult<&'a CType<'a>> {
        Ok(self.alloc.alloc(self.translate_tty(ty)?))
    }

    pub fn translate_sub_ttys(&mut self, tys: &[Id<TType>]) -> AResult<&'a [CType<'a>]> {
        let tys = tys
            .iter()
            .map(|ty| self.translate_tty(*ty))
            .try_collect_vec()?;
        Ok(self.alloc.alloc_slice_copy(&tys))
    }

    pub fn translate_ty(&mut self, ty: Id<LType>, tyck: &mut Typechecker) -> AResult<CType<'a>> {
        self.translate_tty(tyck.initialize_ty(ty, &btreemap! {})?)
    }

    pub fn translate_sub_tys(
        &mut self,
        tys: &[Id<LType>],
        tyck: &mut Typechecker,
    ) -> AResult<&'a [CType<'a>]> {
        let tys = tys
            .iter()
            .map(|ty| self.translate_ty(*ty, tyck))
            .try_collect_vec()?;
        Ok(self.alloc.alloc_slice_copy(&tys))
    }
}
