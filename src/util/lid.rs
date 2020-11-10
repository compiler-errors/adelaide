use std::{fmt::Debug, hash::Hash, lazy::SyncOnceCell, sync::Arc};

use crate::{
    ctx::AdelaideContext,
    util::{Id, Lookup, PrettyPrint},
};

pub struct LId<T: LateLookup + ?Sized>(pub Id<T::Source>, SyncOnceCell<Id<T>>);

impl<T: LateLookup + Lookup + ?Sized> LId<T> {
    pub fn lookup(&self, ctx: &dyn AdelaideContext) -> Arc<T> {
        self.get(ctx).lookup(ctx)
    }

    pub fn get(&self, ctx: &dyn AdelaideContext) -> Id<T> {
        *self.1.get_or_init(|| T::late_lookup(self.0, ctx))
    }
}

pub trait LateLookup {
    type Source;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self>;
}

impl<T: LateLookup + ?Sized> From<Id<T::Source>> for LId<T> {
    fn from(s: Id<T::Source>) -> Self {
        LId(s, SyncOnceCell::new())
    }
}

impl<T: LateLookup + ?Sized> Clone for LId<T> {
    fn clone(&self) -> Self {
        LId(self.0, self.1.clone())
    }
}

impl<T: LateLookup + ?Sized> Debug for LId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LId<{}>({:?})", std::any::type_name::<T>(), self.0.id())
    }
}

impl<T: LateLookup + ?Sized> PrettyPrint for LId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _: &dyn AdelaideContext) -> std::fmt::Result {
        write!(f, "LId<{}>({:?})", std::any::type_name::<T>(), self.0.id())
    }
}

impl<T: LateLookup + ?Sized> Hash for LId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T: LateLookup + ?Sized> PartialEq for LId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T: LateLookup + ?Sized> Eq for LId<T> {}
