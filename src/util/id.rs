use crate::{ctx::AdelaideContext, util::PrettyPrint};
use std::{fmt::Debug, hash::Hash, marker::PhantomData, num::NonZeroU32, sync::Arc};

#[must_use]
pub struct Id<T: ?Sized>(NonZeroU32, PhantomData<T>);

impl<T: ?Sized> Id<T> {
    pub fn id(self) -> u32 {
        self.0.into()
    }
}

impl<T: Lookup + ?Sized> Id<T> {
    pub fn lookup(self, ctx: &dyn AdelaideContext) -> Arc<T> {
        T::lookup(self, ctx)
    }
}

impl<T: ?Sized> salsa::InternKey for Id<T> {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Id(
            unsafe { NonZeroU32::new_unchecked(v.as_u32() + 1) },
            PhantomData,
        )
    }

    fn as_intern_id(&self) -> salsa::InternId {
        salsa::InternId::from(self.0.get() - 1)
    }
}

impl<T: ?Sized> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id(self.0, PhantomData)
    }
}

impl<T: ?Sized> Copy for Id<T> {}

impl<T: ?Sized> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T: ?Sized> Eq for Id<T> {}

impl<T: ?Sized> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T: ?Sized> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T: ?Sized> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T: ?Sized> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id<{}>({})", std::any::type_name::<T>(), self.0)
    }
}

impl<T: Lookup + PrettyPrint + ?Sized> PrettyPrint for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter, ctx: &dyn AdelaideContext) -> std::fmt::Result {
        T::lookup(*self, ctx).fmt(f, ctx)
    }
}

pub trait Lookup {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self>;
    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self>;
}

pub trait Intern {
    type Target: Lookup + ?Sized;
    fn intern(self, ctx: &dyn AdelaideContext) -> Id<Self::Target>;
}

impl<T: Lookup> Intern for T {
    type Target = T;

    fn intern(self, ctx: &dyn AdelaideContext) -> Id<T> {
        Arc::new(self).intern_self(ctx)
    }
}

impl Intern for &str {
    type Target = str;

    fn intern(self, ctx: &dyn AdelaideContext) -> Id<str> {
        let s: Arc<str> = self.into();
        s.intern_self(ctx)
    }
}

impl Lookup for str {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_str(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_str(self)
    }
}
