use std::{
    collections::{BTreeMap, VecDeque},
    fmt::{Debug, Formatter, Result},
};

use crate::ctx::AdelaideContext;

pub trait PrettyPrint {
    fn fmt(&self, f: &mut Formatter, ctx: &dyn AdelaideContext) -> Result;
}

pub struct Pretty<'ctx, T>(pub T, pub &'ctx dyn AdelaideContext);

impl<'ctx, T: PrettyPrint> Debug for Pretty<'ctx, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f, self.1)
    }
}

impl<T: PrettyPrint> PrettyPrint for &'_ T {
    fn fmt(&self, f: &mut Formatter, ctx: &dyn AdelaideContext) -> Result {
        <T as PrettyPrint>::fmt(self, f, ctx)
    }
}

impl<T: PrettyPrint> PrettyPrint for Option<T> {
    fn fmt(&self, f: &mut Formatter, ctx: &dyn AdelaideContext) -> Result {
        match self {
            Some(t) => {
                let mut d = f.debug_tuple("Some");
                d.field(&Pretty(t, ctx));
                d.finish()
            },
            None => write!(f, "None"),
        }
    }
}

impl<T: PrettyPrint> PrettyPrint for Vec<T> {
    fn fmt(&self, f: &mut Formatter, ctx: &dyn AdelaideContext) -> Result {
        let mut h = f.debug_list();

        for i in self {
            h.entry(&Pretty(i, ctx));
        }

        h.finish()
    }
}

impl<T: PrettyPrint> PrettyPrint for VecDeque<T> {
    fn fmt(&self, f: &mut Formatter, ctx: &dyn AdelaideContext) -> Result {
        let mut h = f.debug_list();

        for i in self {
            h.entry(&Pretty(i, ctx));
        }

        h.finish()
    }
}

impl<K: PrettyPrint, V: PrettyPrint> PrettyPrint for BTreeMap<K, V> {
    fn fmt(&self, f: &mut Formatter, ctx: &dyn AdelaideContext) -> Result {
        let mut h = f.debug_map();

        for (k, v) in self {
            h.entry(&Pretty(k, ctx), &Pretty(v, ctx));
        }

        h.finish()
    }
}

macro_rules! simple_pretty {
    ($($ty:ty),*) => {$(
        impl PrettyPrint for $ty {
            fn fmt(&self, f: &mut Formatter, _: &dyn AdelaideContext) -> Result {
                <Self as Debug>::fmt(self, f)
            }
        }
    )*};
    ($($ty:ty,)*) => {
        simple_pretty! { $($ty),* }
    }
}

simple_pretty!(bool, u64, char, str);
simple_pretty!(std::path::PathBuf);
simple_pretty!(crate::file::ModuleName);

macro_rules! tuple_pretty {
    ($($ty:ident),*; $($idx:tt),*) => {
        impl<$($ty: PrettyPrint),*> PrettyPrint for ($($ty,)*) {
            fn fmt(&self, f: &mut Formatter, ctx: &dyn AdelaideContext) -> Result {
                let mut h = f.debug_tuple("");
                $(h.field(&Pretty(&self.$idx, ctx));)*
                h.finish()
            }
        }
    };
}

tuple_pretty!(A; 0);
tuple_pretty!(A, B; 0, 1);
tuple_pretty!(A, B, C; 0, 1, 2);
