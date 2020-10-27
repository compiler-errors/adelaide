use std::{fmt::Debug, hash::Hash, ops::Deref, sync::Arc};

pub struct Opaque<T>(Arc<T>);

impl<T> From<Arc<T>> for Opaque<T> {
    fn from(r: Arc<T>) -> Self {
        Opaque(r)
    }
}

impl<T> From<T> for Opaque<T> {
    fn from(t: T) -> Self {
        Opaque(Arc::new(t))
    }
}

impl<T> Deref for Opaque<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<T> Debug for Opaque<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Opaque<{}>", std::any::type_name::<T>())
    }
}

impl<T> Hash for Opaque<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}

impl<T> PartialEq for Opaque<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.0).eq(&Arc::as_ptr(&other.0))
    }
}

impl<T> Eq for Opaque<T> {}

impl<T> Clone for Opaque<T> {
    fn clone(&self) -> Self {
        Opaque(self.0.clone())
    }
}
