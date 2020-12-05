use std::iter::Zip;

pub trait ZipExact: ExactSizeIterator + Sized {
    fn zip_exact<U>(self, other: U) -> Zip<Self, <U as IntoIterator>::IntoIter>
    where
        U: IntoIterator,
        U::IntoIter: ExactSizeIterator;
}

impl<T: ExactSizeIterator> ZipExact for T {
    fn zip_exact<U>(self, other: U) -> Zip<Self, <U as IntoIterator>::IntoIter>
    where
        U: IntoIterator,
        U::IntoIter: ExactSizeIterator,
    {
        let iter = other.into_iter();
        assert!(
            self.len() == iter.len(),
            "Expected {}, got {}",
            self.len(),
            iter.len()
        );
        self.zip(iter)
    }
}
