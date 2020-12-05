use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
};

pub trait TryCollectVec<T, E> {
    fn try_collect_vec(self) -> Result<Vec<T>, E>;
}

impl<I, T, E> TryCollectVec<T, E> for I
where
    I: Iterator<Item = Result<T, E>>,
{
    fn try_collect_vec(self) -> Result<Vec<T>, E> {
        self.collect()
    }
}

pub trait TryCollectBTreeMap<K, V, E> {
    fn try_collect_btreemap(self) -> Result<BTreeMap<K, V>, E>;
}

impl<I, K: Ord, V, E> TryCollectBTreeMap<K, V, E> for I
where
    I: Iterator<Item = Result<(K, V), E>>,
{
    fn try_collect_btreemap(self) -> Result<BTreeMap<K, V>, E> {
        self.collect()
    }
}

pub trait TryCollectHashMap<K, V, E> {
    fn try_collect_hashmap(self) -> Result<HashMap<K, V>, E>;
}

impl<I, K: Hash + Eq, V, E> TryCollectHashMap<K, V, E> for I
where
    I: Iterator<Item = Result<(K, V), E>>,
{
    fn try_collect_hashmap(self) -> Result<HashMap<K, V>, E> {
        self.collect()
    }
}
