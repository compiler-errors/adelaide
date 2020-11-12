let default_bucket_size: Int = 8.
let threshold: Float = 0.75.

object HashMap<K, V> {
  size: Int,

  buckets: [Bucket<K, V>],
  num_buckets: Int,
}

impl<K, V> Self for HashMap<K, V> {
  fn new() -> HashMap<K, V> = {
    allocate HashMap {
      size: 0,

      buckets: allocate [Bucket; default_bucket_size],
      num_buckets: default_bucket_size
    }
  }.

  fn bucket_from_hash(self, hash: Int) -> Bucket<K, V> = {
    self:buckets[abs(hash % self:num_buckets)]
  }.
}

impl<K, V> Self for HashMap<K, V> where K: Equals<K> + Hash {
  fn get(self, key: K) -> Option<V> = {
    let hash = key:hash().

    for (maybe_hash, maybe_key, maybe_value) in self:bucket_from_hash(hash):entries {
      if hash == maybe_hash {
        if key == maybe_key {
          return Some(maybe_value).
        }
      }
    }

    None
  }.

  fn remove(self, key: K) -> Option<V> = {
    let hash = key:hash().
    let bucket = self:bucket_from_hash(hash).

    let next_link = bucket:entries:root.
    while true {
      match next_link {
        Some(link) => {
          let (old_hash, old_key, old_value) = link:get().

          if hash == old_hash {
            if key == old_key {
              link:unlink().
              self:size = self:size - 1.
              return Some(old_value).
            }
          }

          next_link = link:next.
        },
        None => {
          break.
        },
      }
    }

    None
  }.

  fn put(self, key: K, value: V) -> Option<V> = {
    let hash = key:hash().
    let bucket = self:bucket_from_hash(hash).

    let next_link = bucket:entries:root.
    while true {
      match next_link {
        Some(link) => {
          let (old_hash, old_key, old_value) = link:get().

          if hash == old_hash {
            if key == old_key {
              link:set((hash, key, value)).
              return Some(old_value).
            }
          }

          next_link = link:next.
        },
        None => {
          break.
        },
      }
    }

    self:size = self:size + 1.
    bucket:entries:push_back((hash, key, value)).

    self:try_grow().
    None
  }.

  fn try_grow(self) = {
    if (self:size:to:<Float>()) > (self:num_buckets:to:<Float>()) * threshold {
      let old_buckets = self:buckets.
      self:num_buckets = self:num_buckets * 2.
      self:buckets = allocate [Bucket; self:num_buckets].

      for bucket in old_buckets {
        for (hash, key, value) in bucket:entries {
          self:bucket_from_hash(hash):entries:push_back((hash, key, value)).
        }
      }
    }
  }.
}

impl<K, V> Len for HashMap<K, V> {
  fn len(self) -> Int = self:size.
}

impl<K, V> Deref<K> for HashMap<K, V> where K: Equals<K> + Hash {
  type Result = V.

  fn deref(self, idx: K) -> V = {
    match self:get(idx) {
      Some(v) => v,
      None => panic("No such index"),
    }
  }.
}

impl<K, V> DerefAssign<K> for HashMap<K, V> where K: Equals<K> + Hash {
  fn deref_assign(self, idx: K, value: V) -> V = {
    self:put(idx, value).
    value
  }.
}

impl<K, V> Iterable for HashMap<K, V> {
  type Iterator = HashMapIterator<K, V>.
  type Item = (K, V).

  fn iterator(self) -> HashMapIterator<K, V> = {
    let (next_bucket, buckets) = self:buckets:iterator():next().

    HashMapIterator {
      size_hint: self:size,
      buckets,
      links: next_bucket:unwrap():entries:iterator()
    }
  }.
}

struct HashMapIterator<K, V> {
    size_hint: Int,
    buckets: ArrayIterator<Bucket<K, V>>,
    links: ListIterator<(Int, K, V)>,
}

impl<K, V> Iterator for HashMapIterator<K, V> {
  type Item = (K, V).

  fn next(self) -> (Option<(K, V)>, HashMapIterator<K, V>) = {
    let HashMapIterator { size_hint, buckets, links } = self.

    if links:has_next() {
      let (next_link, links) = links:next().
      let (_, k, v) = next_link:unwrap().
      return (Some((k, v)), HashMapIterator { size_hint: size_hint - 1, buckets, links }).
    }

    let ArrayIterator { idx: buckets_idx, ... } = buckets.

    while buckets:has_next() {
      let (next_bucket, next_buckets) = buckets:next().
      buckets = next_buckets. // We can't overwrite in a destructure.

      let next_bucket = next_bucket:unwrap().

      if next_bucket:entries:len() > 0 {
        let (next_link, links) = next_bucket:entries:iterator():next().
        let (_, k, v) = next_link:unwrap().

        return (Some((k, v)), HashMapIterator { size_hint: size_hint - 1, buckets, links }).
      }
    }

    assert size_hint == 0.
    (None, HashMapIterator { size_hint, buckets, links })
  }.

  fn has_next(self) -> Bool = {
    let HashMapIterator { size_hint, ... } = self.
    size_hint > 0
  }.

  fn size_hint(self) -> Int = {
    let HashMapIterator { size_hint, ... } = self.
    size_hint
  }.
}

impl<K, V> Into<String> for HashMap<K, V> where K: Into<String>, V: Into<String> {
  fn into(self) -> String = {
    let s = "HashMap{".
    let first = true.

    for (k, v) in self {
      if first {
        first = false.
      } else {
        s = s + ", ".
      }

      s = s + (k:to:<String>()) + ": " + (v:to:<String>()).
    }

    s + "}"
  }.
}

object Bucket<K, V> {
  entries: List<(Int, K, V)>,
}

impl<K, V> Default for Bucket<K, V> {
  fn default() -> Bucket<K, V> =
    allocate Bucket { entries: List::new() }.
}

impl<K, V> Into<String> for Bucket<K, V> where K: Into<String>, V: Into<String> {
  fn into(self) -> String = "Bucket { \(self:entries) }".
}
