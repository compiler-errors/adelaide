trait Iterable {
  type Iterator: Iterator<Item=<Self as Iterable>::Item>.
  type Item.

  fn iterator(self) -> <Self as Iterable>::Iterator.
}

impl<It, T> Iterable for It where It: Iterator<Item=T> {
  type Iterator = It.
  type Item = T.

  fn iterator(self) -> Self = self.
}

trait Iterator {
  type Item.

  fn next(self) -> (Option<<Self as Iterator>::Item>, Self).
  fn has_next(self) -> Bool.
  fn size_hint(self) -> Int.
}

trait FromIterator<I> {
  fn from_iterator<It>(it: It) -> Self where It: Iterator<Item=I>.
}

impl<It> Self for It where It: Iterator {
  fn map<F, O>(self, fun: F) -> Map<It, F> where F: Fn(<Self as Iterator>::Item) -> O = Map { fun, iterator: self }.
  fn enumerate(self) -> Enumerate<It> = Enumerate { idx: 0, iterator: self }.
  fn limit(self, limit: Int) -> Limit<It> = Limit { remaining: limit, iterator: self }.
  fn zip<It2>(self, other: It2) -> Zip<It, It2> where It2: Iterator = Zip(self, other).

  fn flat_map<F, OutIt>(self, fun: F) -> FlatMap<It, F, OutIt> where
      F: Fn(<Self as Iterator>::Item) -> OutIt, OutIt: Iterable
    = FlatMap { iterator: self, fun, sub_iterator: None }.

  fn collect<C>(self) -> C where C: FromIterator<<Self as Iterator>::Item> = {
    C::from_iterator(self)
  }.

  fn fold<F, I>(self, i: I, f: F) -> I where F: Fn(I, <Self as Iterator>::Item) -> I = {
    for j in self {
        i = f(i, j).
    }

    i
  }.
}

impl<T> Self for T where T: Iterator, <Self as Iterator>::Item: Default + Add<<Self as Iterator>::Item, Result=<Self as Iterator>::Item> {
  fn sum(self) -> <Self as Iterator>::Item =
    Self::fold(
      self,
      <<Self as Iterator>::Item>::default(),
      |a, b| a + b).
}

struct Map<It, F> {
  iterator: It,
  fun: F,
}

impl<It, F, O> Iterator for Map<It, F> where
  It: Iterator,
  F: Fn(<It as Iterator>::Item) -> O {
  type Item = O.

  fn next(self) -> (Option<O>, Self) = {
    let Map { iterator, fun } = self.

    let (next, iterator) = iterator:next().

    (next:map(fun), Map { iterator, fun })
  }.

  fn has_next(self) -> Bool = {
    let Map { iterator, ... } = self.
    iterator:has_next()
  }.

  fn size_hint(self) -> Int = {
    let Map { iterator, ... } = self.
    iterator:size_hint()
  }.
}

struct Enumerate<It> {
  iterator: It,
  idx: Int,
}

impl<It> Iterator for Enumerate<It> where It: Iterator {
  type Item = (Int, <It as Iterator>::Item).

  fn next(self) -> (Option<(Int, <It as Iterator>::Item)>, Self) = {
    let Enumerate { iterator, idx } = self.

    let (next, iterator) = iterator:next().

    match next {
      Some(next) => (Some((idx, next)), Enumerate { iterator, idx: idx + 1 }),
      None => (None, Enumerate { iterator, idx })
    }
  }.

  fn has_next(self) -> Bool = {
    let Enumerate { iterator, ... } = self.
    iterator:has_next()
  }.

  fn size_hint(self) -> Int = {
    let Enumerate { iterator, ... } = self.
    iterator:size_hint()
  }.
}

struct Limit<It> {
  iterator: It,
  remaining: Int,
}

impl<It> Iterator for Limit<It> where It: Iterator {
  type Item = <It as Iterator>::Item.

  fn next(self) -> (Option<<Self as Iterator>::Item>, Self) = {
    let Limit { iterator, remaining } = self.

    if remaining <= 0 {
      (None, self)
    } else {
      let (next, iterator) = iterator:next().

      (next, Limit { iterator, remaining: remaining - 1 })
    }
  }.

  fn has_next(self) -> Bool = {
    let Limit { iterator, remaining } = self.

    if remaining <= 0 {
      false
    } else {
      iterator:has_next()
    }
  }.

  fn size_hint(self) -> Int = {
    let Limit { iterator, remaining } = self.
    let hint = iterator:size_hint().

    if hint < 0 {
      remaining
    } else {
      min(remaining, hint)
    }
  }.
}

struct Zip<I1, I2>(I1, I2).

impl<I1, I2> Iterator for Zip<I1, I2> where I1: Iterator, I2:Iterator {
  type Item = (<I1 as Iterator>::Item, <I2 as Iterator>::Item).

  fn next(self) -> (Option<<Self as Iterator>::Item>, Self) = {
    let Zip(i1, i2) = self.

    if i1:has_next() &? i2:has_next() {
      let (n1, i1) = i1:next().
      let (n2, i2) = i2:next().

      (Some((n1:unwrap(), n2:unwrap())), Zip(i1, i2))
    } else {
      (None, self)
    }
  }.

  fn has_next(self) -> Bool = {
    let Zip(i1, i2) = self.
    i1:has_next() &? i2:has_next()
  }.

  fn size_hint(self) -> Int = {
    let Zip(i1, i2) = self.
    match (i1:size_hint(), i2:size_hint()) {
      (-1, -1) => -1,
      (-1, b)  => b,
      (a, -1)  => a,
      (a, b)   => min(a, b),
    }
  }.
}

struct FlatMap<It, F, OutIt> where OutIt: Iterable {
  iterator: It,
  fun: F,
  sub_iterator: Option<<OutIt as Iterable>::Iterator>,
}

impl<It, F, OutIt> Iterator for FlatMap<It, F, OutIt> where
    F: Fn(<It as Iterator>::Item) -> OutIt, It: Iterator, OutIt: Iterable {
  type Item = <OutIt as Iterable>::Item.

  fn next(self) -> (Option<<Self as Iterator>::Item>, Self) = {
    let FlatMap { iterator, fun, sub_iterator } = self.

    if let Some(sub_iterator) = sub_iterator {
      if let (Some(sub_item), sub_iterator) = sub_iterator:next() {
        return (Some(sub_item), FlatMap { iterator, fun, sub_iterator: Some(sub_iterator) }).
      }
    }

    loop {
      if let (Some(item), new_iterator) = iterator:next() {
        iterator = new_iterator.
        let sub_iterator = fun(item):iterator().

        if let (Some(sub_item), sub_iterator) = sub_iterator:next() {
          return (Some(sub_item), FlatMap { iterator, fun, sub_iterator: Some(sub_iterator) }).
        }
      } else {
        break.
      }
    }

    (None, FlatMap { iterator, fun, sub_iterator: None })
  }.

  fn has_next(self) -> Bool = {
    todo()
  }.

  fn size_hint(self) -> Int = {
    todo()
  }.
}

impl<T> Iterable for [T] {
  type Iterator = ArrayIterator<T>.
  type Item = T.

  fn iterator(self) -> ArrayIterator<T> = {
    ArrayIterator { array: self, idx: 0 }
  }.
}

struct ArrayIterator<T> {
  array: [T],
  idx: Int,
}

impl<T> Iterator for ArrayIterator<T> {
  type Item = T.

  fn next(self) -> (Option<T>, Self) = {
    let ArrayIterator { array, idx } = self.
    let len = array:len().

    if idx >= len {
      (None, self)
    } else {
      (Some(array[idx]), ArrayIterator { array, idx: idx + 1 })
    }
  }.

  fn has_next(self) -> Bool = {
    let ArrayIterator { array, idx } = self.

    idx < array:len()
  }.

  fn size_hint(self) -> Int = {
    let ArrayIterator { array, idx } = self.

    array:len() - idx
  }.
}

impl<I> FromIterator<I> for [I] {
  fn from_iterator<It>(it: It) -> [I] where It: Iterator<Item=I> = {
    let s = it:size_hint().
    let last = -1.

    let a = internal_alloc_empty_array(max(0, s)).

    for (i, x) in it:enumerate() {
      if i >= a:len() {
        a = copy_array_with_size(a, a:len() * 2).
      }

      a[i] = x.
      last = i.
    }

    if a:len() == last + 1 {
      a
    } else {
      copy_array_with_size(a, last + 1)
    }
  }.
}

struct StringIterator {
  str: String,
  idx: Int,
}

impl Iterable for String {
  type Iterator = StringIterator.
  type Item = Char.

  fn iterator(self) -> StringIterator = {
    StringIterator { str: self, idx: 0 }
  }.
}

impl Iterator for StringIterator {
  type Item = Char.

  fn next(self) -> (Option<Char>, Self) = {
    let StringIterator { str, idx } = self.

    if idx >= str:len() {
      (None, self)
    } else {
      (Some(str[idx]), StringIterator { str, idx: idx + 1 })
    }
  }.

  fn has_next(self) -> Bool = {
    let StringIterator { str, idx } = self.

    idx < str:len()
  }.

  fn size_hint(self) -> Int = {
    let StringIterator { str, idx } = self.

    str:len() - idx
  }.
}

impl FromIterator<Char> for String {
  fn from_iterator<It>(it: It) -> String where It: Iterator<Item=Char> = {
    let s = "a".

    for c in it {
        s = s + (c:to:<String>()).
    }

    s
  }.
}

struct OptionIterator<T>(Option<T>).

impl<T> Iterable for Option<T> {
  type Item = T.
  type Iterator = OptionIterator<T>.

  fn iterator(self) -> OptionIterator<T> = {
    OptionIterator(self)
  }.
}

impl<T> Iterator for OptionIterator<T> {
  type Item = T.

  fn next(self) -> (Option<T>, OptionIterator<T>) = {
    (self:0, OptionIterator(None))
  }.

  fn has_next(self) -> Bool = {
    self:0:is_some()
  }.

  fn size_hint(self) -> Int = {
    if self:0:is_some() {
      1
    } else {
      0
    }
  }.
}

struct Repeat<T>(T, Int).

fn repeat<T>(t: T, times: Int) -> Repeat<T> = {
  Repeat(t, times)
}.

impl<T> Iterator for Repeat<T> {
  type Item = T.

  fn next(self) -> (Option<T>, Repeat<T>) = {
    if self:1 > 0 {
      (Some(self:0), Repeat(self:0, self:1 - 1))
    } else {
      (None, self)
    }
  }.

  fn has_next(self) -> Bool = {
    self:1 > 0
  }.

  fn size_hint(self) -> Int = {
    max(0, self:1)
  }.
}
