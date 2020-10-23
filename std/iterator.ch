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

enum RangeIterator {
  Finite(Int, Int),
  Infinite(Int),
}

impl Iterator for RangeIterator {
  type Item = Int.

  fn next(self) -> (Option<Int>, Self) = {
    match self {
      RangeIterator::Finite(a, b) =>
        if a < b {
          (Some(a), RangeIterator::Finite(a + 1, b))
        } else {
          (None, self)
        },
      RangeIterator::Infinite(a) =>
        (Some(a), RangeIterator::Infinite(a + 1)),
    }
  }.

  fn has_next(self) -> Bool = {
    match self {
      RangeIterator::Finite(a, b) => a < b,
      RangeIterator::Infinite(_) => true,
    }
  }.

  fn size_hint(self) -> Int = {
    match self {
      RangeIterator::Finite(a, b) => b - a,
      RangeIterator::Infinite(_) => -1,
    }
  }.
}

impl<It> Self for It where It: Iterator {
  fn map<F>(self, fun: F) -> Map<It, F> = Map { fun, iterator: self }.
  fn enumerate(self) -> Enumerate<It> = Enumerate { idx: 0, iterator: self }.
  fn limit(self, limit: Int) -> Limit<It> = Limit { remaining: limit, iterator: self }.
  fn zip<It2>(self, other: It2) -> Zip<It, It2> = Zip(self, other).

  fn collect<C>(self) -> C where C: FromIterator<<Self as Iterator>::Item> = {
    <C>::from_iterator(self)
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
      <Self as Iterator>::Item:default(),
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

    let a = internal_alloc_empty_array:<I>(s).

    for (i, x) in it:enumerate() {
      if i >= a:len() {
        a = internal_resize_array(a, a:len() * 2).
      }

      a[i] = x.
      last = i.
    }

    if a:len() == last + 1 {
      a
    } else {
      internal_resize_array(a, last + 1)
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
