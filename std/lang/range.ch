enum Bound<T> {
  Inclusive(T),
  Exclusive(T),
  Unbounded,
}

trait Range<T> {
  type RangeKind.
  fn range(self, t: Bound<T>) -> <Self as Range<T>>::RangeKind.
}

impl Range<Int> for Int {
  type RangeKind = RangeIterator.

  fn range(self, t: Bound<Int>) -> RangeIterator = match t {
    Bound::Inclusive(t) => RangeIterator::Inclusive(self, t),
    Bound::Exclusive(t) => RangeIterator::Exclusive(self, t),
    Bound::Unbounded => RangeIterator::Infinite(self),
  }.
}

enum RangeIterator {
  Inclusive(Int, Int),
  Exclusive(Int, Int),
  Infinite(Int),
}

impl Iterator for RangeIterator {
  type Item = Int.

  fn next(self) -> (Option<Int>, Self) = {
    match self {
      RangeIterator::Inclusive(a, b) =>
        if a <= b {
          (Some(a), RangeIterator::Inclusive(a + 1, b))
        } else {
          (None, self)
        },
      RangeIterator::Exclusive(a, b) =>
        if a < b {
          (Some(a), RangeIterator::Exclusive(a + 1, b))
        } else {
          (None, self)
        },
      RangeIterator::Infinite(a) =>
        (Some(a), RangeIterator::Infinite(a + 1)),
    }
  }.

  fn size_hint(self) -> Int = {
    match self {
      RangeIterator::Inclusive(a, b) => b - a + 1,
      RangeIterator::Exclusive(a, b) => b - a,
      RangeIterator::Infinite(_) => -1,
    }
  }.
}

impl Into<String> for RangeIterator {
  fn into(self) -> String = {
    match self {
      RangeIterator::Inclusive(a, b) => "\(a)..=\(b)",
      RangeIterator::Exclusive(a, b) => "\(a)..\(b)",
      RangeIterator::Infinite(a) => "\(a)...",
    }
  }.
}