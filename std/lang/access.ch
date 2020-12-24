// Array internal operations
extern fn internal_array_deref<T>(a: [T], n: Int) -> T.
extern fn internal_array_store<T>(a: [T], n: Int, i: T) -> T.
extern fn internal_array_slice<T>(a: [T], b: Int, c: Int) -> [T].

// Get a char from a string
extern fn internal_string_deref(a: String, n: Int) -> Char.

trait Deref<Idx> {
  type Result.

  fn deref(self, idx: Idx) -> <Self as Deref<Idx>>::Result.
}

impl<T> Deref<Int> for [T] {
  type Result = T.

  fn deref(self, idx: Int) -> T = {
    if idx < 0 |? idx >= self:len() {
      panic("Index out of bounds for \(type_string_of:<Self>())... length = \(self:len()), index = \(idx).").
    }

    internal_array_deref(self, idx)
  }.
}

impl<T> Deref<RangeIterator> for [T] {
  type Result = [T].

  fn deref(self, idx: RangeIterator) -> [T] = {
    let len = self:len().

    let (start, end) = match idx {
      RangeIterator::Inclusive(a, b) => {
        if a > b {
          panic("Start index of array slice is greater than end. Start = \(a), End = \(b).").
        }

        (a, b + 1)
      },
      RangeIterator::Exclusive(a, b) => {
        if a > b {
          panic("Start index of array slice is greater than end. Start = \(a), End = \(b).").
        }

        (a, b)
      },
      RangeIterator::Infinite(a) => (a, len),
    }.

    if 0 > start {
      panic("Start index of array slice is less than 0. Start = \(start)").
    }

    if start > len {
      start = len.
    }

    if end > len {
      end = len.
    }

    internal_array_slice(self, start, end)
  }.
}

impl Deref<Int> for String {
  type Result = Char.

  fn deref(self, idx: Int) -> Char = {
    if idx < 0 |? idx >= self:len() {
      panic("Index out of bounds for String... length = \(self:len()), index = \(idx).").
    }

    internal_string_deref(self, idx)
  }.
}

trait DerefAssign<Idx> where Self: Deref<Idx> {
  fn deref_assign(
    self,
    idx: Idx,
    value: <Self as Deref<Idx>>::Result) -> <Self as Deref<Idx>>::Result.
}

impl<T> DerefAssign<Int> for [T] {
  fn deref_assign(self, idx: Int, value: T) -> T = {
    if idx < 0 |? idx >= self:len() {
      panic("Index out of bounds for \(type_string_of:<Self>())... length = \(self:len()), index = \(idx).").
    }
    internal_array_store(self, idx, value)
  }.
}
