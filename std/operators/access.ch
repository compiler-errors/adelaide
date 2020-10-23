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
      // if idx < 0 |? idx >= self:len() {
      //  panic:<()>("Index out of bounds for \(type_string:<Self>())... length = \(self:len()), index = \(idx).").
      // }

      internal_array_deref(self, idx)
  }.
}

impl<T> Deref<RangeIterator> for [T] {
  type Result = [T].

  fn deref(self, idx: RangeIterator) -> [T] = {
    let len = self:len().

    let (start, end) = match idx {
      RangeIterator::Finite(a, b) => {
        if a > b {
          panic:<()>("Start index of array slice is greater than end. Start = \(a), End = \(b).").
        }

        (a, b)
      },
      RangeIterator::Infinite(a) => (a, self:len()),
    }.

    if 0 > start {
      panic:<()>("Start index of array slice is less than 0. Start = \(start)").
    }

    if start > len {
      panic:<()>("Start index of array slice is out of bounds. Start = \(start), length = \(len).").
    }

    if end > len {
      panic:<()>("End index of array slice is out of bounds. End = \(start), length = \(len).").
    }

    internal_array_slice(self, start, end)
  }.
}

impl Deref<Int> for String {
  type Result = Char.

  fn deref(self, idx: Int) -> Char = {
    if idx < 0 |? idx >= self:len() {
      panic:<()>("Index out of bounds for String... length = \(self:len()), index = \(idx).").
    }

    internal_string_deref(self, idx)
  }.
}

trait DerefAssign<Idx> {
  type Value.

  fn deref_assign(
    self,
    idx: Idx,
    value: <Self as DerefAssign<Idx>>::Value) -> <Self as DerefAssign<Idx>>::Value.
}

impl<T> DerefAssign<Int> for [T] {
  type Value = T.

  fn deref_assign(self, idx: Int, value: T) -> T = {
      internal_array_store(self, idx, value)
  }.
}
