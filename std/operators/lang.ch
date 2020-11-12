// Transmute all basic types to string
extern fn internal_itos(i: Int) -> String.
extern fn internal_ftos(f: Float) -> String.
extern fn internal_ctos(c: Char) -> String.

// ctoi is basically a no-op in VS, and itof is just widening
extern fn internal_ctoi(c: Char) -> Int.
extern fn internal_itof(c: Int) -> Float.

// Allocate an array full of undefined values
extern fn internal_alloc_empty_array<T>(n: Int) -> [T].

// Length functions
extern fn internal_array_len<T>(a: [T]) -> Int.
extern fn internal_string_len(s: String) -> Int.

trait Len {
  fn len(self) -> Int.
}

impl<T> Len for [T] {
  fn len(self) -> Int = {
    internal_array_len(self)
  }.
}

impl Len for String {
  fn len(self) -> Int = {
    internal_string_len(self)
  }.
}

trait Hash {
  fn hash(self) -> Int.
}

impl Hash for Int {
  fn hash(self) -> Int = {
    self * -7046029254386353131
  }.
}

impl Hash for String {
  fn hash(self) -> Int = {
    let h = 525201411107845655.

    for c in self {
      let c_as_i = internal_ctoi(c).

      h = internal_xor(h, c_as_i).
      h = h * 6616326155283851669.
      h = internal_xor(h, internal_lshr(c_as_i, 47)).
    }

    h
  }.
}

impl Hash for Char {
  fn hash(self) -> Int = {
    let c_as_i = internal_ctoi(self).

    c_as_i:hash()
  }.
}

impl Hash for Bool {
  fn hash(self) -> Int = if self { 0 } else { 1 }.
}

trait Call<Args> {
  type Return.
  fn call(self, args: Args) -> <Self as Call<Args>>::Return.
}

trait AllocateArray {
  fn allocate_array(n: Int) -> [Self].
}

impl<T> AllocateArray for T where T: Default {
  fn allocate_array(n: Int) -> [T] = {
    let a = internal_alloc_empty_array:<T>(n).

    for i in 0..n {
      a[i] = <T>::default().
    }

    a
  }.
}

trait Into<T> {
  fn into(self) -> T.
}

impl<T> Into<T> for T {
  fn into(self) -> T = self.
}

impl Into<Float> for Int {
  fn into(self) -> Float = {
    internal_itof(self)
  }.
}

impl Into<String> for Int {
  fn into(self) -> String = {
    internal_itos(self)
  }.
}

impl Into<String> for Float {
  fn into(self) -> String = {
    internal_ftos(self)
  }.
}

impl Into<String> for Char {
  fn into(self) -> String = {
    internal_ctos(self)
  }.
}

impl Into<String> for Bool {
  fn into(self) -> String = {
    match self {
      true => "true",
      false => "false",
    }
  }.
}

impl<T> Into<String> for [T] where T: Into<String> {
  fn into(self) -> String = {
    let s = "[".
    let first = true.

    for i in self {
      if first {
        first = false.
      } else {
        s = s + ", ".
      }

      s = s + (i:to:<String>()).
    }

    s + "]"
  }.
}

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

/* TODO: Not really an internal fn, but I can't resize arrays that have slices to them. It's too risky::! */
fn internal_resize_array<T>(a: [T], n: Int) -> [T] = {
  let a_new = internal_alloc_empty_array:<T>(n).

  for (i, x) in a:iterator():enumerate():limit(n) {
    a_new[i] = x.
  }

  a_new
}.