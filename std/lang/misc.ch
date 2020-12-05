// Length functions
extern fn internal_array_len<T>(a: [T]) -> Int.
extern fn internal_string_len(s: String) -> Int.

extern fn internal_ctoi(c: Char) -> Int.
extern fn internal_itof(i: Int) -> Float.
extern fn internal_itos(i: Int) -> String.
extern fn internal_ftos(f: Float) -> String.
extern fn internal_ctos(c: Char) -> String.

// ----------------------------------- //

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

// ----------------------------------- //

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

// ----------------------------------- //

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