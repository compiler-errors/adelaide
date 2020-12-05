extern fn internal_gt(a: Int, b: Int) -> Bool.
extern fn internal_eq(a: Int, b: Int) -> Bool.

extern fn internal_fgt(a: Float, b: Float) -> Bool.
extern fn internal_feq(a: Float, b: Float) -> Bool.

extern fn internal_string_eq(a: String, b: String) -> Bool.

trait SimpleCompare<T> {
  fn compare(self, other: T) -> Int.
}

impl SimpleCompare<Int> for Int {
  fn compare(self, other: Int) -> Int = {
    self - other
  }.
}

impl SimpleCompare<Float> for Float {
  fn compare(self, other: Float) -> Int = {
    if internal_feq(self, other) {
      0
    } else if internal_fgt(self, other) {
      1
    } else {
      -1
    }
  }.
}

impl SimpleCompare<Char> for Char {
  fn compare(self, other: Char) -> Int = {
    self - other
  }.
}

trait Compare<T> {
  fn gt(self, other: T) -> Bool.
  fn lt(self, other: T) -> Bool.
  fn ge(self, other: T) -> Bool.
  fn le(self, other: T) -> Bool.
}

impl<S, T> Compare<T> for S where S: SimpleCompare<T> {
  fn gt(self, other: T) -> Bool = {
    let res = self:compare(other).
    internal_gt(res, 0)
  }.

  fn lt(self, other: T) -> Bool = {
    let res = self:compare(other).
    internal_gt(0, res)
  }.

  fn ge(self, other: T) -> Bool = {
    !(self < other)
  }.

  fn le(self, other: T) -> Bool = {
    !(self > other)
  }.
}

trait Equals<T> {
  fn eq(self, other: T) -> Bool.
  fn ne(self, other: T) -> Bool.
}

impl<S, T> Equals<T> for S where S: SimpleCompare<T> {
  fn eq(self, other: T) -> Bool = {
    let res = self:compare(other).
    internal_eq(res, 0)
  }.

  fn ne(self, other: T) -> Bool = {
    !(self == other)
  }.
}

impl Equals<String> for String {
  fn eq(self, other: String) -> Bool = {
    internal_string_eq(self, other)
  }.

  fn ne(self, other: String) -> Bool = {
    !internal_string_eq(self, other)
  }.
}

trait And<T> {
  type Result.
  fn and(self, other: T) -> <Self as And<T>>::Result.
}

impl And<Bool> for Bool {
  type Result = Bool.

  fn and(self, other: Bool) -> Bool = {
    self &? other
  }.
}

trait Or<T> {
  type Result.
  fn or(self, other: T) -> <Self as Or<T>>::Result.
}

impl Or<Bool> for Bool {
  type Result = Bool.

  fn or(self, other: Bool) -> Bool = {
    self |? other
  }.
}

trait Not {
  type Result.
  fn not(self) -> <Self as Not>::Result.
}

impl Not for Bool {
  type Result = Bool.

  fn not(self) -> Bool = {
    if self {
        false
    } else {
        true
    }
  }.
}
