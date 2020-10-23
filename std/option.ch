enum Option<T> {
  None,
  Some(T),
}

impl<T> Self for Option<T> {
  fn map<F, O>(self, f: F) -> Option<O> where F: Fn(T) -> O = {
    match self {
      Some(t) => Some(f(t)),
      None => None,
    }
  }.

  fn expect(self, s: String) -> T = {
    match self {
      Some(v) => v,
      _ => panic(s),
    }
  }.

  fn unwrap(self) -> T = {
    self:expect("No value for \(type_string:<Self>())")
  }.

  fn unwrap_or(self, other: T) -> T = {
    match self {
      Some(v) => v,
      _ => other,
    }
  }.

  fn unwrap_or_else<F>(self, otherwise: F) -> T where F: Fn() -> T = {
    match self {
      Some(v) => v,
      _ => otherwise(),
    }
  }.

  fn is_some(self) -> Bool = {
    match self {
      Some(_) => true,
      None => false,
    }
  }.

  fn is_none(self) -> Bool = {
      !self:is_some()
  }.
}

impl<T> Into<String> for Option<T> where T: Into<String> {
  fn into(self) -> String = {
    match self {
      Some(s) => "Some(\(s))",
      None => "None",
    }
  }.
}

trait Default {
  fn default() -> Self.
}

impl Default for Int {
  fn default() -> Int = 0.
}

impl Default for Char {
  fn default() -> Char = ' '.
}

impl Default for Bool {
  fn default() -> Bool = false.
}

impl Default for String {
  fn default() -> String = "".
}

enum Either<L, R> {
  Left(L),
  Right(R),
}

impl<L, R> Self for Either<L, R> {
  fn expect_left(self, s: String) -> L =
    match self {
      Either::Left(v) => v,
      _ => panic(s),
    }.

  fn unwrap_left(self) -> L =
    self:expect_left("No Left value for \(type_string:<Self>())").

  fn map_left<F, O>(self, f: F) -> Either<O, R> where F: Fn(L) -> O =
    match self {
      Either::Left(l) => Either::Left(f(l)),
      Either::Right(r) => Either::Right(r),
    }.

  fn is_left(self) -> Bool =
    match self {
      Either::Left(_) => true,
      Either::Right(_) => false,
    }.

  fn expect_right(self, s: String) -> R =
    match self {
      Either::Right(v) => v,
      _ => panic(s),
    }.

  fn unwrap_right(self) -> R =
    self:expect_right("No Right value for \(type_string:<Self>())").

  fn map_right<F, O>(self, f: F) -> Either<L, O> where F: Fn(R) -> O =
    match self {
      Either::Left(l) => Either::Left(l),
      Either::Right(r) => Either::Right(f(r)),
    }.

  fn is_right(self) -> Bool =
    match self {
      Either::Left(_) => false,
      Either::Right(_) => true,
    }.

  fn map<F, G, OL, OR>(self, f: F, g: G) -> Either<OL, OR>
    where F: Fn(L) -> OL, G: Fn(R) -> OR = match self {
      Either::Left(l) => Either::Left(f(l)),
      Either::Right(r) => Either::Right(g(r)),
    }.

  fn unify<F, G, O>(self, f: F, g: G) -> O
    where F: Fn(L) -> O, G: Fn(R) -> O = match self {
      Either::Left(l) => f(l),
      Either::Right(r) => g(r),
    }.
}