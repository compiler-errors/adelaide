enum Result<T, E> {
  Ok(T),
  Error(E),
}

impl<T, E> Into<String> for Result<T, E> where T: Into<String>, E: Into<String> {
  fn into(self) -> String = {
    match self {
      Result::Ok(s) => "Ok(\(s))",
      Result::Error(s) => "Error(\(s))",
    }
  }.
}

trait IntoResult {
  type OkType.
  type ErrorType.

  fn into_result(self) -> Result<<Self as IntoResult>::OkType, <Self as IntoResult>::ErrorType>.

  fn from_ok(t: <Self as IntoResult>::OkType) -> Self.
  fn from_error(e: <Self as IntoResult>::ErrorType) -> Self.
}

impl<T, E> IntoResult for Result<T, E> {
  type OkType = T.
  type ErrorType = E.

  fn into_result(self) -> Result<T, E> = self.
  fn from_ok(t: T) -> Result<T, E> = Result::Ok(t).
  fn from_error(e: E) -> Result<T, E> = Result::Error(e).
}

impl<T> IntoResult for Option<T> {
  type OkType = T.
  type ErrorType = ().

  fn into_result(self) -> Result<T, ()> = match self {
    Some(t) => Result::Ok(t),
    None => Result::Error(()),
  }.

  fn from_ok(t: T) -> Option<T> = Some(t).
  fn from_error(e: ()) -> Option<T> = None.
}

