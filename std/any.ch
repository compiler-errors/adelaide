trait Any {}

trait Downcast {
  fn try_downcast<T>(self) -> Option<T>.
}

trait DowncastExt {
  fn downcast<T>(self) -> T.
  fn is<T>(self) -> Bool.
}

impl<S> DowncastExt for S where S: Downcast {
  fn downcast<T>(self) -> T = {
    self:try_downcast():expect("Could not unwrap \(type_string:<S>()) as \(type_string:<T>())")
  }.

  fn is<T>(self) -> Bool = {
    self:try_downcast:<T>():is_some()
  }.
}