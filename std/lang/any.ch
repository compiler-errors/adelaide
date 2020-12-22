// Unwrap any number of Dyn layers and return the concrete value behind it
extern fn internal_unbox_transmute<T, S>(t: T) -> S.

// A marker trait for all non-dynamic types
trait Concrete {}

trait Any {
  fn type_id(self) -> Int.
  fn type_string(self) -> String.
}

impl<T> Any for T where T: Concrete {
  fn type_id(self) -> Int = {
    type_id_of:<T>()
  }.

  fn type_string(self) -> String = {
    type_string_of:<T>()
  }.
}

impl<S> Self for S where S: Any {
  fn try_downcast<T>(self) -> Option<T> where T: Concrete = {
    if self:type_id() == type_id_of:<T>() {
      Some(internal_unbox_transmute(self))
    } else {
      None
    }
  }.

  fn downcast<T>(self) -> T where T: Concrete = {
    self:try_downcast():expect("Could not unwrap Any as type \(type_string_of:<T>()). The boxed value is type \(self:type_string()).")
  }.

  fn is<T>(self) -> Bool where T: Concrete = {
    self:type_id() == type_id_of:<T>()
  }.
}