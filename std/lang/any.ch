// A marker trait for all non-dynamic types
trait Concrete {}

trait Any {
  fn type_id(self) -> Int.
}

impl<T> Any for T where T: Concrete {
  fn type_id(self) -> Int = {
    todo()
  }.
}

impl<S> Self for S where S: Any {
  fn try_downcast<T>(self) -> Option<T> where T: Concrete = {
    todo()
  }.

  fn downcast<T>(self) -> T  where T: Concrete = {
    self:try_downcast():expect("Could not unwrap Any as type \(type_string:<T>()). The boxed value is type \(self:type_string()).")
  }.

  fn is<T>(self) -> Bool where T: Concrete = {
    self:try_downcast:<T>():is_some()
  }.

  fn type_string(self) -> String = {
    todo()
  }.
}

fn type_id_of<T>() -> Int where T: Concrete = {
  todo()
}.