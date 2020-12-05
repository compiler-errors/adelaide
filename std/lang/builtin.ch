extern fn internal_alloc_empty_array<T>(n: Int) -> [T].

// ----------------------------------- //

impl<T> Self for T {
    fn to<S>(self) -> S where T: Into<S> = {
        <Self as Into<S>>::into(self)
    }.
}

// ----------------------------------- //

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