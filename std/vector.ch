let default_size: Int = 8.

object Vector<T> {
  array: [T],
  size: Int,
}

impl<T> Self for Vector<T> {
  fn new() -> Vector<T> = {
    allocate Vector {
      array: internal_alloc_empty_array:<T>(8),
      size: 0
    }
  }.

  fn new_with_size_hint(n: Int) -> Vector<T> = {
    n = if n < 0 { 0 } else { n }.

    allocate Vector {
      array: internal_alloc_empty_array:<T>(n),
      size: 0
    }
  }.

  fn new_from<V>(source: V) -> Vector<T> where V: Iterable<Item = T> + Len = {
    let size = source:len().
    let array = internal_alloc_empty_array(size).
    let vec: Vector<_> = allocate Vector { array, size }.

    for (i, t) in source:iterator():enumerate() {
      let tt: T = array[i].
      tt = t.
    }

    vec
  }.

  fn push(self, t: T) = {
    self:ensure_size(self:size + 1).
    self:array[self:size] = t.
    self:size = self:size + 1.
  }.

  fn pop(self) -> Option<T> = {
    if self:size > 0 {
      self:size = self:size - 1.
      let elem = self:array[self:size].

      self:array[self:size] = internal_undefined_value().

      //self:try_downsize().
      Some(elem)
    } else {
      None
    }
  }.

  fn ensure_size(self, n: Int) = {
    if self:array:len() >= n {
      return.
    }

    let new_size = self:array:len().
    while new_size < n {
      new_size = new_size * 2.
    }

    self:array = copy_array_with_size(self:array, new_size).
    assert self:array:len() >= n.
  }.

  fn into_array(self) -> [T] = {
    if self:array:len() != self:size {
      copy_array_with_size(self:array, self:size)
    } else {
      self:array
    }
  }.
}

impl<T> Iterable for Vector<T> {
  type Iterator = VectorIterator<T>.
  type Item = T.

  fn iterator(self) -> VectorIterator<T> = {
    VectorIterator { vector: self, idx: 0 }
  }.
}

struct VectorIterator<T> {
  vector: Vector<T>,
  idx: Int,
}

impl<T> Iterator for VectorIterator<T> {
  type Item = T.

  fn next(self) -> (Option<T>, VectorIterator<T>) = {
    let VectorIterator { vector, idx } = self.

    if idx >= vector:size {
      (None, VectorIterator { vector, idx })
    } else {
      (Some(vector:array[idx]), VectorIterator { vector, idx: idx + 1 })
    }
  }.

  fn size_hint(self) -> Int = {
    let VectorIterator { vector, idx } = self.

    vector:size - idx
  }.
}

impl<T> Len for Vector<T> {
    fn len(self) -> Int = self:size.
}

impl<T> Deref<Int> for Vector<T> {
  type Result = T.

  fn deref(self, idx: Int) -> T = {
    if idx < 0 | idx >= self:len() {
      panic("Index \(idx) out of bounds. Size is \(self:len())::").
    }

    self:array[idx]
  }.
}

impl<T> DerefAssign<Int> for Vector<T> {
  fn deref_assign(self, idx: Int, value: T) -> T = {
    if idx < 0 | idx >= self:len() {
      panic("Index \(idx) out of bounds. Size is \(self:len())::").
    }

    self:array[idx] = value.
    value
  }.
}

impl<T> Into<String> for Vector<T> where T: Into<String> {
  fn into(self) -> String = {
    let s = "Vector[".
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

impl<I> FromIterator<I> for Vector<I> {
  fn from_iterator<It>(it: It) -> Vector<I> where It: Iterator<Item=I> = {
    let v = Vector::new_with_size_hint(it:size_hint()).

    for i in it {
      v:push(i).
    }

    v
  }.
}

fn copy_array_with_size<T>(input: [T], new_size: Int) -> [T] = {
  let output = internal_alloc_empty_array(new_size).

  for (i, n) in input:iterator():limit(new_size):enumerate() {
    output[i] = n.
  }

  output
}.