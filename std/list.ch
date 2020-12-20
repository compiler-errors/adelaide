object List<T> {
  size: Int,
  root: Option<Link<T>>,
  end: Option<Link<T>>,
}

object Link<T> {
  item: T,
  last: Option<Link<T>>,
  next: Option<Link<T>>,
}

impl<T> Self for List<T> {
  fn new() -> List<T> =
    allocate List { size: 0, root: None, end: None }.

  fn push_front(self, item: T) = {
    self:size = self:size + 1.

    match self:root {
      None => {
        self:root = self:end = Some(Link::new(item)).
      },
      Some(old_root) => {
        let new_root = Link::new(item).
        new_root:link_before(old_root).
        self:root = Some(new_root).
      }
    }
  }.

  fn push_back(self, item: T) = {
    self:size = self:size + 1.

    match self:end {
      None => {
        self:root = self:end = Some(Link::new(item)).
      },
      Some(old_end) => {
        let new_end = Link::new(item).
        new_end:link_after(old_end).
        self:end = Some(new_end).
      }
    }
  }.

  fn pop_front(self) -> Option<T> = {
    match self:root {
      None => None,
      Some(old_root) => {
        let next_root = old_root:next.
        old_root:unlink().

        if next_root:is_none() {
          self:root = self:end = None.
        }

        self:size = self:size - 1.
        Some(old_root:item)
      }
    }
  }.

  fn pop_back(self) -> Option<T> = {
    match self:end {
      None => None,
      Some(old_end) => {
        let next_end = old_end:last.
        old_end:unlink().

        if next_end:is_none() {
          self:root = self:end = None.
        }

        self:size = self:size - 1.
        Some(old_end:item)
      }
    }
  }.

  fn get(self, idx: Int) -> Option<T> = {
    match Self::get_node_internal(self:root, idx) {
      Some(node) => Some(node:item),
      None => None,
    }
  }.

  fn put(self, idx: Int, item: T) = {
    if idx == 0 {
      self:push_front(item).
    } else if idx == self:size {
      self:push_back(item).
    } else if idx < 0 {
      panic:<()>("Invalid index to insert at: \(idx)").
    } else {
      match Self::get_node_internal(self:root, idx - 1) {
        Some(node) => {
          let new_link = Link::new(item).
          node:link_after(new_link).
        },
        None => {
          panic:<()>("Invalid index to insert at: \(idx)").
        },
      }
    }
  }.

  fn remove(self, idx: Int) -> T = {
    if idx < 0 | idx >= self:size {
      panic:<()>("Invalid index to insert at: \(idx)").
    }

    if idx == 0 {
      self:pop_front():unwrap()
    } else if idx == self:size - 1 {
      self:pop_back():unwrap()
    } else {
      match Self::get_node_internal(self:root, idx - 1) {
        Some(node) => {
          node:unlink().
          node:item
        },
        None => {
          panic("Invalid index to insert at: \(idx)")
        },
      }
    }
  }.

  fn get_node_internal(node: Option<Link<T>>, idx: Int) -> Option<Link<T>> = {
    match (node, idx) {
      (node, 0) => node,
      (Some(node), idx) => Self::get_node_internal(node:next, idx - 1),
      (None, _) => None,
    }
  }.
}

impl<T> Self for Link<T> {
  fn new(item: T) -> Link<T> =
    allocate Link { item, last: None, next: None }.

  fn link_after(self, new_next: Link<T>) = {
    let old_next = self:next.
    self:next = Some(new_next).
    new_next:last = Some(self).
    new_next:next = old_next.

    match old_next {
      Some(old_next) => {
        old_next:last = Some(new_next).
      },
      None => {}
    }
  }.

  fn link_before(self, new_last: Link<T>) = {
    let old_last = self:last.
    self:last = Some(new_last).
    new_last:next = Some(self).
    new_last:last = old_last.

    match old_last {
      Some(old_last) => {
        old_last:next = Some(new_last).
      },
      None => {}
    }
  }.

  fn unlink(self) = {
    match self:last {
      Some(last) => {
        last:next = self:next.
      },
      None => {},
    }

    match self:next {
      Some(next) => {
        next:last = self:last.
      },
      None => {},
    }

    self:next = self:last = None.
  }.

  fn get(self) -> T = self:item.

  fn set(self, t: T) = {
    self:item = t.
  }.
}

impl<T> Len for List<T> {
  fn len(self) -> Int = self:size.
}

impl<T> Deref<Int> for List<T> {
  type Result = T.

  fn deref(self, idx: Int) -> T = {
    if idx < 0 | idx >= self:len() {
      panic:<()>("Index \(idx) out of bounds. Size is \(self:len())::").
    }

    Self::get_node_internal(self:root, idx):unwrap():item
  }.
}

impl<T> DerefAssign<Int> for List<T> {
  fn deref_assign(self, idx: Int, value: T) -> T = {
    if idx >= self:len() {
      panic:<()>("Index \(idx) out of bounds. Size is \(self:len())::").
    }

    Self::get_node_internal(self:root, idx):unwrap():item = value.
    value
  }.
}

impl<T> Iterable for List<T> {
  type Iterator = ListIterator<T>.
  type Item = T.

  fn iterator(self) -> ListIterator<T> = ListIterator { link: self:root, size: self:size }.
}

struct ListIterator<T> {
  link: Option<Link<T>>,
  size: Int,
}

impl<T> Iterator for ListIterator<T> {
  type Item = T.

  fn next(self) -> (Option<T>, ListIterator<T>) = {
    let ListIterator { link, size } = self.

    match link {
      Some(link) => (Some(link:item), ListIterator { link: link:next, size: size - 1 }),
      None => (None, self),
    }
  }.

  fn size_hint(self) -> Int = {
    let ListIterator { size, ... } = self.
    size
  }.
}

impl<T> Into<String> for List<T> where T: Into<String> {
  fn into(self) -> String = {
    let s = "List[".
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
