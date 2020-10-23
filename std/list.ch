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
    allocate List { size: 0, root: Option::None, end: Option::None }.

  fn push_front(self, item: T) = {
    self:size = self:size + 1.

    match self:root {
      Option::None => {
        self:root = self:end = Option::Some(Link:new(item)).
      },
      Option::Some(old_root) => {
        let new_root = Link:new(item).
        new_root:link_before(old_root).
        self:root = Option::Some(new_root).
      }
    }
  }.

  fn push_back(self, item: T) = {
    self:size = self:size + 1.

    match self:end {
      Option::None => {
        self:root = self:end = Option::Some(Link:new(item)).
      },
      Option::Some(old_end) => {
        let new_end = Link:new(item).
        new_end:link_after(old_end).
        self:end = Option::Some(new_end).
      }
    }
  }.

  fn pop_front(self) -> Option<T> = {
    match self:root {
      Option::None => Option::None,
      Option::Some(old_root) => {
        let next_root = old_root:next.
        old_root:unlink().

        if next_root:is_none() {
          self:root = self:end = Option::None.
        }

        self:size = self:size - 1.
        Option::Some(old_root:item)
      }
    }
  }.

  fn pop_back(self) -> Option<T> = {
    match self:end {
      Option::None => Option::None,
      Option::Some(old_end) => {
        let next_end = old_end:last.
        old_end:unlink().

        if next_end:is_none() {
          self:root = self:end = Option::None.
        }

        self:size = self:size - 1.
        Option::Some(old_end:item)
      }
    }
  }.

  fn get(self, idx: Int) -> Option<T> = {
    match Self::get_node_internal(self:root, idx) {
      Option::Some(node) => Option::Some(node:item),
      Option::None => Option::None,
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
        Option::Some(node) => {
          let new_link = Link:new(item).
          node:link_after(new_link).
        },
        Option::None => {
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
        Option::Some(node) => {
          node:unlink().
          node:item
        },
        Option::None => {
          panic("Invalid index to insert at: \(idx)")
        },
      }
    }
  }.

  fn get_node_internal(node: Option<Link<T>>, idx: Int) -> Option<Link<T>> = {
    match (node, idx) {
      (node, 0) => node,
      (Option::Some(node), idx) => Self::get_node_internal(node:next, idx - 1),
      (Option::None, _) => Option::None,
    }
  }.
}

impl<T> Self for Link<T> {
  fn new(item: T) -> Link<T> =
    allocate Link { item, last: Option::None, next: Option::None }.

  fn link_after(self, new_next: Link<T>) = {
    let old_next = self:next.
    self:next = Option::Some(new_next).
    new_next:last = Option::Some(self).
    new_next:next = old_next.

    match old_next {
      Option::Some(old_next) => {
        old_next:last = Option::Some(new_next).
      },
      Option::None => {}
    }
  }.

  fn link_before(self, new_last: Link<T>) = {
    let old_last = self:last.
    self:last = Option::Some(new_last).
    new_last:next = Option::Some(self).
    new_last:last = old_last.

    match old_last {
      Option::Some(old_last) => {
        old_last:next = Option::Some(new_last).
      },
      Option::None => {}
    }
  }.

  fn unlink(self) = {
    match self:last {
      Option::Some(last) => {
        last:next = self:next.
      },
      Option::None => {},
    }

    match self:next {
      Option::Some(next) => {
        next:last = self:last.
      },
      Option::None => {},
    }

    self:next = self:last = Option::None.
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
  type Value = T.

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

  fn iterator(self) -> ListIterator<T> = ListIterator::Iterator { link: self:root, size: self:size }.
}

enum ListIterator<T> {
  Iterator {
    link: Option<Link<T>>,
    size: Int,
  },
}

impl<T> Iterator for ListIterator<T> {
  type Item = T.

  fn next(self) -> (Option<T>, ListIterator<T>) = {
    let ListIterator::Iterator { link, size } = self.

    match link {
      Option::Some(link) => (Option::Some(link:item), ListIterator::Iterator { link: link:next, size: size - 1 }),
      Option::None => (Option::None, self),
    }
  }.

  fn has_next(self) -> Bool = {
    let ListIterator::Iterator { link, ... } = self.
    link:is_some()
  }.

  fn size_hint(self) -> Int = {
    let ListIterator::Iterator { size, ... } = self.
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
