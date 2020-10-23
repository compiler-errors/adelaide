object Thread {
  id: Int,
  completed: Option<Dyn<Any>>,
}

fn call_trampoline<F, T>(f: F) -> T where F: Fn() -> T = {
  f()
}.

impl Self for Thread {
  fn spawn<F, T>(f: F) -> Thread where F: Fn() -> T = {
    let c = call_trampoline:<F, T>.
    todo()
  }.

  // Returns the currently running thread.
  fn current() -> Thread = {
    todo()
  }.

  // Wait for a thread to complete, return the value yielded by the thread.
  fn join(self) -> Dyn<Any> = {
    if self:id == Thread:current():id {
      panic:<()>("Cannot Thread:join() on the running thread::").
    }

    while true {
      if let Some(value) = self:completed {
        break value.
      } else {
        todo()
      }
    } else {
      unreachable()
    }
  }.

  // Convenience function. Same as Thread:join, but explicitly downcast.
  // Panics if the joined value isn't the specified concrete type.
  fn join_as<T>(self) -> T = {
    self:join():downcast()
  }.

  // Waits until all threads are finished. This is only allowed to be executed
  // on the main thread, will panic otherwise.
  fn coalesce() = {
    if Thread:current():id != 0 {
      panic:<()>("Can only call Thread:coalesce on the main thread").
    }

    while (todo()) > 1 {
      todo().
    }
  }.

  fn yield() = todo().
}