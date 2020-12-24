extern fn internal_thread_spawn<F, T>(trampoline: fn(F) -> T, call: F) -> ThreadHandle<T> where F: Fn() -> T.
extern fn internal_thread_current() -> Thread.
extern fn internal_thread_count() -> Int.
extern fn internal_thread_switch().
extern fn internal_thread_complete(t: Thread) -> Bool.
extern fn internal_thread_block_on(id: Int).

object Thread {
  id: Int,
}

object ThreadHandle<T> {
  // We store this in a single tuple because we need
  // to make sure we can index the Option<T> return
  // value stably, which might vary depending on how
  // Id<str> hashes...
  thread_and_return: (Thread, Option<T>),
}

fn call_trampoline<F, T>(f: F) -> T where F: Fn() -> T = {
  f()
}.

impl Self for Thread {
  fn spawn<F, T>(f: F) -> ThreadHandle<T> where F: Fn() -> T = {
    internal_thread_spawn(call_trampoline:<F, T>, f)
  }.

  // Returns the currently running thread.
  fn current() -> Thread = {
    internal_thread_current()
  }.

  fn thread_count() -> Int = {
    internal_thread_count()
  }.

  fn switch() = {
    internal_thread_switch().
  }.

  fn is_complete(self) -> Bool = {
    internal_thread_complete(self)
  }.

  fn join(self) = {
    if self:id == Thread::current():id {
      panic("Cannot Thread::join() on the running thread").
    }

    while !self:is_complete() {
      internal_thread_block_on(self:id).
    }
  }.

  // Waits until all threads are finished. This is only allowed to be executed
  // on the main thread, will panic otherwise.
  fn coalesce() = {
    if Thread::current():id != 0 {
      panic("Can only call Thread::coalesce on the main thread").
    }

    while Thread::thread_count() > 1 {
      Thread::switch().
    }
  }.
}

impl<T> Self for ThreadHandle<T> {
  fn join(self) -> T = {
    self:thread_and_return:0:join().

    self:thread_and_return:1:unwrap()
  }.
}