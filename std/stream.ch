trait PollStream {
  type Item.

  fn poll_stream(self) -> (PollStreamState<<Self as PollStream>::Item>, Self).
}

enum PollStreamState<T> {
  Incomplete,
  Yield(T),
  Complete,
}

impl<T> Self for Generator<(), AsyncGeneratorState<T>, ()> {
  fn stream(self) -> AsyncGenerator<T> = AsyncGenerator(self).
}

struct AsyncGenerator<T>(Generator<(), AsyncGeneratorState<T>, ()>).

impl<T> PollStream for AsyncGenerator<T> {
  type Item = T.

  fn poll_stream(self) -> (PollStreamState<T>, Self) = {
    match self:0:resume(()) {
      (YieldState::Yield(AsyncGeneratorState::Yield(item)), new_self) =>
        (PollStreamState::Yield(item), AsyncGenerator(new_self)),
      (YieldState::Yield(AsyncGeneratorState::Incomplete), new_self) =>
        (PollStreamState::Incomplete, AsyncGenerator(new_self)),
      (YieldState::Complete(()), new_self) =>
        (PollStreamState::Complete, AsyncGenerator(new_self)),
    }
  }.
}

impl<T> Self for T where T: PollStream {
  fn next(self) -> StreamFuture<T> = StreamFuture(self).
}

struct StreamFuture<T>(T).

impl<T> Poll for StreamFuture<T> where T: PollStream {
  type Result = (Option<<T as PollStream>::Item>, T).

  fn poll(self) -> (PollState<<Self as Poll>::Result>, Self) =
    match self:0:poll_stream() {
      (PollStreamState::Incomplete, new_stream) =>
        (PollState::Incomplete, StreamFuture(new_stream)),
      (PollStreamState::Yield(item), new_stream) =>
        (PollState::Complete((Some(item), new_stream)), StreamFuture(new_stream)),
      (PollStreamState::Complete, new_stream) =>
        (PollState::Complete((None, new_stream)), StreamFuture(new_stream)),
    }.
}