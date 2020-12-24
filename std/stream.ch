trait PollStream {
  type Item.

  fn poll_stream(self) -> (PollStreamState<<Self as PollStream>::Item>, Self).
}

enum PollStreamState<T> {
  Incomplete,
  Yield(T),
  Complete,
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