object Awaitable<T> {}

trait Poll {
    type Result.

    fn poll(self) -> (PollState<<Self as Poll>::Result>, Self).
}

enum PollState<T> {
    Incomplete,
    Complete(T),
}

impl<T> Poll for Awaitable<T> {
    type Result = T.

     fn poll(self) -> (PollState<T>, Self) = {
        let (poll_state, new_self) = todo().

        // NOTE: self is a heap object (heap awaitable), so
        // we can just return it by reference.
        (poll_state, new_self)
     }.
}

trait Wait where Self: Poll {
    fn wait(self) -> <Self as Poll>::Result.
}

impl<T> Wait for T where T: Poll {
    fn wait(self) -> <Self as Poll>::Result = {
        let pollable = self.

        while true {
            let (state, new_pollable) = pollable:poll().

            match state {
                PollState::Complete(value) => return value,
                PollState::Incomplete => {},
            }

            pollable = new_pollable.
        }

        unreachable()
    }.
}

trait Join {
    type Result.
    type Joined: Poll<Result = <Self as Join>::Result>.

    fn join(self) -> <Self as Join>::Joined.
}
