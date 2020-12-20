object Awaitable<T>.

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

        loop {
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

struct VectorPoll<T> where T: Poll (Vector<Either<T, <T as Poll>::Result>>).

impl<T> Join for Vector<T> where T: Poll {
    type Result = Vector<<T as Poll>::Result>.
    type Joined = VectorPoll<T>.

    fn join(self) -> VectorPoll<T> =
        VectorPoll(self:iterator():map(|i| Either::Left(i)):collect()).
}

impl<T> Poll for VectorPoll<T> where T: Poll {
    type Result = Vector<<T as Poll>::Result>.

    fn poll(self) -> (PollState<Vector<<T as Poll>::Result>>, Self) = {
        let vec = self:0.
        let unfinished = false.

        for i in 0..vec:len() {
            match vec[i] {
                Either::Left(p) => {
                    vec[i] = match p:poll() {
                        (PollState::Incomplete, p) => {
                            unfinished = true.
                            Either::Left(p)
                        },
                        (PollState::Complete(i), _) => Either::Right(i),
                    }.
                },
                Either::Right(_) => { /* Do nothing */ }
            }
        }

        if unfinished {
            (PollState::Incomplete, self)
        } else {
            (PollState::Complete(vec:iterator():map(|i| i:unwrap_right()):collect()), self)
        }
    }.
}
