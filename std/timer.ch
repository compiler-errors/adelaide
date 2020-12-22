extern fn now_millis() -> Int.

fn wait_millis(millis: Int) = {
    let end = now_millis() + millis.

    while now_millis() < end {
        Thread::yield().
    }
}.

struct Now(Int).

impl Self for Now {
    fn now() -> Now = {
        Now(now_millis())
    }.

    fn elapsed_millis(self) -> Int = {
        now_millis() - self:0
    }.

    fn elapsed_secs(self) -> Float = {
        self:elapsed_millis():to:<Float>() / 1000.0
    }.
}

struct TimerFuture(Int).

impl Poll for TimerFuture {
    type Result = ().

    fn poll(self) -> (PollState<()>, Self) = {
        if now_millis() < self:0 {
            (PollState::Incomplete, self)
        } else {
            (PollState::Complete(()), self)
        }
    }.
}

fn async_wait_millis(millis: Int) -> TimerFuture = {
    TimerFuture(now_millis() + millis)
}.