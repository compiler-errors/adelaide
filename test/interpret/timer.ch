async fn main() = {
    println("Starting.").
    
    println("\nFirst, let's await two timers 2000ms + 1000ms in serial").
    let start = Now::now().
    async_wait_millis(2000):await.
    async_wait_millis(1000):await.
    println("And we see that approx 3 seconds should have elapsed: \(start:elapsed_secs())").

    println("\nNext, let's await two timers 2000ms + 1000ms in PARALLEL").
    let start = Now::now().
    // Join the two awaitables so we're awaiting them at once..
    (async_wait_millis(2000), async_wait_millis(1000)):join():await.
    println("And we see that approx 2 seconds should have elapsed: \(start:elapsed_secs())").
}.