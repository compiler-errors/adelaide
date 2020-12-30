fn main() = {
    // Do all of our logic in here
    let async_main = async {
        let stream = async_gen(10).

        while let (Some(item), new_stream) = stream:next():await {
            stream = new_stream.
            println("Yielded \(item)").
        }
    }.

    // Then just synchronously "await" it...
    async_main:wait().
}.

fn async_gen(limit: Int) -> AsyncGenerator<Int> = {
    // As we can see here, `async gen` lets you await AND yield!
    let generator = async gen {
        for i in 0..limit {
            async_wait_millis(i * 200):await.
            yield i.
        }
    }.

    // Turn it into a stream
    generator:stream()
}