fn main() = {
    (async {
        let stream = async_gen().

        while let (Some(item), new_stream) = stream:next():await {
            stream = new_stream.
            println("Yielded \(item)").
        }
    }):wait()
}.

async fn async_main() = {
    
}.

fn async_gen() -> AsyncGenerator<Int> =
    // As we can see here, `async gen` lets you await AND yield!
    (async gen {
        for i in 0..10 {
            async_wait_millis(i * 200):await.
            yield i.
        }
    }):stream().