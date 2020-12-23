fn infallible<T>(t: T) -> Result<T, !> = Result::Ok(t).

fn unwrap_infallible() -> Int = {
    infallible(1)?
}.