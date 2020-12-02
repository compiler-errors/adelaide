// pass

fn foo() -> Int = loop {}.

fn bar() -> Int = {
    if true {
        1
    } else {
        loop {}
    }
}.