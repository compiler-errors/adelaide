fn main() = {
    let g = gen {
        yield 1.
        yield 2.
        yield 3.
        yield 4.
    }.

    g:iterator().
}.