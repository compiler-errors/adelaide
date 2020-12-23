trait Foo {
    type Bar.
}

impl Foo for Int {
    type Bar = ().
}

let x: () = {
    let y: Dyn<Foo<Bar = Float>> = 1:into().
}.