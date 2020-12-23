trait Foo where Self: Concrete {
    fn bar(self).
}

impl Foo for Int {
    fn bar(self) = {
        println("I'm an Int").
    }.
}

fn main() = {
    let i: Dyn<Foo> = todo().
}.