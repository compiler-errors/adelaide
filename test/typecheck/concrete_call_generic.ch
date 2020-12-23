trait Foo {
    fn bar(self) where Self: Concrete.
}

fn baz<T>(t: T) where T: Foo + Concrete = {
    t:bar().
}.