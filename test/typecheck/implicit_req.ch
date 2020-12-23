struct Womp.

trait Bar {}

trait Foo {
    fn baz<Z>() where (Self, Z): Bar.
}

impl<T> Bar for (Womp, T) {}

impl Foo for Womp {
    fn baz<S>() where (Self, S): Bar = ().
}