extern fn internal_generator_unpark<I, Y, F>(g: Generator<I, Y, F>, i: I) -> (YieldState<Y, F>, Generator<I, Y, F>).

trait Yield {
    type In.
    type Yield.
    type Complete.

    fn resume(self, i: <Self as Yield>::In) -> (YieldState<<Self as Yield>::Yield, <Self as Yield>::Complete>, Self).
}

enum YieldState<Y, F> {
    Yield(Y),
    Complete(F),
}

opaque object Generator<I, Y, F>.

impl<I, Y, F> Yield for Generator<I, Y, F> {
    type In = I.
    type Yield = Y.
    type Complete = F.

    fn resume(self, i: I) -> (YieldState<Y, F>, Self) =
      internal_generator_unpark(self, i).
}

impl<Y> Iterable for Generator<(), Y, ()> {
    type Iterator = GeneratorIterator<Y>.
    type Item = Y.

    fn iterator(self) -> GeneratorIterator<Y> = {
        GeneratorIterator(self)
    }.
}

struct GeneratorIterator<Y>(Generator<(), Y, ()>).

impl<Y> Iterator for GeneratorIterator<Y> {
    type Item = Y.

    fn next(self) -> (Option<Y>, Self) = {
        match self:0:resume(()) {
            (YieldState::Yield(item), new_self) =>
                (Some(item), GeneratorIterator(new_self)),
            (YieldState::Complete(()), new_self) =>
                (None, GeneratorIterator(new_self)),
        }
    }.

    fn size_hint(self) -> Int = 0.
}

enum AsyncGeneratorState<Y> {
  Incomplete,
  Yield(Y),
}