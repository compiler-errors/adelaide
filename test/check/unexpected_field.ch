// fail

struct A {
  a: Int,
}

fn bar() -> A = {
  A { a: 1, b: 0 }
}.