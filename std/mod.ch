pub use any::*.
pub use operators::*.
pub use iterator::*.
pub use option::{*, Option::{None, Some}}.
pub use try::*.
pub use vector::*.
pub use list::*.
pub use hash_map::*.
pub use threading::*.
pub use asynchronous::*.

extern fn gc().
extern fn print(s: String).
extern fn unreachable<T>() -> T.
extern fn type_string<T>() -> String.
extern fn exit(i: Int).
extern fn breakpoint().


fn println(s: String) = {
  print(s + "\n").
}.

fn panic<T>(s: String) -> T = {
  println("PANIC: " + s).
  breakpoint().

  exit(-1).
  unreachable()
}.

fn type_string_of<T>(t: T) -> String = {
  type_string:<T>()
}.