use lang::*.
use iterator::*.
use option::{*, Option::{None, Some}}.
use vector::*.
use list::*.
use hash_map::*.
use threading::*.
use timer::*.

// PLEASE don't call this lmfao
extern fn internal_undefined_value<T>() -> T.

extern fn gc().
extern fn print(s: String).
extern fn unreachable<T>() -> T.
extern fn exit(i: Int).
extern fn breakpoint().
extern fn type_id_of<T>() -> Int where T: Concrete.
extern fn type_string_of<T>() -> String where T: Concrete.

fn println(s: String) = {
  print(s + "\n").
}.

fn todo() -> ! = {
  panic("TODO")
}.

fn unimplemented<T>() -> T = {
  panic("TODO: implement commalipses")
}.

fn assert_impl(b: Bool) = {
  if !b {
    panic:<()>("Assert failed").
  }
}.

fn panic<T>(s: String) -> T = {
  println("PANIC: " + s).
  breakpoint().

  exit(-1).
  unreachable()
}.