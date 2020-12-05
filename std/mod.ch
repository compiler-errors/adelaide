use any::*.
use lang::*.
use iterator::*.
use option::{*, Option::{None, Some}}.
use vector::*.
use list::*.
use hash_map::*.
use threading::*.

extern fn gc().
extern fn print(s: String).
extern fn unreachable<T>() -> T.
extern fn type_string<T>() -> String.
extern fn exit(i: Int).
extern fn breakpoint().

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

fn type_string_of<T>(t: T) -> String = {
  type_string:<T>()
}.