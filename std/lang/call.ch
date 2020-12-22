extern fn internal_call<F, Args, Return>(f: F, args: Args) -> Return where F: Call<Args, Return=Return>.

trait Call<Args> {
  type Return.
  fn call(self, args: Args) -> <Self as Call<Args>>::Return.
}