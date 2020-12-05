trait Call<Args> {
  type Return.
  fn call(self, args: Args) -> <Self as Call<Args>>::Return.
}