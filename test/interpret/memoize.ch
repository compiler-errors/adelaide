object Memoize<Args, Return> {
  fun: Dyn<Call<Args, Return=Return>>,
  cache: HashMap<Args, Return>,
}

impl<Args, Return> Self for Memoize<Args, Return> {
  fn new<F>(fun: F) -> Self where F: Call<Args, Return=Return> =
    allocate Memoize { fun: fun:into(), cache: HashMap::new() }.
}

impl<Args, Return> Call<Args> for Memoize<Args, Return> where Args: Hash + Equals<Args> + Into<String>, Return: Into<String> {
  type Return = Return.

  fn call(self, args: Args) -> Return = {
    if let Some(ret) = self:cache:get(args) {
      println("Cached \(args), result = \(ret)").
      ret
    } else {
      println("No entry for \(args).").
      let ret = (self:fun):call(args).
      println("Caching \(args), result = \(ret)").
      self:cache[args] = ret.
      ret
    }
  }.
}

fn fib(i: Int) -> Int = {
  match i {
    0 => 0,
    1 => 1,
    n => fib(n - 1) + fib(n - 2),
  }
}.

let fib_memo: Memoize<(Int,), Int> = Memoize::new(|i: Int| -> Int {
  match i {
    0 => 0,
    1 => 1,
    n => fib_memo(n - 1) + fib_memo(n - 2),
  }
}).

fn main() = {
  for x in 0..28 {
    println("fib(\(x)) = \(fib(x))").
  }

  for x in 0..28 {
    println("fib_memo(\(x)) = \(fib_memo(x))").
  }
}.