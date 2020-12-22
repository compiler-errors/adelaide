
impl<Ret> Call<()> for || -> Ret {
  type Return = Ret.

  fn call(self, args: ()) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret> Call<()> for fn() -> Ret {
  type Return = Ret.

  fn call(self, args: ()) -> Ret = {
    internal_call(self, args)
  }.
}

impl Into<String> for () {
  fn into(self) -> String = {
    "()"
  }.
}

impl Hash for () {
  fn hash(self) -> Int = {
    let h = 7.

    h
  }.
}

impl Equals<()> for () {
  fn eq(self, other: Self) -> Bool = {
    true
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join0.

impl Join for () {
  type Result = ().
  type Joined = Join0.

  fn join(self) -> Join0 =
    Join0.
}

impl Poll for Join0 {
  type Result = ().

  fn poll(self) -> (PollState<()>, Self) = match self {
    Join0 => {
      let success = true.

      let new_self = Join0.

      if success {
        (PollState::Complete(()), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}


impl<Ret, A> Call<(A,)> for |A| -> Ret {
  type Return = Ret.

  fn call(self, args: (A,)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret, A> Call<(A,)> for fn(A) -> Ret {
  type Return = Ret.

  fn call(self, args: (A,)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<A> Into<String> for (A,) where A: Into<String> {
  fn into(self) -> String = {
    "(\(self:0),)"
  }.
}

impl<A> Hash for (A,) where A: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
    h
  }.
}

impl<A> Equals<(A,)> for (A,) where A: Equals<A> {
  fn eq(self, other: Self) -> Bool = {
    self:0:eq(other:0)
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join1<A, PA>(Either<PA, A>).

impl<A, PA> Join for (PA,) where PA: Poll<Result=A> {
  type Result = (A,).
  type Joined = Join1<A, PA>.

  fn join(self) -> Join1<A, PA> =
    Join1(Either::Left(self:0)).
}

impl<A, PA> Poll for Join1<A, PA> where PA: Poll<Result=A> {
  type Result = (A,).

  fn poll(self) -> (PollState<(A,)>, Self) = match self {
    Join1(join0) => {
      let success = true.

      let join0 = match join0 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.

      let new_self = Join1(join0).

      if success {
        (PollState::Complete((join0:unwrap_right(),)), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}


impl<Ret, A, B> Call<(A, B)> for |A, B| -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret, A, B> Call<(A, B)> for fn(A, B) -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<A, B> Into<String> for (A, B) where A: Into<String>, B: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1))"
  }.
}

impl<A, B> Hash for (A, B) where A: Hash, B: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
    h
  }.
}

impl<A, B> Equals<(A, B)> for (A, B) where A: Equals<A>, B: Equals<B> {
  fn eq(self, other: Self) -> Bool = {
    self:0:eq(other:0) &? self:1:eq(other:1)
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join2<A, B, PA, PB>(Either<PA, A>, Either<PB, B>).

impl<A, B, PA, PB> Join for (PA, PB) where PA: Poll<Result=A>, PB: Poll<Result=B> {
  type Result = (A, B).
  type Joined = Join2<A, B, PA, PB>.

  fn join(self) -> Join2<A, B, PA, PB> =
    Join2(Either::Left(self:0), Either::Left(self:1)).
}

impl<A, B, PA, PB> Poll for Join2<A, B, PA, PB> where PA: Poll<Result=A>, PB: Poll<Result=B> {
  type Result = (A, B).

  fn poll(self) -> (PollState<(A, B)>, Self) = match self {
    Join2(join0, join1) => {
      let success = true.

      let join0 = match join0 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join1 = match join1 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.

      let new_self = Join2(join0, join1).

      if success {
        (PollState::Complete((join0:unwrap_right(), join1:unwrap_right())), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}


impl<Ret, A, B, C> Call<(A, B, C)> for |A, B, C| -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret, A, B, C> Call<(A, B, C)> for fn(A, B, C) -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<A, B, C> Into<String> for (A, B, C) where A: Into<String>, B: Into<String>, C: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2))"
  }.
}

impl<A, B, C> Hash for (A, B, C) where A: Hash, B: Hash, C: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
    h
  }.
}

impl<A, B, C> Equals<(A, B, C)> for (A, B, C) where A: Equals<A>, B: Equals<B>, C: Equals<C> {
  fn eq(self, other: Self) -> Bool = {
    self:0:eq(other:0) &? self:1:eq(other:1) &? self:2:eq(other:2)
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join3<A, B, C, PA, PB, PC>(Either<PA, A>, Either<PB, B>, Either<PC, C>).

impl<A, B, C, PA, PB, PC> Join for (PA, PB, PC) where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C> {
  type Result = (A, B, C).
  type Joined = Join3<A, B, C, PA, PB, PC>.

  fn join(self) -> Join3<A, B, C, PA, PB, PC> =
    Join3(Either::Left(self:0), Either::Left(self:1), Either::Left(self:2)).
}

impl<A, B, C, PA, PB, PC> Poll for Join3<A, B, C, PA, PB, PC> where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C> {
  type Result = (A, B, C).

  fn poll(self) -> (PollState<(A, B, C)>, Self) = match self {
    Join3(join0, join1, join2) => {
      let success = true.

      let join0 = match join0 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join1 = match join1 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join2 = match join2 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.

      let new_self = Join3(join0, join1, join2).

      if success {
        (PollState::Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right())), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}


impl<Ret, A, B, C, D> Call<(A, B, C, D)> for |A, B, C, D| -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret, A, B, C, D> Call<(A, B, C, D)> for fn(A, B, C, D) -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<A, B, C, D> Into<String> for (A, B, C, D) where A: Into<String>, B: Into<String>, C: Into<String>, D: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3))"
  }.
}

impl<A, B, C, D> Hash for (A, B, C, D) where A: Hash, B: Hash, C: Hash, D: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
    h
  }.
}

impl<A, B, C, D> Equals<(A, B, C, D)> for (A, B, C, D) where A: Equals<A>, B: Equals<B>, C: Equals<C>, D: Equals<D> {
  fn eq(self, other: Self) -> Bool = {
    self:0:eq(other:0) &? self:1:eq(other:1) &? self:2:eq(other:2) &? self:3:eq(other:3)
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join4<A, B, C, D, PA, PB, PC, PD>(Either<PA, A>, Either<PB, B>, Either<PC, C>, Either<PD, D>).

impl<A, B, C, D, PA, PB, PC, PD> Join for (PA, PB, PC, PD) where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D> {
  type Result = (A, B, C, D).
  type Joined = Join4<A, B, C, D, PA, PB, PC, PD>.

  fn join(self) -> Join4<A, B, C, D, PA, PB, PC, PD> =
    Join4(Either::Left(self:0), Either::Left(self:1), Either::Left(self:2), Either::Left(self:3)).
}

impl<A, B, C, D, PA, PB, PC, PD> Poll for Join4<A, B, C, D, PA, PB, PC, PD> where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D> {
  type Result = (A, B, C, D).

  fn poll(self) -> (PollState<(A, B, C, D)>, Self) = match self {
    Join4(join0, join1, join2, join3) => {
      let success = true.

      let join0 = match join0 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join1 = match join1 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join2 = match join2 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join3 = match join3 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.

      let new_self = Join4(join0, join1, join2, join3).

      if success {
        (PollState::Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right())), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}


impl<Ret, A, B, C, D, E> Call<(A, B, C, D, E)> for |A, B, C, D, E| -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D, E)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret, A, B, C, D, E> Call<(A, B, C, D, E)> for fn(A, B, C, D, E) -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D, E)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<A, B, C, D, E> Into<String> for (A, B, C, D, E) where A: Into<String>, B: Into<String>, C: Into<String>, D: Into<String>, E: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3), \(self:4))"
  }.
}

impl<A, B, C, D, E> Hash for (A, B, C, D, E) where A: Hash, B: Hash, C: Hash, D: Hash, E: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
        h = 31 * h + self:4:hash().
    h
  }.
}

impl<A, B, C, D, E> Equals<(A, B, C, D, E)> for (A, B, C, D, E) where A: Equals<A>, B: Equals<B>, C: Equals<C>, D: Equals<D>, E: Equals<E> {
  fn eq(self, other: Self) -> Bool = {
    self:0:eq(other:0) &? self:1:eq(other:1) &? self:2:eq(other:2) &? self:3:eq(other:3) &? self:4:eq(other:4)
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join5<A, B, C, D, E, PA, PB, PC, PD, PE>(Either<PA, A>, Either<PB, B>, Either<PC, C>, Either<PD, D>, Either<PE, E>).

impl<A, B, C, D, E, PA, PB, PC, PD, PE> Join for (PA, PB, PC, PD, PE) where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D>, PE: Poll<Result=E> {
  type Result = (A, B, C, D, E).
  type Joined = Join5<A, B, C, D, E, PA, PB, PC, PD, PE>.

  fn join(self) -> Join5<A, B, C, D, E, PA, PB, PC, PD, PE> =
    Join5(Either::Left(self:0), Either::Left(self:1), Either::Left(self:2), Either::Left(self:3), Either::Left(self:4)).
}

impl<A, B, C, D, E, PA, PB, PC, PD, PE> Poll for Join5<A, B, C, D, E, PA, PB, PC, PD, PE> where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D>, PE: Poll<Result=E> {
  type Result = (A, B, C, D, E).

  fn poll(self) -> (PollState<(A, B, C, D, E)>, Self) = match self {
    Join5(join0, join1, join2, join3, join4) => {
      let success = true.

      let join0 = match join0 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join1 = match join1 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join2 = match join2 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join3 = match join3 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join4 = match join4 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.

      let new_self = Join5(join0, join1, join2, join3, join4).

      if success {
        (PollState::Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right(), join4:unwrap_right())), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}


impl<Ret, A, B, C, D, E, F> Call<(A, B, C, D, E, F)> for |A, B, C, D, E, F| -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D, E, F)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret, A, B, C, D, E, F> Call<(A, B, C, D, E, F)> for fn(A, B, C, D, E, F) -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D, E, F)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<A, B, C, D, E, F> Into<String> for (A, B, C, D, E, F) where A: Into<String>, B: Into<String>, C: Into<String>, D: Into<String>, E: Into<String>, F: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3), \(self:4), \(self:5))"
  }.
}

impl<A, B, C, D, E, F> Hash for (A, B, C, D, E, F) where A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
        h = 31 * h + self:4:hash().
        h = 31 * h + self:5:hash().
    h
  }.
}

impl<A, B, C, D, E, F> Equals<(A, B, C, D, E, F)> for (A, B, C, D, E, F) where A: Equals<A>, B: Equals<B>, C: Equals<C>, D: Equals<D>, E: Equals<E>, F: Equals<F> {
  fn eq(self, other: Self) -> Bool = {
    self:0:eq(other:0) &? self:1:eq(other:1) &? self:2:eq(other:2) &? self:3:eq(other:3) &? self:4:eq(other:4) &? self:5:eq(other:5)
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join6<A, B, C, D, E, F, PA, PB, PC, PD, PE, PF>(Either<PA, A>, Either<PB, B>, Either<PC, C>, Either<PD, D>, Either<PE, E>, Either<PF, F>).

impl<A, B, C, D, E, F, PA, PB, PC, PD, PE, PF> Join for (PA, PB, PC, PD, PE, PF) where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D>, PE: Poll<Result=E>, PF: Poll<Result=F> {
  type Result = (A, B, C, D, E, F).
  type Joined = Join6<A, B, C, D, E, F, PA, PB, PC, PD, PE, PF>.

  fn join(self) -> Join6<A, B, C, D, E, F, PA, PB, PC, PD, PE, PF> =
    Join6(Either::Left(self:0), Either::Left(self:1), Either::Left(self:2), Either::Left(self:3), Either::Left(self:4), Either::Left(self:5)).
}

impl<A, B, C, D, E, F, PA, PB, PC, PD, PE, PF> Poll for Join6<A, B, C, D, E, F, PA, PB, PC, PD, PE, PF> where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D>, PE: Poll<Result=E>, PF: Poll<Result=F> {
  type Result = (A, B, C, D, E, F).

  fn poll(self) -> (PollState<(A, B, C, D, E, F)>, Self) = match self {
    Join6(join0, join1, join2, join3, join4, join5) => {
      let success = true.

      let join0 = match join0 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join1 = match join1 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join2 = match join2 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join3 = match join3 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join4 = match join4 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join5 = match join5 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.

      let new_self = Join6(join0, join1, join2, join3, join4, join5).

      if success {
        (PollState::Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right(), join4:unwrap_right(), join5:unwrap_right())), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}


impl<Ret, A, B, C, D, E, F, G> Call<(A, B, C, D, E, F, G)> for |A, B, C, D, E, F, G| -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D, E, F, G)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<Ret, A, B, C, D, E, F, G> Call<(A, B, C, D, E, F, G)> for fn(A, B, C, D, E, F, G) -> Ret {
  type Return = Ret.

  fn call(self, args: (A, B, C, D, E, F, G)) -> Ret = {
    internal_call(self, args)
  }.
}

impl<A, B, C, D, E, F, G> Into<String> for (A, B, C, D, E, F, G) where A: Into<String>, B: Into<String>, C: Into<String>, D: Into<String>, E: Into<String>, F: Into<String>, G: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3), \(self:4), \(self:5), \(self:6))"
  }.
}

impl<A, B, C, D, E, F, G> Hash for (A, B, C, D, E, F, G) where A: Hash, B: Hash, C: Hash, D: Hash, E: Hash, F: Hash, G: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
        h = 31 * h + self:4:hash().
        h = 31 * h + self:5:hash().
        h = 31 * h + self:6:hash().
    h
  }.
}

impl<A, B, C, D, E, F, G> Equals<(A, B, C, D, E, F, G)> for (A, B, C, D, E, F, G) where A: Equals<A>, B: Equals<B>, C: Equals<C>, D: Equals<D>, E: Equals<E>, F: Equals<F>, G: Equals<G> {
  fn eq(self, other: Self) -> Bool = {
    self:0:eq(other:0) &? self:1:eq(other:1) &? self:2:eq(other:2) &? self:3:eq(other:3) &? self:4:eq(other:4) &? self:5:eq(other:5) &? self:6:eq(other:6)
  }.

  fn ne(self, other: Self) -> Bool = {
    !(self == other)
  }.
}

struct Join7<A, B, C, D, E, F, G, PA, PB, PC, PD, PE, PF, PG>(Either<PA, A>, Either<PB, B>, Either<PC, C>, Either<PD, D>, Either<PE, E>, Either<PF, F>, Either<PG, G>).

impl<A, B, C, D, E, F, G, PA, PB, PC, PD, PE, PF, PG> Join for (PA, PB, PC, PD, PE, PF, PG) where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D>, PE: Poll<Result=E>, PF: Poll<Result=F>, PG: Poll<Result=G> {
  type Result = (A, B, C, D, E, F, G).
  type Joined = Join7<A, B, C, D, E, F, G, PA, PB, PC, PD, PE, PF, PG>.

  fn join(self) -> Join7<A, B, C, D, E, F, G, PA, PB, PC, PD, PE, PF, PG> =
    Join7(Either::Left(self:0), Either::Left(self:1), Either::Left(self:2), Either::Left(self:3), Either::Left(self:4), Either::Left(self:5), Either::Left(self:6)).
}

impl<A, B, C, D, E, F, G, PA, PB, PC, PD, PE, PF, PG> Poll for Join7<A, B, C, D, E, F, G, PA, PB, PC, PD, PE, PF, PG> where PA: Poll<Result=A>, PB: Poll<Result=B>, PC: Poll<Result=C>, PD: Poll<Result=D>, PE: Poll<Result=E>, PF: Poll<Result=F>, PG: Poll<Result=G> {
  type Result = (A, B, C, D, E, F, G).

  fn poll(self) -> (PollState<(A, B, C, D, E, F, G)>, Self) = match self {
    Join7(join0, join1, join2, join3, join4, join5, join6) => {
      let success = true.

      let join0 = match join0 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join1 = match join1 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join2 = match join2 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join3 = match join3 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join4 = match join4 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join5 = match join5 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.


      let join6 = match join6 {
        Either::Left(left) => match left:poll() {
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {
            success = false.
            Either::Left(left)
          },
        },
        Either::Right(right) => Either::Right(right),
      }.

      let new_self = Join7(join0, join1, join2, join3, join4, join5, join6).

      if success {
        (PollState::Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right(), join4:unwrap_right(), join5:unwrap_right(), join6:unwrap_right())), new_self)
      } else {
        (PollState::Incomplete, new_self)
      }
    },
  }.
}

