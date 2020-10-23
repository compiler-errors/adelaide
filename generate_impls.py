#!/usr/bin/env python3
"""
This is a helpful little script that generates the stupid traits to call closures and fns.
"""

ARGFMT = """
impl<Ret{comma_generic_tys}> Call<{tuple_ty}> for {closure_type} {{
  type Return = Ret.

  fn call(self, args: {tuple_ty}) -> Ret = {{
    todo()
  }}.
}}

impl<Ret{comma_generic_tys}> Call<{tuple_ty}> for {fn_type} {{
  type Return = Ret.

  fn call(self, args: {tuple_ty}) -> Ret = {{
    todo()
  }}.
}}

impl{angled_generic_tys} Into<String> for {tuple_ty}{where_into_string} {{
  fn into(self) -> String = {{
    {string_constructor}
  }}.
}}

impl{angled_generic_tys} Hash for {tuple_ty}{where_hash} {{
  fn hash(self) -> Int = {{
    let h = 7.{hashes}
    h
  }}.
}}

struct Join{n}{join_generics}{join_variant}.

impl{join_generics} Join for {awaitable_tuple_ty}{where_poll} {{
  type Result = {tuple_ty}.
  type Joined = Join{n}{join_generics}.

  fn join(self) -> Join{n}{join_generics} =
    Join{n}{join_variant_lefts}.
}}

impl{join_generics} Poll for Join{n}{join_generics}{where_poll} {{
  type Result = {tuple_ty}.

  fn poll(self) -> (PollState<{tuple_ty}>, Self) = match self {{
    Join{n}{join_variant_variables} => {{
      let success = true.
{join_variant_process}
      let new_self = Join{n}{join_variant_variables}.

      if success {{
        (PollState::Complete({join_complete_tuple}), new_self)
      }} else {{
        (PollState::Incomplete, new_self)
      }}
    }},
  }}.
}}
"""

JOIN_VARIANT_PROCESS_FMT = """
      let join{m} = match join{m} {{
        Either::Left(left) => match left:poll() {{
          (PollState::Complete(right), _) => Either::Right(right),
          (PollState::Incomplete, left) => {{
            success = false.
            Either::Left(left)
          }},
        }},
        Either::Right(right) => Either::Right(right),
      }}.
"""

for i in range(0,8):
    generics = ["" + chr(ord('A') + j) for j in range(i)]
    generic_tys = ", ".join(generics)
    comma_generic_tys = "" if i == 0 else (", " + ", ".join(generics))
    tuple_ty = "()" if i == 0 else ("(A,)" if i == 1 else ("(" + ", ".join(generics) + ")"))
    angled_generic_tys = "" if i == 0 else ("<" + ", ".join(generics) + ">")
    closure_type = f"|{generic_tys}| -> Ret"
    fn_type = f"fn({generic_tys}) -> Ret"
    fn_type_with_env = "fn(" + ", ".join(["ClosureEnvironment"] + generics) + ") -> Ret"
    comma_unpacked_args = "" if i == 0 else (", " + ", ".join("args:" + str(i) for i in range(i)))

    where_into_string = "" if i == 0 else (" where " + ", ".join(g + ": Into<String>" for g in generics))
    string_constructor = "\"()\"" if i == 0 else ("\"(\(self:0),)\"" if i == 1 else ("\"(\(" + "), \(".join(f"self:{i}" for i in range(i)) + "))\""))

    where_hash = "" if i == 0 else (" where " + ", ".join(g + ": Hash" for g in generics))
    hashes = "\n" + "\n".join(f"        h = 31 * h + self:{i}:hash()." for i in range(i))

    join_variant = "" if i == 0 else ("(" + ", ".join(["Either<P" + chr(ord('A') + j) + ", " + chr(ord('A') + j) + ">" for j in range(i)]) + ")")
    join_generics = "" if i == 0 else ("<" + ", ".join(generics + ["P" + chr(ord('A') + j) for j in range(i)]) + ">")
    awaitable_generics = ["P" + chr(ord('A') + j) for j in range(i)]
    awaitable_tuple_ty = "()" if i == 0 else ("(PA,)" if i == 1 else ("(" + ", ".join(awaitable_generics) + ")"))
    where_poll = "" if i == 0 else (" where " + ", ".join("P" + chr(ord('A') + j) + ": Poll<Result=" + chr(ord('A') + j) + ">" for j in range(i)))
    join_variant_lefts = "" if i == 0 else ("(" + ", ".join(f"Either::Left(self:{j})" for j in range(i)) + ")")
    join_variant_variables = "" if i == 0 else ("(" + ", ".join(f"join{j}" for j in range(i)) + ")")
    join_complete_tuple = "()" if i == 0 else ("(join0:unwrap_right(),)" if i == 1 else ("(" + ", ".join(f"join{j}:unwrap_right()" for j in range(i)) + ")"))
    join_variant_process = "\n".join(JOIN_VARIANT_PROCESS_FMT.format(m=j) for j in range(i))

    print(ARGFMT.format(n=i,
                        tuple_ty=tuple_ty,
                        generic_tys=generic_tys,
                        comma_generic_tys=comma_generic_tys,
                        angled_generic_tys=angled_generic_tys,
                        closure_type=closure_type,
                        fn_type=fn_type,
                        fn_type_with_env=fn_type_with_env,
                        comma_unpacked_args=comma_unpacked_args,
                        where_into_string=where_into_string,
                        string_constructor=string_constructor,
                        where_hash=where_hash,
                        hashes=hashes,
                        join_generics=join_generics,
                        where_poll=where_poll,
                        join_variant=join_variant,
                        awaitable_tuple_ty=awaitable_tuple_ty,
                        join_variant_lefts=join_variant_lefts,
                        join_variant_variables=join_variant_variables,
                        join_variant_process=join_variant_process,
                        join_complete_tuple=join_complete_tuple,
                        ))
