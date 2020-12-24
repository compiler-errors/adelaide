use std::{collections::HashMap, time::Instant};

use itertools::{traits::HomogeneousTuple, Itertools};
use lazy_static::lazy_static;

use crate::translate::CType;

use super::{
    heap::{Generator, Heap, Value},
    thread::{Control, State, Thread, ThreadId},
    IError, IResult, Interpreter,
};

fn unwrap_args<T: HomogeneousTuple>(args: Vec<T::Item>) -> T {
    args.into_iter().tuples().next().unwrap()
}

type InternalFunction = for<'a> fn(
    &Interpreter<'a>,
    &mut Heap<'a>,
    &mut Thread<'a>,
    &'a [CType<'a>],
    Vec<Value<'a>>,
) -> IResult<State<'a>>;

macro_rules! simple_fn {
    (() => $result:expr) => {{
        #[allow(unused_variables)]
        fn inner<'a>(
            _: &Interpreter<'a>,
            _: &mut Heap<'a>,
            _: &mut Thread<'a>,
            _: &'a [CType<'a>],
            args: Vec<Value<'a>>,
        ) -> IResult<State<'a>> {
            Ok(State::Value($result))
        }

        inner
    }};
    ($pattern:pat => $result:expr) => {{
        fn inner<'a>(
            _: &Interpreter<'a>,
            _: &mut Heap<'a>,
            _: &mut Thread<'a>,
            _: &'a [CType<'a>],
            args: Vec<Value<'a>>,
        ) -> IResult<State<'a>> {
            if let $pattern = unwrap_args(args) {
                Ok(State::Value($result))
            } else {
                unreachable!()
            }
        }

        inner
    }};
}

lazy_static! {
    pub static ref START: Instant = Instant::now();
    pub static ref INTERNAL_FUNCTIONS: HashMap<&'static str, InternalFunction> = {
        let mut map: HashMap<&'static str, InternalFunction> = hashmap! {};

        map.insert(
            "internal_add",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b))),
        );
        map.insert(
            "internal_sub",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b))),
        );
        map.insert(
            "internal_mul",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b))),
        );
        map.insert(
            "internal_div",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_div(b))),
        );
        map.insert(
            "internal_rem",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_rem(b))),
        );
        map.insert(
            "internal_eq",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::boolean(a == b)),
        );
        map.insert(
            "internal_gt",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::boolean(a > b)),
        );
        map.insert(
            "internal_neg",
            simple_fn!((Value::Int(a),) => Value::Int(-a)),
        );

        map.insert(
            "internal_itof",
            simple_fn!((Value::Int(a),) => Value::Float(a as f64)),
        );

        map.insert("internal_xor", simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(((a as u64) ^ (b as u64)) as i64)));
        map.insert("internal_lshr", simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(((a as u64) >> (b as u64)) as i64)));

        map.insert(
            "internal_csub",
            simple_fn!((Value::Int(a), Value::Int(b)) => Value::Int(a - b)),
        );
        map.insert(
            "internal_ctoi",
            simple_fn!((Value::Int(a),) => Value::Int(a)),
        );

        map.insert(
            "internal_fadd",
            simple_fn!((Value::Float(a), Value::Float(b)) => Value::Float(a + b)),
        );
        map.insert(
            "internal_fsub",
            simple_fn!((Value::Float(a), Value::Float(b)) => Value::Float(a - b)),
        );
        map.insert(
            "internal_fmul",
            simple_fn!((Value::Float(a), Value::Float(b)) => Value::Float(a * b)),
        );
        map.insert(
            "internal_fdiv",
            simple_fn!((Value::Float(a), Value::Float(b)) => Value::Float(a / b)),
        );
        map.insert(
            "internal_feq",
            simple_fn!((Value::Float(a), Value::Float(b)) => Value::boolean(a == b)),
        );
        map.insert(
            "internal_fgt",
            simple_fn!((Value::Float(a), Value::Float(b)) => Value::boolean(a > b)),
        );
        map.insert(
            "internal_fneg",
            simple_fn!((Value::Float(a),) => Value::Float(-a)),
        );

        map.insert(
            "internal_itos",
            simple_fn!((Value::Int(i),) => Value::String(i.to_string())),
        );
        map.insert(
            "internal_ftos",
            simple_fn!((Value::Float(f),) => Value::String(f.to_string())),
        );
        map.insert(
            "internal_ctos",
            simple_fn!((Value::Int(c),) => Value::String((c as u8 as char).to_string())),
        );

        map.insert(
            "internal_string_add",
            simple_fn!((Value::String(a), Value::String(b)) => Value::String(a + &b)),
        );
        map.insert("internal_string_deref", simple_fn!((Value::String(s), Value::Int(i)) => Value::Int(s.chars().nth(i as usize).unwrap() as i64)));
        map.insert(
            "internal_string_eq",
            simple_fn!((Value::String(a), Value::String(b)) => Value::boolean(a == b)),
        );
        map.insert(
            "internal_string_len",
            simple_fn!((Value::String(a),) => Value::Int(a.len() as i64)),
        );

        map.insert("internal_call", call);

        map.insert("breakpoint", breakpoint);
        map.insert("exit", exit);

        map.insert("gc", gc);

        map.insert("internal_alloc_empty_array", alloc_empty_array);
        map.insert("internal_array_deref", array_deref);
        map.insert("internal_array_len", array_len);
        map.insert("internal_array_slice", array_slice);
        map.insert("internal_array_store", array_store);

        map.insert("internal_thread_complete", thread_complete);
        map.insert("internal_thread_count", thread_count);
        map.insert("internal_thread_current", thread_current);
        map.insert("internal_thread_spawn", thread_spawn);
        map.insert("internal_thread_switch", thread_switch);
        map.insert("internal_thread_block_on", thread_block_on);

        map.insert("internal_generator_unpark", generator_unpark);

        map.insert("internal_unbox_transmute", unbox_transmute);
        map.insert(
            "internal_undefined_value",
            simple_fn!(() => Value::Undefined),
        );

        map.insert(
            "print",
            simple_fn!((Value::String(s),) => {
                print!("{}", s);
                Value::unit()
            }),
        );

        map.insert("type_id_of", type_id_of);
        map.insert("type_string_of", type_string_of);
        map.insert("unreachable", unreachable);

        map.insert(
            "now_millis",
            simple_fn!(() => Value::Int(START.elapsed().as_millis() as i64)),
        );

        map
    };
}

fn call<'a>(
    interpreter: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    thread: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (Value::Callable(captures, fun), Value::Struct(args)) = unwrap_args(args) {
        interpreter.do_call(heap, thread, fun, captures, args)
    } else {
        unreachable!()
    }
}

#[cold]
#[inline(never)]
fn breakpoint<'a>(
    _: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    Ok(State::Value(Value::unit()))
}

fn exit<'a>(
    _: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    Err(IError::Exit)
}

fn unreachable<'a>(
    _: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    println!("Panic: Unreachable statement!");
    Err(IError::Panic)
}

fn gc<'a>(
    _: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    todo!()
}

fn alloc_empty_array<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (Value::Int(size),) = unwrap_args(args) {
        Ok(State::Value(
            heap.allocate(vec![Value::Undefined; size as usize]),
        ))
    } else {
        unreachable!()
    }
}

fn array_deref<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (array, Value::Int(idx)) = unwrap_args(args) {
        Ok(State::Value(
            heap.index_allocated(array, idx as usize).clone(),
        ))
    } else {
        unreachable!()
    }
}

fn array_len<'a>(
    _: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (Value::Object(_, _, len),) = unwrap_args(args) {
        Ok(State::Value(Value::Int(len as i64)))
    } else {
        unreachable!()
    }
}

fn array_slice<'a>(
    _: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (Value::Object(obj, start, _), Value::Int(a), Value::Int(b)) = unwrap_args(args) {
        Ok(State::Value(Value::Object(
            obj,
            start + a as usize,
            b as usize - a as usize,
        )))
    } else {
        unreachable!()
    }
}

fn array_store<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (array, Value::Int(idx), value) = unwrap_args(args) {
        *heap.index_allocated(array, idx as usize) = value.clone();
        Ok(State::Value(value))
    } else {
        unreachable!()
    }
}

fn thread_complete<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    thread: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (Value::Int(id),) = unwrap_args(args) {
        let id = ThreadId(id as usize);
        Ok(State::Value(Value::boolean(
            thread.id != id
                && !heap.blocked_threads.contains_key(&id)
                && !heap.ready_threads.contains_key(&id),
        )))
    } else {
        unreachable!()
    }
}

fn thread_spawn<'a>(
    interpreter: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (Value::Callable(_, trampoline), callable) = unwrap_args(args) {
        let mut thread = heap.new_thread();
        let handle = thread.handle.clone();

        let state = interpreter.do_call(heap, &mut thread, trampoline, vec![], vec![callable])?;
        thread.control.push(Control::Parked(state));

        heap.push_ready_thread(thread);

        Ok(State::Value(handle))
    } else {
        unreachable!()
    }
}

fn thread_current<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    thread: &mut Thread<'a>,
    _: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let Value::Struct(thread_and_return) = heap.index_allocated(thread.handle.clone(), 0) {
        Ok(State::Value(thread_and_return[0].clone()))
    } else {
        unreachable!()
    }
}

fn thread_count<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    Ok(State::Value(Value::Int(
        1 + heap.ready_threads.len() as i64 + heap.blocked_threads.len() as i64,
    )))
}

fn thread_switch<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    thread: &mut Thread<'a>,
    _: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let Some(new_thread) = heap.pop_ready_thread() {
        let mut old_thread = std::mem::replace(thread, new_thread);
        // Park the unit value state onto the top of the old thread, since that's what
        // this function is expected to return.
        old_thread
            .control
            .push(Control::Parked(State::Value(Value::unit())));
        heap.push_ready_thread(old_thread);

        // Unpark the state from the current thread
        if let Some(Control::Parked(state)) = thread.control.pop() {
            Ok(state)
        } else {
            unreachable!()
        }
    } else {
        // Do nothing, no thread to which to switch
        Ok(State::Value(Value::unit()))
    }
}

fn thread_block_on<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    thread: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (Value::Int(id),) = unwrap_args(args) {
        if !heap.block_on_thread(thread, ThreadId(id as usize)) {
            return Ok(State::Value(Value::unit()));
        }

        if let Some(new_thread) = heap.pop_ready_thread() {
            let mut old_thread = std::mem::replace(thread, new_thread);
            // Park the unit value state onto the top of the old thread, since that's what
            // this function is expected to return.
            old_thread
                .control
                .push(Control::Parked(State::Value(Value::unit())));
            heap.push_blocked_thread(old_thread);

            // Unpark the state from the current thread
            if let Some(Control::Parked(state)) = thread.control.pop() {
                Ok(state)
            } else {
                unreachable!()
            }
        } else {
            Err(IError::Deadlock)
        }
    } else {
        unreachable!()
    }
}

fn generator_unpark<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    thread: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    debug!("Unparking the generator");

    let (generator, value) = unwrap_args(args);

    match heap.store_generator_evaluating(generator.clone()) {
        Generator::Incomplete { stack_rev, slots } => {
            thread.control.extend(stack_rev.into_iter().rev());
            thread.slots.push(slots);

            Ok(State::Value(value))
        },
        Generator::Complete(value) => {
            heap.store_generator_complete(generator.clone(), value.clone());

            //Err(IError::AfterComplete)
            Ok(State::Value(Value::Struct(vec![
                Value::Variant("Complete", vec![value]),
                generator,
            ])))
        },
        Generator::Evaluating => Err(IError::DoubleResume),
    }
}

fn unbox_transmute<'a>(
    _: &Interpreter<'a>,
    heap: &mut Heap<'a>,
    _: &mut Thread<'a>,
    _: &'a [CType<'a>],
    args: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    if let (value @ Value::Dyn(_),) = unwrap_args(args) {
        let mut value = value;
        while matches!(value, Value::Dyn(_)) {
            value = heap.unbox_dyn(value);
        }
        Ok(State::Value(value))
    } else {
        unreachable!()
    }
}

fn type_id_of<'a>(
    interpreter: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    tys: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    Ok(State::Value(Value::Int(
        interpreter.program.type_ids[&tys[0]] as i64,
    )))
}

fn type_string_of<'a>(
    interpreter: &Interpreter<'a>,
    _: &mut Heap<'a>,
    _: &mut Thread<'a>,
    tys: &'a [CType<'a>],
    _: Vec<Value<'a>>,
) -> IResult<State<'a>> {
    Ok(State::Value(Value::String(
        interpreter.program.type_strings[&tys[0]].to_owned(),
    )))
}
