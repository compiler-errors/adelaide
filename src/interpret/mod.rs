mod heap;
mod internal;
mod thread;

use std::time::Instant;

use bumpalo::Bump;

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    lowering::LoopId,
    translate::{
        CExpression, CFunction, CLiteral, CPattern, CProgram, CStackId, CStatement, Translator,
    },
    typechecker::{typecheck_program, Typechecker},
    util::{AError, AResult, Pretty, ZipExact},
};

use self::{
    heap::{Heap, Value},
    internal::INTERNAL_FUNCTIONS,
    thread::{Control, State, Thread, ThreadId, ThreadState, THREAD_QUANTUM_MILLIS},
};

pub fn interpret_program(ctx: &dyn AdelaideContext) -> AResult<()> {
    let alloc = Bump::new();

    let typechecked = Typechecker::new_concrete(&typecheck_program(ctx)?, btreemap! {});
    let translator = Translator::new(ctx, typechecked, &alloc);
    let program = translator.translate_program()?;

    match (Interpreter { program }.run()) {
        Ok(()) | Err(IError::Exit) => Ok(()),
        Err(IError::Oof(span)) => Err(AError::Oof { span }),
        Err(_) => todo!(),
    }
}

pub struct Interpreter<'a> {
    program: &'a CProgram<'a>,
}

type IResult<T> = Result<T, IError>;

pub enum IError {
    Exit,
    Panic,
    Deadlock,
    DoubleAwait,
    NoPattern,
    Oof(Span),
}

impl<'a> Interpreter<'a> {
    fn run(&self) -> IResult<()> {
        let mut heap = Heap::new();

        // Allocate the main thread first, so it has thread_id == 0
        let mut main_thread = heap.new_thread();
        assert_eq!(main_thread.id, ThreadId(0));

        for (id, global) in &self.program.globals {
            debug!("Evaluating global: {:?} = {:?}", id, global.name);

            let mut global_thread = heap.new_thread();
            // Set up a scope with N slots
            global_thread
                .slots
                .push(vec![Value::Undefined; global.slots]);
            global_thread.control.push(Control::Scope);
            // Park thread here
            global_thread
                .control
                .push(Control::Parked(State::Expression(global.body)));

            match self.run_thread(&mut heap, &mut global_thread)? {
                ThreadState::Complete(e) => {
                    heap.globals.insert(*id, e);
                },
                ThreadState::Incomplete => {
                    todo!("Die")
                },
            }
        }

        debug!("Done evaluating globals");

        let main_fn = self.program.functions[&self.program.main];
        // Set up the main thread to be executing a single call to main.
        // Park the state right after the fn call has initiated.
        let state = self.do_call(&mut heap, &mut main_thread, main_fn, vec![], vec![])?;
        main_thread.control.push(Control::Parked(state));

        let mut current_thread = main_thread;
        loop {
            match self.run_thread(&mut heap, &mut current_thread)? {
                ThreadState::Complete(_) if current_thread.id == ThreadId(0) => {
                    debug!("Main thread exited!");
                    return Ok(());
                },
                ThreadState::Complete(exit_value) => {
                    if let Value::Struct(values) = heap.index_allocated(current_thread.handle, 0) {
                        values[1] = Value::Variant("Some", vec![exit_value]);
                    } else {
                        unreachable!();
                    }

                    for blocked_id in current_thread.blocking {
                        heap.signal_thread(current_thread.id, blocked_id);
                    }

                    current_thread = heap.pop_ready_thread().ok_or_else(|| IError::Deadlock)?;
                },
                ThreadState::Incomplete =>
                    if let Some(new_thread) = heap.pop_ready_thread() {
                        let old_thread = std::mem::replace(&mut current_thread, new_thread);
                        heap.push_ready_thread(old_thread);
                    },
            }
        }
    }

    fn run_thread(&self, heap: &mut Heap<'a>, thread: &mut Thread<'a>) -> IResult<ThreadState> {
        let mut state = if let Some(Control::Parked(state)) = thread.control.pop() {
            state
        } else {
            unreachable!()
        };

        thread.start = Instant::now();

        loop {
            if thread.control.is_empty() {
                if let State::Value(value) = state {
                    return Ok(ThreadState::Complete(value));
                }
            }

            if thread.start.elapsed().as_millis() > THREAD_QUANTUM_MILLIS {
                // Optimization: only preempt if there are threads to switch to.
                if heap.num_ready_threads() > 0 {
                    thread.control.push(Control::Parked(state));
                    return Ok(ThreadState::Incomplete);
                } else {
                    thread.start = Instant::now();
                }
            }

            let new_state = match state {
                State::Expression(e) => self.apply_expression(heap, thread, e),
                State::LvalExpression(l) => self.apply_lval_expression(heap, thread, l),
                State::Statement(s) => self.apply_statement(heap, thread, s),
                State::Value(v) => self.apply_value(heap, thread, v),
            };

            match new_state {
                Ok(new_state) => {
                    state = new_state;
                },
                Err(e) => {
                    // TODO: I could add a stack trace by unwinding `thread.control`
                    return Err(e);
                },
            }
        }
    }

    fn apply_expression(
        &self,
        heap: &mut Heap<'a>,
        thread: &mut Thread<'a>,
        expr: CExpression<'a>,
    ) -> IResult<State<'a>> {
        let state = match expr {
            CExpression::Literal(CLiteral::True) => State::Value(Value::Int(1)),
            CExpression::Literal(CLiteral::False) => State::Value(Value::Int(0)),
            CExpression::Literal(CLiteral::Int(i)) => State::Value(Value::Int(i)),
            CExpression::Literal(CLiteral::Float(f)) => State::Value(Value::Float(f)),
            CExpression::Literal(CLiteral::Char(c)) => State::Value(Value::Int(c as i64)),
            CExpression::Literal(CLiteral::String(s)) => State::Value(Value::String(s.to_owned())),
            CExpression::Variable(v) => State::Value(thread.slots.last().unwrap()[v.0].clone()),
            CExpression::Block(&[], expr) => State::Expression(*expr),
            CExpression::Block(stmts, expr) => {
                thread.control.push(Control::Block(&stmts[1..], *expr));
                State::Statement(stmts[0])
            },
            CExpression::AsyncBlock(slots, captures, expr) => {
                let mut slots = vec![Value::Undefined; slots];
                for (old_id, new_id) in captures.0 {
                    slots[new_id.0] = thread.slots.last().unwrap()[old_id.0].clone();
                }
                State::Value(heap.allocate_async(slots, *expr))
            },
            CExpression::Global(id) => State::Value(heap.globals[&id].clone()),
            CExpression::GlobalFunction(id) =>
                State::Value(Value::Callable(vec![], self.program.functions[&id])),
            CExpression::Call(fun, &[]) =>
                self.do_call(heap, thread, self.program.functions[&fun], vec![], vec![])?,
            CExpression::Call(fun, values) => {
                thread.control.push(Control::Invoke(
                    self.program.functions[&fun],
                    &values[1..],
                    vec![],
                ));
                State::Expression(values[0])
            },
            CExpression::Struct(&[]) => State::Value(Value::unit()),
            CExpression::Struct(values) => {
                let (idx, value) = values[0];
                thread.control.push(Control::Struct(
                    &values[1..],
                    vec![Value::Undefined; values.len()],
                    idx,
                ));
                State::Expression(value)
            },
            CExpression::Object(&[]) => State::Value(heap.allocate(vec![])),
            CExpression::Object(values) => {
                let (idx, value) = values[0];
                thread.control.push(Control::Object(
                    &values[1..],
                    vec![Value::Undefined; values.len()],
                    idx,
                ));
                State::Expression(value)
            },
            CExpression::Variant(var, &[]) => State::Value(Value::Variant(var, vec![])),
            CExpression::Variant(var, values) => {
                let (idx, value) = values[0];
                thread.control.push(Control::Variant(
                    var,
                    &values[1..],
                    vec![Value::Undefined; values.len()],
                    idx,
                ));
                State::Expression(value)
            },
            CExpression::StructAccess(expr, idx) => {
                thread.control.push(Control::StructAccess(idx));
                State::Expression(*expr)
            },
            CExpression::ObjectAccess(expr, idx) => {
                thread.control.push(Control::ObjectAccess(idx));
                State::Expression(*expr)
            },
            CExpression::Assign(left, right) => {
                thread.control.push(Control::ApplyTo(*left));
                State::Expression(*right)
            },
            CExpression::Or(left, right) => {
                thread.control.push(Control::Or(*right));
                State::Expression(*left)
            },
            CExpression::And(left, right) => {
                thread.control.push(Control::And(*right));
                State::Expression(*left)
            },
            CExpression::If(cond, then, els) => {
                thread.control.push(Control::If(*then, *els));
                State::Expression(*cond)
            },
            CExpression::Return(expr) => {
                thread.control.push(Control::Return);
                State::Expression(*expr)
            },
            CExpression::Break(id, expr) => {
                thread.control.push(Control::Break(id));
                State::Expression(*expr)
            },
            CExpression::Continue(id) => self.do_continue(thread, id)?,
            CExpression::Closure(captures, fun) => {
                let captures = captures
                    .0
                    .iter()
                    .map(|(old_id, new_id)| {
                        (*new_id, thread.slots.last().unwrap()[old_id.0].clone())
                    })
                    .collect();
                State::Value(Value::Callable(captures, *fun))
            },
            CExpression::Loop(id, expr) => {
                thread.control.push(Control::Loop(id, *expr));
                State::Expression(*expr)
            },
            CExpression::Match(expr, patterns) => {
                thread.control.push(Control::Match(patterns));
                State::Expression(*expr)
            },
        };

        Ok(state)
    }

    fn apply_lval_expression(
        &self,
        heap: &mut Heap<'a>,
        thread: &mut Thread<'a>,
        expr: CExpression<'a>,
    ) -> IResult<State<'a>> {
        if let Some(Control::Apply {
            rval,
            mut indices_rev,
        }) = thread.control.pop()
        {
            let state = match expr {
                CExpression::Variable(id) => {
                    self.do_store(
                        &mut thread.slots.last_mut().unwrap()[id.0],
                        &indices_rev,
                        rval.clone(),
                    );
                    State::Value(rval)
                },
                CExpression::Global(id) => {
                    self.do_store(
                        heap.globals.get_mut(&id).unwrap(),
                        &indices_rev,
                        rval.clone(),
                    );
                    State::Value(rval)
                },
                CExpression::StructAccess(lval, idx) => {
                    indices_rev.push(idx);
                    thread.control.push(Control::Apply { rval, indices_rev });
                    State::LvalExpression(*lval)
                },
                CExpression::ObjectAccess(expr, object_idx) => {
                    thread.control.push(Control::ApplyObject {
                        rval,
                        object_idx,
                        indices_rev,
                    });
                    State::Expression(*expr)
                },
                _ => unreachable!(),
            };

            Ok(state)
        } else {
            unreachable!()
        }
    }

    fn apply_statement(
        &self,
        heap: &mut Heap<'a>,
        thread: &mut Thread<'a>,
        stmt: CStatement<'a>,
    ) -> IResult<State<'a>> {
        let state = match stmt {
            CStatement::Let(pattern, expr) => {
                thread.control.push(Control::InfallibleApply(pattern));
                State::Expression(expr)
            },
            CStatement::Expression(expr) => State::Expression(expr),
        };

        Ok(state)
    }

    fn apply_value(
        &self,
        heap: &mut Heap<'a>,
        thread: &mut Thread<'a>,
        value: Value<'a>,
    ) -> IResult<State<'a>> {
        let new_state = match thread.control.pop().unwrap() {
            Control::Parked(_) | Control::Apply { .. } => unreachable!(),
            Control::Scope => {
                thread.slots.pop().unwrap();
                State::Value(value)
            },
            Control::AsyncScope(awaitable) => {
                thread.slots.pop().unwrap();
                heap.store_awaitable_complete(awaitable.clone(), value.clone());

                State::Value(Value::Struct(vec![
                    Value::Variant("Complete", vec![value]),
                    awaitable,
                ]))
            },
            Control::Block(&[], expr) => State::Expression(expr),
            Control::Block(stmts, expr) => {
                thread.control.push(Control::Block(&stmts[1..], expr));
                State::Statement(stmts[0])
            },
            Control::Invoke(fun, &[], mut args) => {
                args.push(value);
                self.do_call(heap, thread, fun, vec![], args)?
            },
            Control::Invoke(fun, exprs, mut args) => {
                args.push(value);
                thread.control.push(Control::Invoke(fun, &exprs[1..], args));
                State::Expression(exprs[0])
            },
            Control::Struct(&[], mut values, idx) => {
                values[idx] = value;
                State::Value(Value::Struct(values))
            },
            Control::Struct(exprs, mut values, idx) => {
                values[idx] = value;
                let (idx, expr) = exprs[0];
                thread
                    .control
                    .push(Control::Struct(&exprs[1..], values, idx));
                State::Expression(expr)
            },
            Control::Object(&[], mut values, idx) => {
                values[idx] = value;
                State::Value(heap.allocate(values))
            },
            Control::Object(exprs, mut values, idx) => {
                values[idx] = value;
                let (idx, expr) = exprs[0];
                thread
                    .control
                    .push(Control::Object(&exprs[1..], values, idx));
                State::Expression(expr)
            },
            Control::Variant(variant, &[], mut values, idx) => {
                values[idx] = value;
                State::Value(Value::Variant(variant, values))
            },
            Control::Variant(variant, exprs, mut values, idx) => {
                values[idx] = value;
                let (idx, expr) = exprs[0];
                thread
                    .control
                    .push(Control::Variant(variant, &exprs[1..], values, idx));
                State::Expression(expr)
            },
            Control::StructAccess(idx) => match value {
                Value::Struct(values) => State::Value(values.into_iter().nth(idx).unwrap()),
                _ => unreachable!(),
            },
            Control::ObjectAccess(idx) => match value {
                Value::Object(..) => State::Value(heap.index_allocated(value, idx).clone()),
                _ => unreachable!(),
            },
            Control::Or(expr) => match value {
                Value::Int(0) => State::Expression(expr),
                Value::Int(1) => State::Value(value),
                _ => unreachable!(),
            },
            Control::And(expr) => match value {
                Value::Int(0) => State::Value(value),
                Value::Int(1) => State::Expression(expr),
                _ => unreachable!(),
            },
            Control::If(then, els) => match value {
                Value::Int(1) => State::Expression(then),
                Value::Int(0) => State::Expression(els),
                _ => unreachable!(),
            },
            Control::Return => self.do_return(heap, thread, value)?,
            Control::Break(id) => self.do_break(thread, id, value)?,
            Control::Loop(id, expr) => {
                thread.control.push(Control::Loop(id, expr));
                State::Expression(expr)
            },
            Control::Match(patterns) => {
                let mut new_expr = None;

                for (pattern, expr) in patterns {
                    if self.do_pattern_apply(heap, thread, *pattern, &value)? {
                        new_expr = Some(*expr);
                        break;
                    }
                }

                if let Some(expr) = new_expr {
                    State::Expression(expr)
                } else {
                    return Err(IError::NoPattern);
                }
            },
            Control::InfallibleApply(pattern) => {
                if !self.do_pattern_apply(heap, thread, pattern, &value)? {
                    unreachable!();
                }
                // Doesn't matter what we return from a let-expr
                State::Value(Value::Undefined)
            },
            Control::ApplyTo(v) => {
                thread.control.push(Control::Apply {
                    rval: value,
                    indices_rev: vec![],
                });
                State::LvalExpression(v)
            },
            Control::ApplyObject {
                rval,
                object_idx,
                indices_rev,
            } => {
                self.do_store(
                    heap.index_allocated(value, object_idx),
                    &indices_rev,
                    rval.clone(),
                );
                State::Value(rval)
            },
        };

        Ok(new_state)
    }

    fn do_store(&self, mut left: &mut Value<'a>, struct_indices_rev: &[usize], value: Value<'a>) {
        for idx in struct_indices_rev.iter().rev() {
            if let Value::Struct(values) = left {
                left = &mut values[*idx];
            }
        }

        *left = value;
    }

    fn do_pattern_apply(
        &self,
        heap: &mut Heap<'a>,
        thread: &mut Thread<'a>,
        pattern: CPattern<'a>,
        value: &Value<'a>,
    ) -> IResult<bool> {
        match (pattern, value) {
            (CPattern::Underscore, _) => Ok(true),
            (CPattern::Literal(CLiteral::True), Value::Int(v)) => (Ok(*v == 1)),
            (CPattern::Literal(CLiteral::False), Value::Int(v)) => (Ok(*v == 0)),
            (CPattern::Literal(CLiteral::Int(p)), Value::Int(v)) => (Ok(p == *v)),
            (CPattern::Literal(CLiteral::Char(p)), Value::Int(v)) => (Ok(p as i64 == *v)),
            (CPattern::Literal(CLiteral::Float(p)), Value::Float(v)) => (Ok(p == *v)),
            (CPattern::Literal(CLiteral::String(p)), Value::String(v)) => (Ok(p == v.as_str())),
            (CPattern::Variable(id), value) => {
                thread.slots.last_mut().unwrap()[id.0] = value.clone();
                Ok(true)
            },
            (CPattern::Destructure(patterns), Value::Struct(values)) => {
                for (pattern, value) in patterns.iter().zip_exact(values) {
                    if !self.do_pattern_apply(heap, thread, *pattern, &value)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            },
            (CPattern::DestructureEnum(p, patterns), Value::Variant(v, values)) if p == *v => {
                for (pattern, value) in patterns.iter().zip_exact(values) {
                    if !self.do_pattern_apply(heap, thread, *pattern, &value)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            },
            _ => Ok(false),
        }
    }

    fn do_continue(&self, thread: &mut Thread<'a>, id: LoopId) -> IResult<State<'a>> {
        loop {
            match thread.control.last() {
                Some(Control::Scope) | Some(Control::AsyncScope(_)) | None => {
                    unreachable!();
                },
                Some(Control::Loop(other_id, expr)) if *other_id == id => {
                    return Ok(State::Expression(*expr));
                },
                Some(_) => {
                    thread.control.pop();
                },
            }
        }
    }

    fn do_break(
        &self,
        thread: &mut Thread<'a>,
        id: LoopId,
        value: Value<'a>,
    ) -> IResult<State<'a>> {
        loop {
            match thread.control.pop() {
                Some(Control::Scope) | Some(Control::AsyncScope(_)) | None => {
                    unreachable!();
                },
                Some(Control::Loop(other_id, _)) if other_id == id => {
                    return Ok(State::Value(value));
                },
                Some(_) => {
                    // Already popped...
                },
            }
        }
    }

    fn do_return(
        &self,
        heap: &mut Heap<'a>,
        thread: &mut Thread<'a>,
        value: Value<'a>,
    ) -> IResult<State<'a>> {
        loop {
            match thread.control.pop() {
                None => unreachable!(),
                Some(Control::AsyncScope(awaitable)) => {
                    thread.slots.pop().unwrap();
                    heap.store_awaitable_complete(awaitable.clone(), value.clone());

                    return Ok(State::Value(Value::Struct(vec![
                        Value::Variant("Complete", vec![value]),
                        awaitable,
                    ])));
                },
                Some(Control::Scope) => {
                    thread.slots.pop().unwrap();
                    return Ok(State::Value(value));
                },
                Some(_) => {
                    // Already popped...
                },
            }
        }
    }

    fn do_call(
        &self,
        heap: &mut Heap<'a>,
        thread: &mut Thread<'a>,
        fun: CFunction<'a>,
        captures: Vec<(CStackId, Value<'a>)>,
        mut args: Vec<Value<'a>>,
    ) -> IResult<State<'a>> {
        match fun {
            CFunction::Simple {
                slots,
                parameters,
                body,
            } => {
                let mut new_slots = vec![Value::Undefined; slots];

                for (id, value) in captures {
                    new_slots[id.0] = value;
                }

                if let Some(Control::Scope) = thread.control.last() {
                    *thread.slots.last_mut().unwrap() = new_slots;
                } else {
                    thread.control.push(Control::Scope);
                    thread.slots.push(new_slots);
                }

                for (param, arg) in parameters.iter().zip_exact(args) {
                    if !self.do_pattern_apply(heap, thread, *param, &arg)? {
                        unreachable!();
                    }
                }

                Ok(State::Expression(body))
            },
            CFunction::Dispatch(idx) => {
                assert!(captures.is_empty());

                let old_self = std::mem::replace(&mut args[0], Value::Undefined);
                // Unbox the dyn and replace the arg with the unboxed self (thanks object
                // safety!)
                let (new_self, fun_id) = heap.index_dyn(old_self, idx);
                args[0] = new_self;
                // Grab the function
                let fun = self.program.functions[&fun_id];

                self.do_call(heap, thread, fun, vec![], args)
            },
            CFunction::DynamicBox(vtable) => {
                assert!(captures.is_empty());

                let value = args.into_iter().nth(0).unwrap();
                Ok(State::Value(heap.allocate_dyn(value, vtable)))
            },
            CFunction::Extern(name, generics) => {
                assert!(captures.is_empty());

                if let Some(fun) = INTERNAL_FUNCTIONS.get(name) {
                    fun(self, heap, thread, generics, args)
                } else {
                    unreachable!("Missing internal function {}", name);
                }
            },
        }
    }
}
