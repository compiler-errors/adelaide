use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter::once,
    time::Instant,
};

use crate::{
    lowering::{fresh_id, LGlobal},
    translate::{CExpression, CFunction, CFunctionId, CStackId, CVTable},
    util::Id,
};

use super::thread::{fresh_thread_id, Control, State, Thread, ThreadId};

pub struct Heap<'a> {
    pub globals: HashMap<Id<LGlobal>, Value<'a>>,

    pub thread_schedule: VecDeque<ThreadId>,
    pub ready_threads: HashMap<ThreadId, Thread<'a>>,
    pub blocked_threads: HashMap<ThreadId, Thread<'a>>,

    pub objects: HashMap<ObjectId, Vec<Value<'a>>>,
    pub generators: HashMap<GeneratorId, Generator<'a>>,
    pub dyn_boxes: HashMap<DynId, DynBox<'a>>,
}

#[derive(Clone, Debug)]
pub enum Value<'a> {
    Undefined,
    Int(i64),
    Float(f64),
    String(String),
    Struct(Vec<Value<'a>>),
    Variant(&'a str, Vec<Value<'a>>),
    Object(ObjectId, usize, usize),
    Generator(GeneratorId),
    Dyn(DynId),
    // TODO: Is this better represented by an `Option<Arc<[_]>>`?
    // Makes the callable slightly more cloneable.
    Callable(Vec<(CStackId, Value<'a>)>, CFunction<'a>),
}

impl<'a> Value<'a> {
    pub fn unit() -> Value<'a> {
        Value::Struct(vec![])
    }

    pub fn boolean(b: bool) -> Value<'a> {
        Value::Int(if b { 1 } else { 0 })
    }
}

pub enum Generator<'a> {
    Incomplete {
        stack_rev: Vec<Control<'a>>,
        slots: Vec<Value<'a>>,
    },
    Evaluating,
    Complete(Value<'a>),
}

pub struct DynBox<'a>(Value<'a>, CVTable<'a>);

impl<'a> Heap<'a> {
    pub fn new() -> Heap<'a> {
        Heap {
            globals: hashmap! {},

            thread_schedule: VecDeque::new(),
            ready_threads: hashmap! {},
            blocked_threads: hashmap! {},

            objects: hashmap! {},
            generators: hashmap! {},
            dyn_boxes: hashmap! {},
        }
    }

    pub fn allocate(&mut self, values: Vec<Value<'a>>) -> Value<'a> {
        let id = fresh_id();
        let len = values.len();

        self.objects.insert(id, values);

        Value::Object(id, 0, len)
    }

    pub fn allocate_generator(
        &mut self,
        slots: Vec<Value<'a>>,
        in_slot: Option<CStackId>,
        expr: CExpression<'a>,
    ) -> Value<'a> {
        let id = fresh_id();

        self.generators.insert(id, Generator::Incomplete {
            stack_rev: vec![
                Control::FirstResume(in_slot, expr),
                Control::GeneratorScope(Value::Generator(id)),
            ],
            slots,
        });

        Value::Generator(id)
    }

    pub fn allocate_dyn(&mut self, value: Value<'a>, vtable: CVTable<'a>) -> Value<'a> {
        let id = fresh_id();
        self.dyn_boxes.insert(id, DynBox(value, vtable));

        Value::Dyn(id)
    }

    pub fn index_allocated(&mut self, object: Value<'a>, idx: usize) -> &mut Value<'a> {
        if let Value::Object(id, start, _) = object {
            &mut self.objects.get_mut(&id).unwrap()[start + idx]
        } else {
            unreachable!()
        }
    }

    pub fn index_dyn(&mut self, value: Value<'a>, idx: usize) -> (Value<'a>, CFunctionId<'a>) {
        if let Value::Dyn(id) = value {
            let dyn_box = self.dyn_boxes.get_mut(&id).unwrap();

            (dyn_box.0.clone(), (dyn_box.1).0[idx])
        } else {
            unreachable!()
        }
    }

    pub fn unbox_dyn(&mut self, value: Value<'a>) -> Value<'a> {
        if let Value::Dyn(id) = value {
            self.dyn_boxes.get_mut(&id).unwrap().0.clone()
        } else {
            unreachable!()
        }
    }

    pub fn store_generator_evaluating(&mut self, generator: Value<'a>) -> Generator<'a> {
        if let Value::Generator(id) = generator {
            self.generators.insert(id, Generator::Evaluating).unwrap()
        } else {
            unreachable!();
        }
    }

    pub fn store_generator_incomplete(
        &mut self,
        generator: Value<'a>,
        stack_rev: Vec<Control<'a>>,
        slots: Vec<Value<'a>>,
    ) {
        if let Value::Generator(id) = generator {
            let incomplete = self
                .generators
                .insert(id, Generator::Incomplete { stack_rev, slots });
            assert!(matches!(incomplete, Some(Generator::Evaluating)));
        } else {
            unreachable!();
        }
    }

    pub fn store_generator_complete(&mut self, generator: Value<'a>, value: Value<'a>) {
        if let Value::Generator(id) = generator {
            let incomplete = self.generators.insert(id, Generator::Complete(value));
            assert!(matches!(incomplete, Some(Generator::Evaluating)));
        } else {
            unreachable!();
        }
    }

    pub fn new_thread(&mut self) -> Thread<'a> {
        let id: ThreadId = fresh_thread_id();
        // allocate the Thread object
        let thread = self.allocate(vec![Value::Int(id.0 as i64)]);
        // allocate the ThreadHandle object
        let thread_handle = self.allocate(vec![Value::Struct(vec![
            thread,
            Value::Variant("None", vec![]),
        ])]);

        Thread {
            id,
            start: Instant::now(),
            control: vec![],
            slots: vec![],
            blocking: hashset! {},
            blocked_on: hashset! {},
            handle: thread_handle,
        }
    }

    pub fn num_ready_threads(&self) -> usize {
        self.ready_threads.len()
    }

    pub fn pop_ready_thread(&mut self) -> Option<Thread<'a>> {
        self.thread_schedule
            .pop_front()
            .map(|idx| self.ready_threads.remove(&idx).unwrap())
    }

    pub fn push_ready_thread(&mut self, thread: Thread<'a>) {
        self.thread_schedule.push_back(thread.id);
        self.ready_threads.insert(thread.id, thread);
    }

    pub fn push_blocked_thread(&mut self, thread: Thread<'a>) {
        self.thread_schedule.push_back(thread.id);
        self.ready_threads.insert(thread.id, thread);
    }

    pub fn signal_thread(&mut self, current_thread_id: ThreadId, blocked_thread_id: ThreadId) {
        let blocked_thread = self.blocked_threads.get_mut(&blocked_thread_id).unwrap();
        blocked_thread.blocked_on.remove(&current_thread_id);

        if blocked_thread.blocked_on.is_empty() {
            let thread = self.blocked_threads.remove(&blocked_thread_id).unwrap();
            self.push_ready_thread(thread);
        }
    }

    pub fn block_on_thread(
        &mut self,
        blocked_thread: &mut Thread,
        blocking_thread_id: ThreadId,
    ) -> bool {
        if let Some(blocking_thread) = self.ready_threads.get_mut(&blocking_thread_id) {
            blocking_thread.blocking.insert(blocked_thread.id);
            blocked_thread.blocked_on.insert(blocking_thread_id);
            return true;
        }

        if let Some(blocking_thread) = self.blocked_threads.get_mut(&blocking_thread_id) {
            blocking_thread.blocking.insert(blocked_thread.id);
            blocked_thread.blocked_on.insert(blocking_thread_id);
            return true;
        }

        false
    }

    pub fn gc(&mut self, thread: &Thread<'a>) {
        let mut mark = GcMark {
            objects: hashset! {},
            generators: hashset! {},
            dyn_boxes: hashset! {},
        };

        // Mark all threads
        for t in self
            .blocked_threads
            .values()
            .chain(self.ready_threads.values())
            .chain(once(thread))
        {
            self.mark_thread(t, &mut mark)
        }

        // Mark all globals
        for g in self.globals.values() {
            self.mark_value(g, &mut mark)
        }

        self.objects.retain(|id, _| mark.objects.contains(id));
        self.generators.retain(|id, _| mark.generators.contains(id));
        self.dyn_boxes.retain(|id, _| mark.dyn_boxes.contains(id));
    }

    pub fn mark_thread(&self, thread: &Thread<'a>, mark: &mut GcMark) {
        for c in thread.control.iter() {
            self.mark_control(c, mark)
        }

        for v in thread.slots.iter().flat_map(|v| v.iter()) {
            self.mark_value(v, mark)
        }

        self.mark_value(&thread.handle, mark);
    }

    pub fn mark_control(&self, control: &Control<'a>, mark: &mut GcMark) {
        match control {
            Control::Parked(s) => {
                self.mark_state(s, mark);
            },
            Control::GeneratorScope(v)
            | Control::Apply { rval: v, .. }
            | Control::ApplyObject { rval: v, .. } => {
                self.mark_value(v, mark);
            },
            Control::Invoke(_, _, values)
            | Control::Struct(_, values, _)
            | Control::Object(_, values, _)
            | Control::Variant(_, _, values, _) =>
                for v in values {
                    self.mark_value(v, mark);
                },
            Control::FirstResume(_, _)
            | Control::Scope
            | Control::Block(_, _)
            | Control::StructAccess(_)
            | Control::ObjectAccess(_)
            | Control::Or(_)
            | Control::And(_)
            | Control::If(_, _)
            | Control::Return
            | Control::Yield
            | Control::Break(_)
            | Control::Loop(_, _)
            | Control::Match(_)
            | Control::InfallibleApply(_)
            | Control::ApplyTo(_) => {
                // Do nothing
            },
        }
    }

    pub fn mark_state(&self, state: &State<'a>, mark: &mut GcMark) {
        match state {
            State::Value(v) => {
                self.mark_value(v, mark);
            },
            State::Expression(_) | State::LvalExpression(_) | State::Statement(_) => {
                // Do nothing
            },
        }
    }

    pub fn mark_value(&self, value: &Value<'a>, mark: &mut GcMark) {
        match value {
            Value::Struct(values) | Value::Variant(_, values) =>
                for v in values {
                    self.mark_value(v, mark);
                },
            Value::Callable(values, _) =>
                for (_, v) in values {
                    self.mark_value(v, mark);
                },
            Value::Object(id, _, _) =>
                if mark.objects.insert(*id) {
                    for v in &self.objects[id] {
                        self.mark_value(v, mark);
                    }
                },
            Value::Generator(id) => {
                if mark.generators.insert(*id) {
                    match &self.generators[id] {
                        Generator::Incomplete { stack_rev, slots } => {
                            for v in slots {
                                self.mark_value(v, mark);
                            }

                            for s in stack_rev {
                                self.mark_control(s, mark);
                            }
                        },
                        Generator::Complete(v) => {
                            self.mark_value(v, mark);
                        },
                        Generator::Evaluating => {
                            // Do nothing
                        },
                    }
                }
            },
            Value::Dyn(id) =>
                if mark.dyn_boxes.insert(*id) {
                    self.mark_value(&self.dyn_boxes[id].0, mark);
                },
            Value::Undefined | Value::Int(_) | Value::Float(_) | Value::String(_) => {
                // Do nothing
            },
        }
    }
}

pub struct GcMark {
    pub objects: HashSet<ObjectId>,
    pub generators: HashSet<GeneratorId>,
    pub dyn_boxes: HashSet<DynId>,
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct ObjectId(pub usize);

impl From<usize> for ObjectId {
    fn from(u: usize) -> Self {
        Self(u)
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct GeneratorId(pub usize);

impl From<usize> for GeneratorId {
    fn from(u: usize) -> Self {
        Self(u)
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct DynId(pub usize);

impl From<usize> for DynId {
    fn from(u: usize) -> Self {
        Self(u)
    }
}
