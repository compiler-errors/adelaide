use std::{
    collections::{HashMap, VecDeque},
    time::Instant,
};

use crate::{
    lowering::{fresh_id, LGlobal},
    translate::{CExpression, CFunction, CFunctionId, CStackId, CVTable},
    util::Id,
};

use super::thread::{fresh_thread_id, Control, Thread, ThreadId};

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
