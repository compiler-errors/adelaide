use std::{
    collections::HashSet,
    sync::atomic::{AtomicUsize, Ordering},
    time::Instant,
};

use crate::{
    lowering::LoopId,
    translate::{CExpression, CFunction, CPattern, CStackId, CStatement},
};

use super::heap::Value;

/// How long does the thread spin until preempted (in milliseconds)
pub const THREAD_QUANTUM_MILLIS: u128 = 50;

static THREAD_IDS: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct ThreadId(pub usize);

pub fn fresh_thread_id() -> ThreadId {
    ThreadId(THREAD_IDS.fetch_add(1, Ordering::Relaxed))
}

pub struct Thread<'a> {
    pub id: ThreadId,
    pub start: Instant,

    pub control: Vec<Control<'a>>,
    pub slots: Vec<Vec<Value<'a>>>,

    pub blocking: HashSet<ThreadId>,
    pub blocked_on: HashSet<ThreadId>,

    pub handle: Value<'a>,
}

pub enum ThreadState<'a> {
    Complete(Value<'a>),
    Incomplete,
}

pub enum State<'a> {
    Expression(CExpression<'a>),
    LvalExpression(CExpression<'a>),
    Statement(CStatement<'a>),
    Value(Value<'a>),
}

pub enum Control<'a> {
    Parked(State<'a>),
    Scope,
    GeneratorScope(Value<'a>),
    FirstResume(Option<CStackId>, CExpression<'a>),
    Block(&'a [CStatement<'a>], CExpression<'a>),
    Invoke(CFunction<'a>, &'a [CExpression<'a>], Vec<Value<'a>>),
    /// Allocate a structure.
    ///
    /// The first element is the remaining expressions which need to be
    /// evaluated, the second is the contents of the structure (filled with
    /// Undefined, initially), and the final value is an index that signifies
    /// where to place the value we're currently evaluating.
    Struct(&'a [(usize, CExpression<'a>)], Vec<Value<'a>>, usize),
    /// Allocate a heap collection. Identical to Struct other than the final
    /// result will be stored in the heap, as opposed to by value.
    Object(&'a [(usize, CExpression<'a>)], Vec<Value<'a>>, usize),
    /// Allocate a variant. Identical to Struct, but also has an enum variant
    Variant(
        &'a str,
        &'a [(usize, CExpression<'a>)],
        Vec<Value<'a>>,
        usize,
    ),
    StructAccess(usize),
    ObjectAccess(usize),
    Or(CExpression<'a>),
    And(CExpression<'a>),
    If(CExpression<'a>, CExpression<'a>),
    Return,
    Yield,
    Break(LoopId),
    Loop(LoopId, CExpression<'a>),
    Match(&'a [(CPattern<'a>, CExpression<'a>)]),
    InfallibleApply(CPattern<'a>),
    ApplyTo(CExpression<'a>),
    Apply {
        rval: Value<'a>,
        indices_rev: Vec<usize>,
    },
    ApplyObject {
        rval: Value<'a>,
        object_idx: usize,
        indices_rev: Vec<usize>,
    },
}
