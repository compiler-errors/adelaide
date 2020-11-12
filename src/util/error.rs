use std::path::PathBuf;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    lexer::Span,
    lowering::{LScopeItem, LUseError, LUseItem},
    parser::{PEnum, PModPath, PModule},
    util::Id,
};

pub type AResult<T> = std::result::Result<T, AError>;

#[derive(Debug, Clone, Hash, Eq, PartialEq, Diagnostic)]
pub enum AError {
    #[message = "IO Error while reading {child_path}"]
    #[note = "{io_error}"]
    IOErrorInArgumentPath {
        child_path: String,
        io_error: String,
    },

    #[message = "IO error while reading children from {path}"]
    #[note = "{io_error}"]
    IOErrorInPathChild { path: PathBuf, io_error: String },

    #[message = "IO error while reading {path}"]
    #[note = "{io_error}"]
    IOError { path: PathBuf, io_error: String },

    #[message = "UTF-8 error while reading {path}"]
    #[note = "{io_error}"]
    Utf8Error { path: PathBuf, io_error: String },

    #[message = "Lexer error: {why}"]
    LexerError {
        #[span]
        span: Span,
        why: String,
    },

    #[message = "Parser error: {why}"]
    ParserError {
        #[span]
        span: Span,
        why: String,
    },

    #[message = "Unexpected token {given}, expected {expected}"]
    UnexpectedToken {
        given: String,
        expected: String,
        #[span]
        span: Span,
    },

    #[message = "Duplicated item `{name}`"]
    DuplicatedItem {
        what: &'static str,
        #[span = "Named by {what} defined here"]
        span: Span,
        what2: &'static str,
        #[span = "Named by {what2} defined here"]
        span2: Span,
        name: Id<str>,
    },

    #[message = "Duplicated definition of {kind} `{name}`"]
    DuplicatedDefinition {
        kind: &'static str,
        name: Id<str>,
        #[span]
        span: Span,
        #[span]
        span2: Span,
    },

    #[message = "The {kind} `{name}` is not a module, cannot access item from it"]
    ItemIsNotAModule {
        kind: &'static str,
        name: Id<str>,
        #[span = "The {kind} is defined here"]
        def_span: Span,
        #[span = "Tried to access child item here"]
        use_span: Span,
    },

    #[message = "The {parent_kind} `{parent_name}` is missing {child_kind} `{child_name}`"]
    MissingSubItem {
        parent_kind: &'static str,
        parent_name: Id<str>,
        #[span = "The {parent_kind} `{parent_name}` is defined here"]
        parent_span: Span,
        child_kind: &'static str,
        child_name: Id<str>,
        #[span = "Tried to access {child_kind} `{child_name}` here"]
        use_span: Span,
    },

    #[message = "The {parent_kind} `{parent_name}` is missing {child_kind} `{child_name}`"]
    MissingSubItemNoUsage {
        parent_kind: &'static str,
        parent_name: Id<str>,
        #[span = "The {parent_kind} `{parent_name}` is defined here"]
        parent_span: Span,
        child_kind: &'static str,
        child_name: Id<str>,
    },

    #[message = "Unexpected {item_kind} `{item_name}` in {parent_kind} `{parent_name}`"]
    UnexpectedSubItem {
        parent_kind: &'static str,
        parent_name: Id<str>,
        item_kind: &'static str,
        item_name: Id<str>,
        #[span]
        span: Span,
    },

    #[message = "Expected {item_kind} `{item_name}` in {parent_kind} `{parent_name}`"]
    ExpectedSubItem {
        parent_kind: &'static str,
        parent_name: Id<str>,
        item_kind: &'static str,
        item_name: Id<str>,
        #[span]
        span: Span,
    },

    #[message = "No such item `{name}` in `{mod_path}`"]
    MissingItemInModule {
        mod_path: PModPath,
        name: Id<str>,
        #[span = "Referenced here"]
        use_span: Span,
    },

    #[message = "No such item `{name}`"]
    MissingItem {
        #[span]
        span: Span,
        name: Id<str>,
    },

    #[message = "No such loop label `{name}`"]
    MissingLabel {
        #[span]
        span: Span,
        name: Id<str>,
    },

    #[message = "Not in a loop"]
    NotInLoop {
        #[span]
        span: Span,
    },

    #[message = "Illegal infer type `_`"]
    IllegalInfer {
        #[span]
        span: Span,
    },

    #[message = "The `return` operator is not allowed outside of a function, closure, or async \
                 block"]
    IllegalReturn {
        #[span]
        span: Span,
    },

    #[message = "The `await` operator is not allowed outside of an async block"]
    IllegalAwait {
        #[span]
        span: Span,
    },

    #[message = "An elaborated type is not allowed outside of a static call or associated type"]
    IllegalElaboration {
        #[span]
        span: Span,
    },

    #[message = "The {kind} `{name}` is not a type"]
    ItemIsNotAType {
        kind: &'static str,
        name: Id<str>,
        #[span = "The {kind} is defined here"]
        def_span: Span,
        #[span = "Referenced here"]
        use_span: Span,
    },

    #[message = "The {kind} `{name}` is not a trait"]
    ItemIsNotATrait {
        kind: &'static str,
        name: Id<str>,
        #[span = "The {kind} is defined here"]
        def_span: Span,
        #[span = "Referenced here"]
        use_span: Span,
    },

    #[message = "Duplicated trait bound `{name}`"]
    DuplicatedTraitBound {
        name: Id<str>,
        #[span]
        span: Span,
        #[span]
        span2: Span,
    },

    #[message = "Attempted to `allocate` a struct, use the struct literal syntax instead"]
    #[note = "If you meant to make a static call, use `::` instead of `:`"]
    TriedAllocatingStruct {
        parent_name: Id<str>,
        #[span = "Struct defined here"]
        parent_span: Span,
        #[span]
        use_span: Span,
    },

    #[message = "Attempted to construct an object on the stack, use the `allocate` operator \
                 instead"]
    TriedConstructingObject {
        parent_name: Id<str>,
        #[span = "Object defined here"]
        parent_span: Span,
        #[span]
        use_span: Span,
    },

    #[message = "Attempted to destructure an object, when it must be a struct"]
    TriedDestructuringObject {
        parent_name: Id<str>,
        #[span = "Object defined here"]
        parent_span: Span,
        #[span]
        use_span: Span,
    },

    #[message = "Cannot attach generic types to the bare enum variant \
                 `{enum_name}::{variant_name}`"]
    BareEnumGenerics {
        enum_name: Id<str>,
        variant_name: Id<str>,
        #[span = "Remove the generics from this usage, or fully qualify the enum"]
        span: Span,
    },

    #[message = "Cannot attach generic types to the {kind} `{name}`"]
    DenyGenerics {
        kind: &'static str,
        name: Id<str>,
        #[span = "Remove the generics from this usage"]
        use_span: Span,
        #[span = "Variable defined here"]
        def_span: Span,
    },

    #[message = "Cannot parse {kind} `{number}` as a number"]
    NotANumber {
        kind: &'static str,
        number: Id<str>,
        #[span]
        span: Span,
    },

    #[message = "Expected {expected} {kind}, but found {given} {kind}"]
    ParityDisparity {
        kind: &'static str,
        expected: usize,
        #[span = "The {kind} are defined here"]
        expected_span: Span,
        given: usize,
        #[span]
        given_span: Span,
    },

    #[message = "The {kind} `{name}` is not an enum, cannot construct variant `{variant}` from it"]
    NotAnEnumVariant {
        kind: &'static str,
        name: Id<str>,
        variant: Id<str>,
        #[span]
        use_span: Span,
    },

    #[message = "The {kind} `{name}` has no members"]
    CannotAccessMembers {
        kind: &'static str,
        name: Id<str>,
        mem: Id<str>,
        #[span = "Tried to access member `{mem}` here"]
        use_span: Span,
        #[span]
        def_span: Span,
    },

    #[message = "The {kind} `{name}` cannot be treated as an expression"]
    NotAnExpression {
        kind: &'static str,
        name: Id<str>,
        #[span = "The {kind} is defined here"]
        def_span: Span,
        #[span = "The {kind} was used here"]
        use_span: Span,
    },

    #[message = "Cannot construct or allocate {kind} `{name}`"]
    CannotConstruct {
        kind: &'static str,
        name: Id<str>,
        #[span = "Tried to construct {kind} here"]
        use_span: Span,
        #[span]
        def_span: Span,
    },

    #[message = "Incorrectly constructing {parent_kind} `{parent_name}`"]
    IncorrectConstructor {
        parent_kind: &'static str,
        parent_name: Id<str>,
        expected_kind: &'static str,
        #[span = "Expected {expected_kind} arguments"]
        expected_span: Span,
        given_kind: &'static str,
        #[span = "Given {given_kind} arguments"]
        given_span: Span,
    },
}

impl From<LUseError> for AError {
    fn from(l: LUseError) -> Self {
        match l {
            LUseError::MissingItem(mod_path, name, use_span) => AError::MissingItemInModule {
                mod_path,
                name,
                use_span,
            },
            LUseError::Cycle(_, _) => todo!(),
            LUseError::Error(a) => a,
            LUseError::NotAModule(kind, name, def_span, use_span) => AError::ItemIsNotAModule {
                kind,
                name,
                def_span,
                use_span,
            },
            LUseError::MissingSubItem {
                parent_kind,
                parent_name,
                parent_span,
                child_kind,
                child_name,
                use_span,
            } => AError::MissingSubItem {
                parent_kind,
                parent_name,
                parent_span,
                child_kind,
                child_name,
                use_span,
            },
        }
    }
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self, ctx: &dyn AdelaideContext) -> Diagnostic<Id<AFile>>;
}
