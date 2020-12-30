use std::path::PathBuf;

use codespan_reporting::diagnostic::Diagnostic;

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    lexer::Span,
    lowering::LUseError,
    parser::PModPath,
    typechecker::{TImplWitness, TTraitType, TType},
    util::Id,
};

pub type AResult<T> = std::result::Result<T, AError>;

#[derive(Debug, Clone, Hash, Eq, PartialEq, Diagnostic, PrettyPrint)]
pub enum AError {
    /*#[message = "This should never happen"]
    Oof {
        #[span]
        span: Span,
    },*/
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
    IOErrorInPath { path: PathBuf, io_error: String },

    #[message = "IO error"]
    #[note = "{io_error}"]
    IOError { io_error: String },

    #[message = "Broken pipe"]
    BrokenPipe,

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

    #[message = "The {kind} `{name}` is not a module, cannot access item from it"]
    ItemIsNotAModuleNoUsage {
        kind: &'static str,
        name: Id<str>,
        #[span = "The {kind} is defined here"]
        def_span: Span,
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

    #[message = "Missing field `{item_name}` in constructor of {parent_kind} `{parent_name}`"]
    ExpectedField {
        parent_kind: &'static str,
        parent_name: Id<str>,
        item_name: Id<str>,
        #[span]
        def_span: Span,
        #[span]
        use_span: Span,
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

    #[message = "Illegal associated type"]
    IllegalAssoc {
        #[span]
        span: Span,
    },

    #[message = "Illegal `Self` type"]
    IllegalSelf {
        #[span]
        span: Span,
    },

    #[message = "Cannot implement trait `Concrete`."]
    #[note = "Trait is automatically implemented for types that are not `Dyn`."]
    CannotImplementConcrete {
        #[span]
        span: Span,
    },

    #[message = "No such method `{name}` in trait `{trait_name}`"]
    NoMethod {
        trait_name: Id<str>,
        #[span = "Trait `{trait_name}` defined here"]
        trait_span: Span,
        name: Id<str>,
        #[span = "Method called here"]
        use_span: Span,
    },

    // TODO: Make this message better, lol
    #[message = "Impl is an orphan, must be declared in either the trait's module or the type's \
                 module"]
    Orphan {
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

    #[message = "Attempted to construct an opaque object/struct"]
    TriedConstructingOpaque {
        parent_name: Id<str>,
        #[span = "Object/struct defined as opaque here"]
        opaque_span: Span,
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
        use_span: Span,
        #[span = "Variant defined here"]
        def_span: Span,
    },

    #[message = "Cannot attach generic types to the {kind} `{name}`"]
    DenyGenerics {
        kind: &'static str,
        name: Id<str>,
        #[span = "Remove the generics from this usage"]
        use_span: Span,
        #[span = "{kind} defined here"]
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

    #[message = "Missing trait bound `{bound}`, dynamic type must be fully elaborated"]
    MissingTraitBound {
        trait_name: Id<str>,
        bound: Id<str>,
        #[span = "Add `{bound} = <type>` here"]
        use_span: Span,
        #[span = "Associated type defined here"]
        #[span]
        def_span: Span,
    },

    #[message = "Conflicting solutions:\n\n\n{solution} and\n\n\n{other_solution}"]
    ConflictingSolutions {
        solution: TImplWitness,
        other_solution: TImplWitness,
    },

    #[message = "No solution for trait {trait_ty} found for {ty}"]
    NoSolution {
        ty: Id<TType>,
        trait_ty: Id<TTraitType>,
        #[span]
        ty_span: Span,
        #[span]
        trait_ty_span: Span,
    },

    #[message = "Cannot determine the type `{ty}`"]
    AmbiguousType {
        ty: Id<TType>,
        #[span]
        use_span: Span,
    },

    #[message = "Cannot unify types `{left_ty}` and `{right_ty}`"]
    CannotUnify {
        left_ty: Id<TType>,
        right_ty: Id<TType>,
        #[span = "`{left_ty}`"]
        left_span: Span,
        #[span = "`{right_ty}`"]
        right_span: Span,
    },

    #[message = "Cannot unify types `{left_trait_ty}` and `{right_trait_ty}`"]
    CannotUnifyTraits {
        left_trait_ty: Id<TTraitType>,
        right_trait_ty: Id<TTraitType>,
        #[span]
        left_span: Span,
        #[span]
        right_span: Span,
    },

    #[message = "Cannot access member `{name}` for type `{ty}`"]
    CannotAccess {
        ty: Id<TType>,
        name: Id<str>,
        #[span]
        span: Span,
    },

    #[message = "Cannot access member index {idx} for type `{ty}`"]
    CannotAccessIdx {
        ty: Id<TType>,
        idx: usize,
        #[span]
        span: Span,
    },

    #[message = "Cannot determine the trait that provides method `{name}` for type `{call_ty}`"]
    #[note = "You can try to elaborate the call type like `<{call_ty} as Trait>::{name}`"]
    AmbiguousMethod {
        call_ty: Id<TType>,
        name: Id<str>,
        #[span]
        span: Span,
    },

    #[message = "The trait `{trait_name}` is not object-safe due to method `{method_name}`"]
    NotObjectSafeMethod {
        trait_name: Id<str>,
        method_name: Id<str>,
        #[span = "This method must be object-safe, try adding `where Self: Concrete`"]
        method_span: Span,
        #[span = "Required to be object-safe due to..."]
        use_span: Span,
    },

    #[message = "The trait `{trait_name}` is not object-safe due to its referencing type `{ty}`"]
    NotObjectSafeType {
        trait_name: Id<str>,
        ty: Id<TType>,
        #[span = "This type is not object-safe"]
        def_span: Span,
        #[span = "Required to be object-safe due to..."]
        use_span: Span,
    },

    #[message = "Two `main` functions declared in this program, expected only one"]
    ConflictingMain {
        #[span]
        span: Span,
        #[span]
        other_span: Span,
    },

    #[message = "Expected `main` to have signature `fn main() -> ()`"]
    BadMainSignature {
        #[span]
        span: Span,
    },

    #[message = "No `main` function declared in this program, expected one"]
    MissingMain,

    #[message = "This expression is not an lvalue, cannot be the left side of an assignment"]
    NotAnLValue { span: Span },
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self, ctx: &dyn AdelaideContext) -> Diagnostic<Id<AFile>>;
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

impl From<std::io::Error> for AError {
    fn from(e: std::io::Error) -> Self {
        if e.kind() == std::io::ErrorKind::BrokenPipe {
            AError::BrokenPipe
        } else {
            AError::IOError {
                io_error: format!("{}", e),
            }
        }
    }
}
