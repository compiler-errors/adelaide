use std::path::PathBuf;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    lexer::Span,
    lowering::{LScopeItem, LUseError, LUseItem},
    parser::{PEnum, PModule},
    util::Id,
};

pub type AResult<T> = std::result::Result<T, AError>;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum AError {
    IOErrorInArgumentPath(String, String),
    IOErrorInPathChild(PathBuf, String),
    IOErrorInFile(String, String),
    Utf8Error(String, String),
    SpanError(Span, String),
    Span2Error(Span, Span, String),
    Duplicated(&'static str, Span, &'static str, Span, &'static str, String),
    ModuleMissingItem(Id<PModule>, Id<str>, Span),
    ImportCycle(Vec<Span>, Id<str>),
    UseItemNotAModule(LUseItem, Span),
    ScopeItemNotAModule(LScopeItem, Span),
    IncorrectGenerics(Span, usize, usize),
    MissingIn(&'static str, Id<str>, Span, &'static str, Id<str>, Span),
    NotANumber(&'static str, Span, Id<str>),
}

impl AError {
    pub fn into_diagnostic(self, ctx: &dyn AdelaideContext) -> Diagnostic<Id<AFile>> {
        match self {
            AError::SpanError(a, why) => Diagnostic::error()
                .with_message(why)
                .with_labels(vec![a.into_label()]),
            AError::Span2Error(a, b, why) => Diagnostic::error()
                .with_message(why)
                .with_labels(vec![a.into_label(), b.into_label()]),
            AError::Duplicated(kind, a, what_a, b, what_b, name) => Diagnostic::error()
                .with_message(format!("Duplicated {} with name `{}`", kind, name))
                .with_labels(vec![
                    a.into_label()
                        .with_message(format!("Named by this {} here", what_a)),
                    b.into_label()
                        .with_message(format!("Named by this {} here", what_b)),
                ]),
            AError::IOErrorInArgumentPath(a, b) => Diagnostic::error()
                .with_message(format!("IO error when reading input file `{}`", a))
                .with_notes(vec![b]),
            AError::IOErrorInPathChild(a, b) => Diagnostic::error()
                .with_message(format!(
                    "IO error when reading path `{}`",
                    a.to_string_lossy()
                ))
                .with_notes(vec![b]),
            AError::IOErrorInFile(a, b) => Diagnostic::error()
                .with_message(format!("IO error when reading module `{}`", a))
                .with_notes(vec![b]),
            AError::Utf8Error(a, b) => Diagnostic::error()
                .with_message(format!("UTF8 decode error when reading module `{}`", a))
                .with_notes(vec![b]),
            AError::ModuleMissingItem(a, b, s) => Diagnostic::error()
                .with_message(format!(
                    "Module `{}` has no item `{}`",
                    a.lookup(ctx).source.lookup(ctx).mod_path.join("::"),
                    b.lookup(ctx)
                ))
                .with_labels(vec![s.into_label().with_message(format!("Imported here"))]),
            AError::ImportCycle(a, b) => {
                debug!("{} many elements in cycle", a.len());
                Diagnostic::error()
                    .with_message(format!(
                        "Import cycle detected when importing `{}`",
                        b.lookup(ctx)
                    ))
                    .with_labels(a.iter().rev().map(|s| s.into_label()).collect())
            },
            AError::UseItemNotAModule(k, u) => {
                let (what, name, s) = k.info(ctx);
                Diagnostic::error()
                    .with_message(format!(
                        "Cannot get child from {} `{}`, it is not a module or enum",
                        what,
                        name.lookup(ctx)
                    ))
                    .with_labels(vec![
                        s.into_label().with_message("Defined here"),
                        u.into_label().with_message("Accessed here"),
                    ])
            },
            AError::ScopeItemNotAModule(k, u) => {
                let (what, name, s) = k.info(ctx);
                Diagnostic::error()
                    .with_message(format!(
                        "Cannot get child from {} `{}`, it is not a module or enum",
                        what,
                        name.lookup(ctx)
                    ))
                    .with_labels(vec![
                        s.into_label().with_message("Defined here"),
                        u.into_label().with_message("Accessed here"),
                    ])
            },
            AError::IncorrectGenerics(s, given, expected) => Diagnostic::error()
                .with_message(format!(
                    "Incorrect number of generics... expected {} types, provided {}",
                    expected, given
                ))
                .with_labels(vec![s.into_label()]),
            AError::MissingIn(kind, def_name, defined, what, name, used) => Diagnostic::error()
                .with_message(format!(
                    "The {} `{}` is missing {} `{}`",
                    kind,
                    def_name.lookup(ctx),
                    what,
                    name.lookup(ctx)
                ))
                .with_labels(vec![
                    defined
                        .into_label()
                        .with_message(format!("The {} is defined here", kind)),
                    used.into_label()
                        .with_message(format!("The missing {} is referenced here", what)),
                ]),
            AError::NotANumber(what, s, i) => Diagnostic::error()
                .with_message(format!(
                    "Cannot parse {} `{}` as a number",
                    what,
                    i.lookup(ctx)
                ))
                .with_labels(vec![s.into_label()]),
        }
    }
}

impl From<LUseError> for AError {
    fn from(e: LUseError) -> Self {
        match e {
            LUseError::Error(a) => a,
            LUseError::MissingItem(a, b, c) => AError::ModuleMissingItem(a, b, c),
            LUseError::MissingIn(a, b, c, d, e, f) => AError::MissingIn(a, b, c, d, e, f),
            LUseError::Cycle(a, b) => AError::ImportCycle(a, b),
            LUseError::NotAModule(a, s) => AError::UseItemNotAModule(a, s),
        }
    }
}
