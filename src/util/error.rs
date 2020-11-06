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
    ModuleDuplicateItem(Span, &'static str, Span, &'static str, String),
    ModuleMissingItem(Id<PModule>, Id<str>, Span),
    EnumMissingVariant(Span, Id<PEnum>, Id<str>),
    ImportCycle(Vec<Span>, Id<str>),
    UseItemNotAModule(LUseItem, Span),
    ScopeItemNotAModule(LScopeItem, Span),
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
            AError::ModuleDuplicateItem(a, what_a, b, what_b, name) => Diagnostic::error()
                .with_message(format!("Duplicate item with name `{}`", name))
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
            AError::EnumMissingVariant(s, a, b) => Diagnostic::error()
                .with_message(format!(
                    "Enum `{}` is missing variant `{}`",
                    a.lookup(ctx).name.lookup(ctx),
                    b.lookup(ctx)
                ))
                .with_labels(vec![
                    s.into_label().with_message("Referenced here"),
                    a.lookup(ctx)
                        .span
                        .into_label()
                        .with_message(format!("In this enum")),
                ]),
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
        }
    }
}

impl From<LUseError> for AError {
    fn from(e: LUseError) -> Self {
        match e {
            LUseError::Error(a) => a,
            LUseError::Missing(a, b, c) => AError::ModuleMissingItem(a, b, c),
            LUseError::MissingVariant(s, a, b) => AError::EnumMissingVariant(s, a, b),
            LUseError::Cycle(a, b) => AError::ImportCycle(a, b),
            LUseError::NotAModule(a, s) => AError::UseItemNotAModule(a, s),
        }
    }
}
