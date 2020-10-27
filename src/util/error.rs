use std::path::PathBuf;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    lexer::Span,
    lowering::{LUseError, LUseItem},
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
    LowerDuplicate(Span, &'static str, Span, &'static str, String),
    LowerMissing(Id<PModule>, Id<str>, Span),
    LowerMissingVariant(Id<PEnum>, Id<str>),
    LowerCycle(Vec<Span>, Id<str>),
    LowerNotAModule(LUseItem),
}

impl AError {
    pub fn into_diagnostic(self, ctx: &dyn AdelaideContext) -> Diagnostic<Id<AFile>> {
        match self {
            AError::SpanError(Span(file, start, end), why) => Diagnostic::error()
                .with_message(why)
                .with_labels(vec![Label::primary(file, start..end)]),
            AError::Span2Error(Span(file1, start1, end1), Span(file2, start2, end2), why) =>
                Diagnostic::error().with_message(why).with_labels(vec![
                    Label::primary(file1, start1..end1),
                    Label::primary(file2, start2..end2),
                ]),
            AError::LowerDuplicate(
                Span(file_a, start_a, end_a),
                what_a,
                Span(file_b, start_b, end_b),
                what_b,
                name,
            ) => Diagnostic::error()
                .with_message(format!("Duplicate item with name `{}`", name))
                .with_labels(vec![
                    Label::primary(file_a, start_a..end_a)
                        .with_message(format!("Named by this {} here...", what_a)),
                    Label::primary(file_b, start_b..end_b)
                        .with_message(format!("Named by this {} here...", what_b)),
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
            AError::LowerMissing(a, b, s) => Diagnostic::error()
                .with_message(format!(
                    "Module `{}` is missing item `{}`",
                    a.lookup(ctx).file_id.lookup(ctx).mod_path.join("::"),
                    b.lookup(ctx)
                ))
                .with_labels(vec![s.into_label().with_message(format!("At this import"))]),
            AError::LowerMissingVariant(a, b) => Diagnostic::error()
                .with_message(format!(
                    "Enum `{}` is missing variant `{}`",
                    a.lookup(ctx).name.lookup(ctx),
                    b.lookup(ctx)
                ))
                .with_labels(vec![a
                    .lookup(ctx)
                    .span
                    .into_label()
                    .with_message(format!("In this enum"))]),
            AError::LowerCycle(a, b) => {
                debug!("{} many elements in cycle", a.len());
                Diagnostic::error()
                    .with_message(format!(
                        "Import cycle detected when importing `{}`",
                        b.lookup(ctx)
                    ))
                    .with_labels(a.iter().rev().map(|s| s.into_label()).collect())
            },
            AError::LowerNotAModule(k) => {
                let (what, name, Span(file, start, end)) = k.info(ctx);
                Diagnostic::error()
                    .with_message(format!(
                        "Cannot get child from {} `{}`, it is not a module",
                        what,
                        name.lookup(ctx)
                    ))
                    .with_labels(vec![Label::primary(file, start..end)])
            },
        }
    }
}

impl From<LUseError> for AError {
    fn from(e: LUseError) -> Self {
        match e {
            LUseError::Error(a) => a,
            LUseError::Missing(a, b, c) => AError::LowerMissing(a, b, c),
            LUseError::MissingVariant(a, b) => AError::LowerMissingVariant(a, b),
            LUseError::Cycle(a, b) => AError::LowerCycle(a, b),
            LUseError::NotAModule(a) => AError::LowerNotAModule(a),
        }
    }
}
