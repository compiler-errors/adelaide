use std::path::PathBuf;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{file::AFile, lexer::Span, util::Id};

pub type AResult<T> = std::result::Result<T, AError>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AError {
    InvalidModuleError(String),
    IOErrorInArgumentPath(String, String),
    IOErrorInPathChild(PathBuf, String),
    IOErrorInFile(Id<AFile>, String),
    Utf8Error(Id<AFile>, String),
    LexerError(Span, String),
    ParseError(Span, String),
    Parse2Error(Span, Span, String),
}

impl Into<Diagnostic<Id<AFile>>> for AError {
    fn into(self) -> Diagnostic<Id<AFile>> {
        match self {
            AError::ParseError(Span(file, start, end), why)
            | AError::LexerError(Span(file, start, end), why) => Diagnostic::error()
                .with_message(why)
                .with_labels(vec![Label::primary(file, start..end)]),
            AError::Parse2Error(Span(file1, start1, end1), Span(file2, start2, end2), why) =>
                Diagnostic::error().with_message(why).with_labels(vec![
                    Label::primary(file1, start1..end1),
                    Label::primary(file2, start2..end2),
                ]),
            _ => todo!(),
        }
    }
}
