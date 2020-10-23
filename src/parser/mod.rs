lalrpop_mod!(pub cheshire, "/parser/cheshire.rs");

mod ast;
mod util;

use std::fmt::Display;

pub use ast::*;
use lalrpop_util::ParseError;

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    lexer::{Lexer, Span, SpanToken, Token},
    util::{AError, AResult, Id, Intern},
};

pub fn parse_mod(ctx: &dyn AdelaideContext, file_id: Id<AFile>) -> AResult<Id<PModule>> {
    let raw_mod = ctx.read_file(file_id)?;

    let lexer = Lexer::new(ctx, file_id, &raw_mod.contents).map(|s| match s {
        // I absolutely hate the fact that I have to do this, but this is a limitation of LALRPOP.
        Ok(SpanToken(Span(_, l, h), t)) => Ok((l, t, h)),
        Err(e) => Err(e),
    });

    let mut items = vec![];

    for c in ctx.literal_module_children(file_id) {
        items.push(PItem::Module(ctx.parse_mod(c)?));
    }

    let parsed_items = cheshire::ModuleParser::new()
        .parse(file_id, ctx, lexer)
        .map_err(|e| map_parse_error(file_id, e))?;
    items.extend(parsed_items);

    Ok(PModule {
        file_id: file_id.into(),
        items,
    }
    .intern(ctx))
}

fn map_parse_error(file_id: Id<AFile>, error: ParseError<usize, Token, AError>) -> AError {
    match error {
        ParseError::User { error } => error,
        ParseError::UnrecognizedToken {
            token: (l, t, h),
            expected,
        } => AError::SpanError(
            Span(file_id, l, h),
            format!("Unexpected token {}, expected {}", t, CommaSep(expected)),
        ),
        ParseError::UnrecognizedEOF { location, expected } => AError::SpanError(
            Span(file_id, location.saturating_sub(1), location),
            format!("Unexpected EOF, expected {}", CommaSep(expected)),
        ),
        _ => todo!(),
    }
}

struct CommaSep(Vec<String>);

impl Display for CommaSep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.len() {
            0 => panic!("Unexpected"),
            1 => write!(f, "{}", self.0[0]),
            2 => write!(f, "{} or {}", self.0[0], self.0[1]),
            _ => {
                for (i, x) in self.0.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                        if i == self.0.len() - 1 {
                            write!(f, "or ")?;
                        }
                    }

                    write!(f, "{}", x)?;
                }

                Ok(())
            },
        }
    }
}
