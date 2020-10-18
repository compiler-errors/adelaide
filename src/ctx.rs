use std::{cmp::Ordering, sync::Arc};

use codespan_reporting::files::Files;

use crate::{
    file::AFile,
    read::{RawFile, RawFileSource},
    util::{AResult, Id},
};

#[salsa::query_group(AdelaideStorage)]
pub trait AdelaideContext: salsa::Database {
    #[salsa::input]
    fn mod_tree_root(&self) -> Id<AFile>;

    fn line_starts(&self, key: Id<AFile>) -> Arc<[usize]>;

    fn line_start(&self, id: Id<AFile>, idx: usize) -> Option<usize>;

    fn literal_module_children(&self, data: Id<AFile>) -> Vec<Id<AFile>>;

    #[salsa::interned]
    fn intern_str(&self, data: Arc<str>) -> Id<str>;

    #[salsa::interned]
    fn intern_file(&self, data: Arc<AFile>) -> Id<AFile>;

    #[salsa::invoke(crate::read::read_file)]
    fn read_file(&self, key: Id<AFile>) -> AResult<Arc<RawFile>>;

    #[salsa::invoke(crate::lexer::lex_mod)]
    fn lex_mod(&self, key: Id<AFile>) -> AResult<()>;
}

fn line_starts(ctx: &dyn AdelaideContext, id: Id<AFile>) -> Arc<[usize]> {
    if let Some(source) = ctx.source(id) {
        codespan_reporting::files::line_starts(source.as_ref()).collect()
    } else {
        vec![].into()
    }
}

fn line_start(ctx: &dyn AdelaideContext, id: Id<AFile>, idx: usize) -> Option<usize> {
    let line_starts = ctx.line_starts(id);

    match idx.cmp(&line_starts.len()) {
        Ordering::Less => line_starts.get(idx).cloned(),
        Ordering::Equal => ctx.source(id).map(|f| f.as_ref().len()),
        Ordering::Greater => None,
    }
}

fn literal_module_children(ctx: &dyn AdelaideContext, file_id: Id<AFile>) -> Vec<Id<AFile>> {
    ctx.lookup_intern_file(file_id)
        .children
        .values()
        .cloned()
        .collect()
}

#[salsa::database(AdelaideStorage)]
#[derive(Default)]
pub struct AdelaideDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AdelaideDatabase {}

impl<'ctx> Files<'ctx> for dyn AdelaideContext {
    type FileId = Id<AFile>;
    type Name = String;
    type Source = RawFileSource;

    fn name(&'ctx self, id: Self::FileId) -> Option<Self::Name> {
        if id == self.mod_tree_root() {
            None
        } else {
            id.lookup(self)
                .path
                .as_ref()
                .map(|p| p.display().to_string())
        }
    }

    fn source(&'ctx self, id: Self::FileId) -> Option<Self::Source> {
        if let Ok(f) = self.read_file(id) {
            Some(f.into())
        } else {
            error!("Trouble reading file: {:?}", id);
            None
        }
    }

    fn line_index(&'ctx self, id: Self::FileId, byte_index: usize) -> Option<usize> {
        match self.line_starts(id).binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_range(
        &'ctx self,
        id: Self::FileId,
        line_index: usize,
    ) -> Option<std::ops::Range<usize>> {
        let line_start = self.line_start(id, line_index)?;
        let next_line_start = self.line_start(id, line_index + 1)?;

        Some(line_start..next_line_start)
    }
}

/// Stupid dummy impl because apparently I need it. This should get inlined to
/// the dyn impl above.
impl<'ctx> Files<'ctx> for AdelaideDatabase {
    type FileId = Id<AFile>;
    type Name = String;
    type Source = RawFileSource;

    fn name(&'ctx self, id: Self::FileId) -> Option<Self::Name> {
        (self as &dyn AdelaideContext).name(id)
    }

    fn source(&'ctx self, id: Self::FileId) -> Option<Self::Source> {
        (self as &dyn AdelaideContext).source(id)
    }

    fn line_index(&'ctx self, id: Self::FileId, byte_index: usize) -> Option<usize> {
        (self as &dyn AdelaideContext).line_index(id, byte_index)
    }

    fn line_range(
        &'ctx self,
        id: Self::FileId,
        line_index: usize,
    ) -> Option<std::ops::Range<usize>> {
        (self as &dyn AdelaideContext).line_range(id, line_index)
    }
}
