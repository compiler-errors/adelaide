use std::sync::Arc;

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    util::{AError, AResult, Id},
};

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct RawFile {
    pub contents: String,
}

pub struct RawFileSource(Arc<RawFile>);

impl From<Arc<RawFile>> for RawFileSource {
    fn from(a: Arc<RawFile>) -> Self {
        RawFileSource(a)
    }
}

impl AsRef<str> for RawFileSource {
    fn as_ref(&self) -> &str {
        &self.0.contents
    }
}

pub fn read_file<'ctx>(
    ctx: &'ctx dyn AdelaideContext,
    file_id: Id<AFile>,
) -> AResult<Arc<RawFile>> {
    let file = file_id.lookup(ctx);

    let contents = if let Some(path) = &file.path {
        if path.is_file() {
            let contents_raw = std::fs::read(&path).map_err(|e| map_io_error(ctx, file_id, e))?;
            String::from_utf8(contents_raw).map_err(|e| map_utf8_error(ctx, file_id, e))?
        } else {
            "".to_string()
        }
    } else {
        "".to_string()
    };

    Ok(Arc::new(RawFile { contents }))
}

fn map_io_error(ctx: &dyn AdelaideContext, file_id: Id<AFile>, e: std::io::Error) -> AError {
    let path = file_id.lookup(ctx).path.clone().unwrap();
    AError::IOErrorInPath {
        path,
        io_error: format!("{}", e),
    }
}

fn map_utf8_error(
    ctx: &dyn AdelaideContext,
    file_id: Id<AFile>,
    e: std::string::FromUtf8Error,
) -> AError {
    let path = file_id.lookup(ctx).path.clone().unwrap();
    AError::Utf8Error {
        path,
        io_error: format!("{}", e),
    }
}
