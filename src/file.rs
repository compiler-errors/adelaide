use crate::{
    ctx::AdelaideContext,
    util::{AError, AResult, Id, Intern},
};
use lazy_static::lazy_static;
use regex::Regex;
use std::{
    collections::BTreeMap,
    ffi::OsStr,
    path::{Path, PathBuf},
    sync::Arc,
};

lazy_static! {
    static ref MODULE_NAME_REGEX: Regex = Regex::new(r"^[a-z][a-zA-Z0-9_]*$").unwrap();
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct AFile {
    pub path: Option<PathBuf>,
    pub mod_path: Vec<String>,
    pub children: BTreeMap<String, Id<AFile>>,
}

pub fn initialize_from_path_arguments(
    ctx: &mut dyn AdelaideContext,
    input: Vec<String>,
) -> AResult<()> {
    let mut children: BTreeMap<String, Id<AFile>> = btreemap! {};

    for child_name in input {
        debug!("Initializing child: {:?}", child_name);

        let child_path = PathBuf::from(child_name.clone())
            .canonicalize()
            .map_err(|e| map_path_error(child_name, e))?;
        let child_name = expect_mod_name(child_path.file_stem())?;
        let child = initialize_child(ctx, &[], child_path)?;
        children.insert(child_name, child);
    }

    let root = AFile {
        path: None,
        mod_path: vec![],
        children,
    }
    .intern(ctx);

    ctx.set_mod_tree_root(root);
    Ok(())
}

pub fn initialize_child(
    ctx: &mut dyn AdelaideContext,
    mod_path: &[String],
    my_path: PathBuf,
) -> AResult<Id<AFile>> {
    if !is_valid_mod(my_path.as_path()) {
        unreachable!()
    }

    let mod_name = expect_mod_name(my_path.file_stem())?;
    let mut mod_path = mod_path.to_vec();
    mod_path.push(mod_name.clone());
    let mut children = btreemap! {};

    debug!(
        "Initializing child: {:?} as `{}`",
        my_path,
        itertools::join(&mod_path, ":")
    );

    let mut file_path = None;

    if my_path.is_dir() {
        let my_path = &my_path;

        for child in std::fs::read_dir(my_path).map_err(|e| map_io_error(my_path, e))? {
            let child = child.map_err(|e| map_io_error(&my_path, e))?;
            let child_path = child.path();

            if is_valid_mod(&child_path) {
                let mod_name = expect_mod_name(child_path.file_stem())?;

                if mod_name == "mod" {
                    if !child.path().is_file() {
                        todo!("Error!")
                    }

                    file_path = Some(child_path);
                } else {
                    children.insert(mod_name, initialize_child(ctx, &mod_path, child_path)?);
                }
            }
        }
    } else if my_path.is_file() {
        file_path = Some(my_path);
    } else {
        unreachable!()
    }

    let file_id = AFile {
        // It doesn't hurt to keep the dir path
        path: file_path,
        mod_path,
        children,
    }
    .intern(ctx);

    Ok(file_id)
}

fn is_valid_mod(p: &Path) -> bool {
    debug!("Checking if {:?} is a valid module", p);

    if let Some(file_stem) = p.file_stem() {
        if let Some(module_name) = file_stem.to_str() {
            if MODULE_NAME_REGEX.is_match(module_name) {
                if let Some(extension) = p.extension() {
                    return p.is_file() && is_valid_extension(extension);
                } else {
                    return p.is_dir();
                }
            }
        }
    }

    false
}

fn is_valid_extension(extension: &OsStr) -> bool {
    extension == "ch"
}

fn expect_mod_name(name: Option<&OsStr>) -> AResult<String> {
    if let Some(module_name) = name.and_then(OsStr::to_str) {
        if MODULE_NAME_REGEX.is_match(module_name) {
            return Ok(module_name.to_owned());
        }
    }

    todo!("Error handling")
}

fn map_path_error(child_path: String, e: std::io::Error) -> AError {
    AError::IOErrorInArgumentPath {
        child_path,
        io_error: format!("{}", e),
    }
}

fn map_io_error(my_path: &Path, e: std::io::Error) -> AError {
    AError::IOErrorInPathChild {
        path: my_path.to_path_buf(),
        io_error: format!("{}", e),
    }
}

pub fn mod_parent(ctx: &dyn AdelaideContext, key: Id<AFile>) -> Id<AFile> {
    let path = &key.lookup(ctx).mod_path;
    let mut m = ctx.mod_tree_root();

    if !path.is_empty() {
        for p in &path[0..path.len() - 1] {
            m = m.lookup(ctx).children[p];
        }
    }

    m
}
