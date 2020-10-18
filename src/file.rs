use crate::{
    ctx::AdelaideContext,
    util::{AError, AResult, Id, Intern, Lookup},
};
use lazy_static::lazy_static;
use regex::Regex;
use std::{
    collections::BTreeMap,
    convert::{TryFrom, TryInto},
    ffi::OsStr,
    path::{Path, PathBuf},
    sync::Arc,
};

lazy_static! {
    static ref MODULE_NAME_REGEX: Regex = Regex::new(r"^[a-z][a-zA-Z0-9_]*$").unwrap();
}

#[derive(Debug, Hash, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct ModuleName(String);
impl TryFrom<String> for ModuleName {
    type Error = AError;

    fn try_from(value: String) -> AResult<ModuleName> {
        if MODULE_NAME_REGEX.is_match(&value) {
            Ok(ModuleName(value))
        } else {
            Err(AError::InvalidModuleError(value))
        }
    }
}

impl Into<String> for ModuleName {
    fn into(self) -> String {
        self.0
    }
}

impl std::fmt::Display for ModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct AFile {
    pub path: Option<PathBuf>,
    pub mod_path: Vec<ModuleName>,
    pub children: BTreeMap<ModuleName, Id<AFile>>,
}

pub fn initialize_from_path_arguments(
    ctx: &mut dyn AdelaideContext,
    input: Vec<String>,
) -> AResult<()> {
    let mut children: BTreeMap<ModuleName, Id<AFile>> = btreemap! {};

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
    mod_path: &[ModuleName],
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

    if my_path.is_dir() {
        let my_path = &my_path;

        for child in std::fs::read_dir(my_path).map_err(|e| map_io_error(my_path, e))? {
            let child_path = child.map_err(|e| map_io_error(&my_path, e))?.path();

            if is_valid_mod(&child_path) {
                let mod_name = expect_mod_name(child_path.file_stem())?;
                children.insert(mod_name, initialize_child(ctx, &mod_path, child_path)?);
            }
        }
    }

    let file_id = AFile {
        // It doesn't hurt to keep the dir path
        path: if my_path.is_file() {
            Some(my_path)
        } else {
            None
        },
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

fn expect_mod_name(name: Option<&OsStr>) -> AResult<ModuleName> {
    if let Some(module_name) = name.and_then(OsStr::to_str) {
        if MODULE_NAME_REGEX.is_match(module_name) {
            return module_name.to_owned().try_into();
        }
    }

    todo!("Error handling")
}

fn map_path_error(child_name: String, e: std::io::Error) -> AError {
    AError::IOErrorInArgumentPath(child_name, format!("{}", e))
}

fn map_io_error(my_path: &Path, e: std::io::Error) -> AError {
    AError::IOErrorInPathChild(my_path.to_path_buf(), format!("{}", e))
}

impl Lookup for AFile {
    fn lookup(id: Id<AFile>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_file(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_file(self)
    }
}
