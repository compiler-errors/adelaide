use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    sync::{Arc, Mutex},
};

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    parser::{PEnum, PFunction, PGlobal, PItem, PModPath, PModule, PObject, PTrait},
    util::{AError, AResult, Id, Intern, Opaque, Pretty},
};

use super::LScopeItem;

pub type LUseResult = Result<LUseItem, LUseError>;

#[derive(Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LUseItem {
    Module(Id<PModule>),
    Global(Id<PGlobal>),
    Function(Id<PFunction>),
    Object(Id<PObject>),
    Enum(Id<PEnum>),
    EnumVariant(Id<PEnum>, Id<str>),
    Trait(Id<PTrait>),
    Imported {
        span: Span,
        base: Option<Id<PModule>>,
        path: VecDeque<(Span, Id<str>)>,
        in_module: Id<PModule>,
        name: Id<str>,
    },
}

impl LUseItem {
    pub fn info(&self, ctx: &dyn AdelaideContext) -> (&'static str, Id<str>, Span) {
        match self {
            LUseItem::Module(e) => {
                let e = e.lookup(ctx);
                ("module", e.name, e.span)
            },
            LUseItem::Global(e) => {
                let e = e.lookup(ctx);
                ("global variable", e.name, e.span)
            },
            LUseItem::Function(e) => {
                let e = e.lookup(ctx);
                ("function", e.name, e.span)
            },
            LUseItem::Object(e) => {
                let e = e.lookup(ctx);
                (
                    if e.is_structural { "struct" } else { "object" },
                    e.name,
                    e.span,
                )
            },
            LUseItem::Enum(e) => {
                let e = e.lookup(ctx);
                ("enum", e.name, e.span)
            },
            LUseItem::EnumVariant(e, v) => {
                let e = e.lookup(ctx);

                for (span, name, _) in &e.variants {
                    if name == v {
                        let full_name =
                            format!("{}::{}", e.name.lookup(ctx), v.lookup(ctx)).intern(ctx);
                        return ("enum variant", full_name, *span);
                    }
                }

                unreachable!("This enum variant should be guaranteed to exist")
            },
            LUseItem::Trait(e) => {
                let e = e.lookup(ctx);
                ("trait", e.name, e.span)
            },
            LUseItem::Imported {
                span,
                base: _,
                path: _,
                name,
                in_module: _,
            } => ("imported item", *name, *span),
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum LUseError {
    MissingItem(PModPath, Id<str>, Span),
    Cycle(Vec<Span>, Id<str>),
    Error(AError),
    NotAModule(&'static str, Id<str>, Span, Span),
    MissingSubItem {
        parent_kind: &'static str,
        parent_name: Id<str>,
        parent_span: Span,
        child_kind: &'static str,
        child_name: Id<str>,
        use_span: Span,
    },
}

impl From<AError> for LUseError {
    fn from(e: AError) -> Self {
        LUseError::Error(e)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LEarlyContext {
    named_items: BTreeMap<Id<str>, LUseItem>,
    full_imports: Vec<(Option<Id<PModule>>, VecDeque<(Span, Id<str>)>, Span)>,
}

pub fn lower_mod_base(ctx: &dyn AdelaideContext, key: Id<PModule>) -> AResult<Arc<LEarlyContext>> {
    let module = key.lookup(ctx);

    let mut named_items = btreemap! {};
    let mut full_imports = vec![];

    for &i in &module.items {
        match i {
            PItem::Use(u) => {
                let u = u.lookup(ctx);
                let absolute = u.absolute;

                for e in &u.elements {
                    match e {
                        crate::parser::PUseElement::UseAll(p, s) => {
                            full_imports.push((
                                if absolute { None } else { Some(key) },
                                p.clone(),
                                *s,
                            ));
                        },
                        crate::parser::PUseElement::UseSelf(p, _, n)
                        | crate::parser::PUseElement::Use(p, n) => {
                            let n = n.unwrap_or_else(|| *p.back().unwrap());
                            insert_base_item(&mut named_items, ctx, LUseItem::Imported {
                                span: n.0,
                                base: if absolute { None } else { Some(key) },
                                path: p.clone(),
                                in_module: key,
                                name: n.1,
                            })?;
                        },
                    }
                }
            },
            PItem::Module(m) => insert_base_item(&mut named_items, ctx, LUseItem::Module(m))?,
            PItem::Global(g) => insert_base_item(&mut named_items, ctx, LUseItem::Global(g))?,
            PItem::Function(f) => insert_base_item(&mut named_items, ctx, LUseItem::Function(f))?,
            PItem::Object(o) => insert_base_item(&mut named_items, ctx, LUseItem::Object(o))?,
            PItem::Enum(e) => insert_base_item(&mut named_items, ctx, LUseItem::Enum(e))?,
            PItem::Trait(t) => insert_base_item(&mut named_items, ctx, LUseItem::Trait(t))?,
            PItem::Impl(_) => { /* Do nothing, not named. */ },
        }
    }

    Ok(Arc::new(LEarlyContext {
        named_items,
        full_imports,
    }))
}

fn insert_base_item(
    map: &mut BTreeMap<Id<str>, LUseItem>,
    ctx: &dyn AdelaideContext,
    i: LUseItem,
) -> AResult<()> {
    let (what, name, span) = i.info(ctx);

    if let Some(old) = map.insert(name, i) {
        let (what2, _, span2) = old.info(ctx);

        Err(AError::DuplicatedItem {
            what,
            span,
            what2,
            span2,
            name,
        })
    } else {
        Ok(())
    }
}

/// --- Early --- ///

pub fn early_lookup_ctx(
    _: &dyn AdelaideContext,
) -> Opaque<Mutex<HashMap<(Id<PModule>, Id<str>), LUseResult>>> {
    Arc::new(Mutex::new(hashmap! {})).into()
}

pub fn lookup_item_early(
    ctx: &dyn AdelaideContext,
    module: Id<PModule>,
    path: VecDeque<(Span, Id<str>)>,
) -> AResult<LUseItem> {
    let seen = ctx.early_lookup_ctx();
    let mut seen = seen.lock().unwrap();

    Ok(lookup_item_early_deep(
        ctx,
        LUseItem::Module(module),
        path,
        &mut seen,
    )?)
}

fn lookup_item_early_deep(
    ctx: &dyn AdelaideContext,
    mut current_item: LUseItem,
    mut path: VecDeque<(Span, Id<str>)>,
    seen: &mut HashMap<(Id<PModule>, Id<str>), LUseResult>,
) -> LUseResult {
    let parse_root = ctx.parse_root()?;
    assert!(!path.is_empty(), "Cannot lookup an empty path!");

    while !path.is_empty() || matches!(current_item, LUseItem::Imported { .. }) {
        match current_item {
            LUseItem::Imported {
                span,
                base,
                path,
                in_module,
                name,
            } => {
                seen.insert((in_module, name), Err(LUseError::Cycle(vec![span], name)));

                current_item = lookup_item_early_deep_relative(ctx, base, path, seen).map_err(
                    // Add a cycle edge for error printing purposes
                    |e| match e {
                        LUseError::Cycle(mut s, n) => {
                            s.push(span);
                            LUseError::Cycle(s, n)
                        },
                        e => e,
                    },
                )?;

                seen.insert((in_module, name), Ok(current_item.clone()));
            },
            LUseItem::Module(m) => {
                let info = ctx.lower_mod_base(m)?;
                let (span, name) = path.pop_front().unwrap();

                match seen.get(&(m, name)) {
                    Some(Err(LUseError::Cycle(s, n))) => {
                        let mut s = s.clone();
                        s.push(span);
                        return Err(LUseError::Cycle(s, *n));
                    },
                    Some(i) => {
                        current_item = i.clone()?;
                    },
                    None => {
                        /* We haven't seen this item before */
                        if let Some(e) = info.named_items.get(&name) {
                            current_item = e.clone();
                        } else {
                            seen.insert((m, name), Err(LUseError::Cycle(vec![span], name)));

                            let look_for_item = info
                                .full_imports
                                .iter()
                                .flat_map(|(relative, path, _)| {
                                    // Given an import like
                                    //   use a::b::*.
                                    // and we're looking for
                                    //   $CURRENT_ITEM::c::d.
                                    // We can look for
                                    //   a::b::c
                                    // And if we find it, we're good.
                                    let mut path = path.clone();
                                    path.push_back((span, name));

                                    match lookup_item_early_deep_relative(
                                        ctx, *relative, path, seen,
                                    ) {
                                        Ok(i) => Some(i),
                                        Err(_) => None,
                                    }
                                })
                                .next();

                            if let Some(i) = look_for_item {
                                current_item = i;
                            } else {
                                current_item = lookup_item_early_deep(
                                    ctx,
                                    LUseItem::Module(parse_root),
                                    vec![(span, name)].into(),
                                    seen,
                                )
                                .map_err(|_| LUseError::MissingItem(PModPath(m), name, span))?;
                            }

                            seen.insert((m, name), Ok(current_item.clone()));
                        }
                    },
                }
            },
            LUseItem::Enum(e) => {
                let info = e.lookup(ctx);
                let (span, name) = path.pop_front().unwrap();

                if info.variants.iter().any(|(_, n, _)| n == &name) {
                    current_item = LUseItem::EnumVariant(e, name);
                } else {
                    return Err(LUseError::MissingSubItem {
                        parent_kind: "enum",
                        parent_name: info.name,
                        parent_span: info.span,
                        child_kind: "variant",
                        child_name: name,
                        use_span: span,
                    });
                }
            },
            _ => {
                let (use_span, _) = path.pop_front().unwrap();
                let (kind, name, def_span) = current_item.info(ctx);
                return Err(LUseError::NotAModule(kind, name, def_span, use_span));
            },
        }
    }

    Ok(current_item)
}

pub fn lookup_item_early_deep_relative(
    ctx: &dyn AdelaideContext,
    module: Option<Id<PModule>>,
    path: VecDeque<(Span, Id<str>)>,
    seen: &mut HashMap<(Id<PModule>, Id<str>), LUseResult>,
) -> LUseResult {
    debug!(
        "Looking up path {:?} in module {:?}",
        Pretty(&module, ctx),
        Pretty(&path, ctx)
    );

    let original_error = if let Some(module) = module {
        match lookup_item_early_deep(ctx, LUseItem::Module(module), path.clone(), seen) {
            Ok(i) => return Ok(i),
            Err(e) => Some(e),
        }
    } else {
        None
    };

    match lookup_item_early_deep(ctx, LUseItem::Module(ctx.parse_root()?), path, seen) {
        Ok(i) => Ok(i),
        Err(e) => Err(original_error.unwrap_or(e)),
    }
}

/// --- Late --- ///

pub fn local_mod_items(
    ctx: &dyn AdelaideContext,
    module: Id<PModule>,
) -> AResult<Arc<HashMap<Id<str>, LScopeItem>>> {
    let mut items = hashmap! {};
    let mut blame_spans = hashmap! {};

    let info = ctx.lower_mod_base(module)?;

    for (name, i) in &info.named_items {
        match i {
            LUseItem::Imported { .. } => {},
            item => {
                let (_, _, span) = i.info(ctx);
                insert_late_item(
                    &mut items,
                    &mut blame_spans,
                    ctx,
                    *name,
                    span,
                    item.clone().into(),
                    false,
                )?;
            },
        }
    }

    Ok(Arc::new(items))
}

pub fn mod_items(
    ctx: &dyn AdelaideContext,
    module: Id<PModule>,
) -> AResult<Arc<HashMap<Id<str>, LScopeItem>>> {
    let mut items = hashmap! {};
    let mut blame_spans = hashmap! {};
    let mut visited = hashset! {};

    mod_items_deep(
        &mut items,
        &mut blame_spans,
        &mut visited,
        ctx,
        LUseItem::Module(module),
        false,
    )?;

    mod_items_deep(
        &mut items,
        &mut blame_spans,
        &mut visited,
        ctx,
        LUseItem::Module(ctx.parse_root()?),
        true,
    )?;

    mod_items_deep(
        &mut items,
        &mut blame_spans,
        &mut visited,
        ctx,
        LUseItem::Module(ctx.parse_std()?),
        true,
    )?;

    Ok(Arc::new(items))
}

fn mod_items_deep(
    items: &mut HashMap<Id<str>, LScopeItem>,
    blame_spans: &mut HashMap<Id<str>, Span>,
    visited: &mut HashSet<Id<PModule>>,
    ctx: &dyn AdelaideContext,
    item: LUseItem,
    allow_shadow: bool,
) -> AResult<()> {
    let parse_root = ctx.parse_root()?;

    match item {
        LUseItem::Module(module) => {
            if !visited.insert(module) {
                return Ok(());
            }

            let info = ctx.lower_mod_base(module)?;

            for (name, i) in &info.named_items {
                match i {
                    LUseItem::Imported {
                        span,
                        base,
                        path,
                        name,
                        in_module: _,
                    } => {
                        let base = base.unwrap_or(parse_root);
                        let item = ctx.lookup_item_early(base, path.clone())?;
                        insert_late_item(
                            items,
                            blame_spans,
                            ctx,
                            *name,
                            *span,
                            item.into(),
                            allow_shadow,
                        )?;
                    },
                    item => {
                        let (_, _, span) = i.info(ctx);
                        insert_late_item(
                            items,
                            blame_spans,
                            ctx,
                            *name,
                            span,
                            item.clone().into(),
                            allow_shadow,
                        )?;
                    },
                }

                for (relative, path, _) in &info.full_imports {
                    let base = relative.unwrap_or(parse_root);
                    let item = ctx.lookup_item_early(base, path.clone())?;
                    mod_items_deep(items, blame_spans, visited, ctx, item, allow_shadow)?;
                }
            }
        },
        i => {
            let (kind, name, def_span) = i.info(ctx);
            return Err(AError::ItemIsNotAModuleNoUsage {
                kind,
                name,
                def_span,
            });
        },
    }

    Ok(())
}

fn insert_late_item(
    items: &mut HashMap<Id<str>, LScopeItem>,
    blame_spans: &mut HashMap<Id<str>, Span>,
    ctx: &dyn AdelaideContext,
    name: Id<str>,
    span: Span,
    item: LScopeItem,
    allow_shadow: bool,
) -> AResult<()> {
    if let Some(old) = blame_spans.insert(name, span) {
        let (what, _, _) = item.info(ctx);
        let (what2, _, span2) = items.get(&name).unwrap().info(ctx);

        if allow_shadow {
            // If redefinition is ok, then put that old span back...
            blame_spans.insert(name, old);
            return Ok(());
        } else {
            return Err(AError::DuplicatedItem {
                what,
                span,
                what2,
                span2,
                name,
            });
        }
    }

    items.insert(name, item);

    Ok(())
}

pub fn lookup_item(
    ctx: &dyn AdelaideContext,
    span: Span,
    module: Id<PModule>,
    name: Id<str>,
) -> AResult<LScopeItem> {
    let items = ctx.mod_items(module)?;

    items
        .get(&name)
        .copied()
        .ok_or_else(|| LUseError::MissingItem(PModPath(module), name, span).into())
}
