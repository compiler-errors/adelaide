use std::sync::Arc;

use crate::{
    ctx::AdelaideContext,
    lowering::{LImpl, LModule, LScopeItem, LTrait},
    util::{AResult, Id, LId},
};

pub fn get_impls_for_trait(ctx: &dyn AdelaideContext, tr: Id<LTrait>) -> AResult<Arc<[Id<LImpl>]>> {
    let mut impls = vec![];

    for m in &*ctx.lower_mods()? {
        for i in m.lookup(ctx).impls.values() {
            if i.lookup(ctx)
                .trait_ty
                .map_or(false, |f| f.lookup(ctx).tr.get(ctx) == tr)
            {
                impls.push(*i);
            }
        }
    }

    Ok(impls.into())
}

pub fn get_inherent_impls(ctx: &dyn AdelaideContext) -> AResult<Arc<[Id<LImpl>]>> {
    let mut impls = vec![];

    for m in &*ctx.lower_mods()? {
        for i in m.lookup(ctx).impls.values() {
            if i.lookup(ctx).trait_ty.is_none() {
                impls.push(*i);
            }
        }
    }

    Ok(impls.into())
}

pub fn get_traits_accessible_in_module(
    ctx: &dyn AdelaideContext,
    m: Id<LModule>,
) -> AResult<Arc<[Id<LTrait>]>> {
    Ok(ctx
        .mod_items(m.lookup(ctx).source)?
        .values()
        .flat_map(|i| match i {
            LScopeItem::Trait(t) => {
                let t: LId<LTrait> = (*t).into();
                Some(t.get(ctx))
            },
            _ => None,
        })
        .collect())
}
