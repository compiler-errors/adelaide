use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    sync::{Arc, Mutex},
};

use codespan_reporting::files::Files;

use crate::{
    file::AFile,
    lexer::Span,
    lowering::{
        LConstructorShape, LEarlyContext, LEnum, LExpression, LFunction, LGlobal, LImpl, LModule,
        LObject, LPattern, LScopeItem, LStatement, LTrait, LTraitShape, LTraitType,
        LTraitTypeWithBindings, LType, LUseItem, LUseResult,
    },
    parser::{
        PEnum, PExpression, PFunction, PGlobal, PImpl, PModule, PObject, PPattern, PStatement,
        PTrait, PTraitType, PTraitTypeWithBindings, PType, PUse,
    },
    read::{RawFile, RawFileSource},
    typechecker::{TTraitType, TTraitTypeWithBindings, TType},
    util::{AResult, Id, Intern, Opaque},
};

#[salsa::query_group(AdelaideStorage)]
pub trait AdelaideContext: salsa::Database {
    #[salsa::input]
    fn mod_tree_root(&self) -> Id<AFile>;

    #[salsa::invoke(crate::file::mod_parent)]
    fn mod_parent(&self, key: Id<AFile>) -> Id<AFile>;

    fn line_starts(&self, key: Id<AFile>) -> Arc<[usize]>;

    fn line_start(&self, id: Id<AFile>, idx: usize) -> Option<usize>;

    fn literal_module_children(&self, data: Id<AFile>) -> Arc<[Id<AFile>]>;

    #[salsa::interned]
    fn intern_str(&self, data: Arc<str>) -> Id<str>;

    #[salsa::interned]
    fn intern_afile(&self, data: Arc<AFile>) -> Id<AFile>;

    fn static_name(&self, name: &'static str) -> Id<str>;

    // ------ LEXER ------ //

    #[salsa::invoke(crate::read::read_file)]
    fn read_file(&self, key: Id<AFile>) -> AResult<Arc<RawFile>>;

    #[salsa::invoke(crate::lexer::lex_mod)]
    fn lex_mod(&self, key: Id<AFile>) -> AResult<()>;

    // ------ PARSER ------ //

    #[salsa::invoke(crate::parser::parse_program)]
    fn parse_program(&self) -> AResult<Id<PModule>>;

    #[salsa::invoke(crate::parser::parse_std)]
    fn parse_std(&self) -> AResult<Id<PModule>>;

    #[salsa::invoke(crate::parser::parse_lang)]
    fn parse_lang(&self) -> AResult<Id<PModule>>;

    #[salsa::invoke(crate::parser::parse_mod)]
    fn parse_mod(&self, file_id: Id<AFile>) -> AResult<Id<PModule>>;

    #[salsa::interned]
    fn intern_pmodule(&self, key: Arc<PModule>) -> Id<PModule>;

    #[salsa::interned]
    fn intern_puse(&self, key: Arc<PUse>) -> Id<PUse>;

    #[salsa::interned]
    fn intern_pfunction(&self, key: Arc<PFunction>) -> Id<PFunction>;

    #[salsa::interned]
    fn intern_pobject(&self, key: Arc<PObject>) -> Id<PObject>;

    #[salsa::interned]
    fn intern_ptrait(&self, key: Arc<PTrait>) -> Id<PTrait>;

    #[salsa::interned]
    fn intern_pimpl(&self, key: Arc<PImpl>) -> Id<PImpl>;

    #[salsa::interned]
    fn intern_penum(&self, key: Arc<PEnum>) -> Id<PEnum>;

    #[salsa::interned]
    fn intern_ptype(&self, key: Arc<PType>) -> Id<PType>;

    #[salsa::interned]
    fn intern_ptraittype(&self, key: Arc<PTraitType>) -> Id<PTraitType>;

    #[salsa::interned]
    fn intern_ptraittypewithbindings(
        &self,
        key: Arc<PTraitTypeWithBindings>,
    ) -> Id<PTraitTypeWithBindings>;

    #[salsa::interned]
    fn intern_pglobal(&self, key: Arc<PGlobal>) -> Id<PGlobal>;

    #[salsa::interned]
    fn intern_pexpression(&self, key: Arc<PExpression>) -> Id<PExpression>;

    #[salsa::interned]
    fn intern_pstatement(&self, key: Arc<PStatement>) -> Id<PStatement>;

    #[salsa::interned]
    fn intern_ppattern(&self, key: Arc<PPattern>) -> Id<PPattern>;

    // ------ LOWERING ------ //

    #[salsa::invoke(crate::lowering::std_item)]
    fn std_item(&self, name: &'static str) -> LScopeItem;

    #[salsa::invoke(crate::lowering::lower_program)]
    fn lower_program(&self) -> AResult<Id<LModule>>;

    #[salsa::invoke(crate::lowering::lower_mod)]
    fn lower_mod(&self, key: Id<PModule>) -> AResult<Id<LModule>>;

    #[salsa::invoke(crate::lowering::lower_mods)]
    fn lower_mods(&self) -> AResult<Arc<[Id<LModule>]>>;

    #[salsa::invoke(crate::lowering::lower_mod_base)]
    fn lower_mod_base(&self, key: Id<PModule>) -> AResult<Arc<LEarlyContext>>;

    #[salsa::invoke(crate::lowering::early_lookup_ctx)]
    fn early_lookup_ctx(&self) -> Opaque<Mutex<HashMap<(Id<PModule>, Id<str>), LUseResult>>>;

    #[salsa::invoke(crate::lowering::lookup_item_early)]
    fn lookup_item_early(
        &self,
        module: Id<PModule>,
        path: VecDeque<(Span, Id<str>)>,
    ) -> AResult<LUseItem>;

    #[salsa::invoke(crate::lowering::local_mod_items)]
    fn local_mod_items(&self, module: Id<PModule>) -> AResult<Arc<HashMap<Id<str>, LScopeItem>>>;

    #[salsa::invoke(crate::lowering::mod_items)]
    fn mod_items(&self, module: Id<PModule>) -> AResult<Arc<HashMap<Id<str>, LScopeItem>>>;

    // TODO: Could just reverse this path argument and use a Vec lol
    #[salsa::invoke(crate::lowering::lookup_item)]
    fn lookup_item(&self, span: Span, module: Id<PModule>, name: Id<str>) -> AResult<LScopeItem>;

    #[salsa::interned]
    fn intern_lmodule(&self, key: Arc<LModule>) -> Id<LModule>;

    #[salsa::interned]
    fn intern_lfunction(&self, key: Arc<LFunction>) -> Id<LFunction>;

    #[salsa::interned]
    fn intern_lobject(&self, key: Arc<LObject>) -> Id<LObject>;

    #[salsa::interned]
    fn intern_lenum(&self, key: Arc<LEnum>) -> Id<LEnum>;

    #[salsa::interned]
    fn intern_ltrait(&self, key: Arc<LTrait>) -> Id<LTrait>;

    #[salsa::interned]
    fn intern_limpl(&self, key: Arc<LImpl>) -> Id<LImpl>;

    #[salsa::interned]
    fn intern_ltype(&self, key: Arc<LType>) -> Id<LType>;

    #[salsa::interned]
    fn intern_ltraittype(&self, key: Arc<LTraitType>) -> Id<LTraitType>;

    #[salsa::interned]
    fn intern_ltraittypewithbindings(
        &self,
        key: Arc<LTraitTypeWithBindings>,
    ) -> Id<LTraitTypeWithBindings>;

    #[salsa::interned]
    fn intern_lglobal(&self, key: Arc<LGlobal>) -> Id<LGlobal>;

    #[salsa::interned]
    fn intern_lexpression(&self, key: Arc<LExpression>) -> Id<LExpression>;

    #[salsa::interned]
    fn intern_lstatement(&self, key: Arc<LStatement>) -> Id<LStatement>;

    #[salsa::interned]
    fn intern_lpattern(&self, key: Arc<LPattern>) -> Id<LPattern>;

    #[salsa::invoke(crate::lowering::get_bound_names)]
    fn get_bound_names(&self, tr: Id<PTrait>) -> AResult<Arc<HashMap<Id<str>, Span>>>;

    #[salsa::invoke(crate::lowering::object_constructor)]
    fn object_constructor(&self, e: Id<PObject>) -> AResult<Arc<LConstructorShape>>;

    #[salsa::invoke(crate::lowering::enum_variant_constructor)]
    fn enum_variant_constructor(&self, e: Id<PEnum>, v: Id<str>)
        -> AResult<Arc<LConstructorShape>>;

    #[salsa::invoke(crate::lowering::enum_variant_span)]
    fn enum_variant_span(&self, e: Id<PEnum>, v: Id<str>) -> AResult<Span>;

    #[salsa::invoke(crate::lowering::trait_shape)]
    fn trait_shape(&self, key: Id<PTrait>) -> AResult<Arc<LTraitShape>>;

    // ------ LOWERING CONVENIENCE METHODS ------ //

    #[salsa::invoke(crate::lowering::lower_pollstate_item)]
    fn lower_pollstate_item(&self) -> AResult<Id<LEnum>>;

    #[salsa::invoke(crate::lowering::lower_generator_item)]
    fn lower_generator_item(&self) -> AResult<Id<LObject>>;

    #[salsa::invoke(crate::lowering::lower_concrete_item)]
    fn lower_concrete_item(&self) -> AResult<Id<LTrait>>;

    #[salsa::invoke(crate::lowering::lower_into_item)]
    fn lower_into_item(&self) -> AResult<Id<LTrait>>;

    #[salsa::invoke(crate::lowering::lower_exit_value_item)]
    fn lower_exit_value_item(&self) -> AResult<Id<LTrait>>;

    // ------ TYPECHECKING ------ //

    #[salsa::interned]
    fn intern_ttype(&self, key: Arc<TType>) -> Id<TType>;

    #[salsa::interned]
    fn intern_ttraittype(&self, key: Arc<TTraitType>) -> Id<TTraitType>;

    #[salsa::interned]
    fn intern_ttraittypewithbindings(
        &self,
        key: Arc<TTraitTypeWithBindings>,
    ) -> Id<TTraitTypeWithBindings>;

    fn static_ty(&self, ty: &'static TType) -> Id<TType>;

    #[salsa::invoke(crate::typechecker::get_impls_for_trait)]
    fn get_impls_for_trait(&self, tr: Id<LTrait>) -> AResult<Arc<[Id<LImpl>]>>;

    #[salsa::invoke(crate::typechecker::get_inherent_impls)]
    fn get_inherent_impls(&self) -> AResult<Arc<[Id<LImpl>]>>;

    #[salsa::invoke(crate::typechecker::get_traits_accessible_in_module)]
    fn get_traits_accessible_in_module(&self, m: Id<LModule>) -> AResult<Arc<[Id<LTrait>]>>;

    #[salsa::invoke(crate::typechecker::typecheck_program_result)]
    fn typecheck_program(&self) -> AResult<()>;

    // ------ MONOMORPHIZATION ------ //

    #[salsa::invoke(crate::translate::translate_program)]
    fn translate_program(&self) -> AResult<()>;

    // ...
}

fn static_name(ctx: &dyn AdelaideContext, s: &'static str) -> Id<str> {
    s.intern(ctx)
}

fn static_ty(ctx: &dyn AdelaideContext, ty: &'static TType) -> Id<TType> {
    ty.clone().intern(ctx)
}

// ------ File Stuff ------ //

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
        Ordering::Less => line_starts.get(idx).copied(),
        Ordering::Equal => ctx.source(id).map(|f| f.as_ref().len()),
        Ordering::Greater => None,
    }
}

fn literal_module_children(ctx: &dyn AdelaideContext, file_id: Id<AFile>) -> Arc<[Id<AFile>]> {
    file_id.lookup(ctx).children.values().copied().collect()
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
            Some("ROOOOT".to_string())
        } else {
            let x = id
                .lookup(self)
                .path
                .as_ref()
                .map(|p| p.display().to_string());

            debug!(
                "{:?} = {:?}",
                id.lookup(self).mod_path,
                id.lookup(self).path
            );

            x
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
