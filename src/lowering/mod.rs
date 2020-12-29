mod expr;
mod item;
mod pattern;
mod ty;
mod uses;

use std::{
    collections::{BTreeMap, HashMap},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use crate::{
    ctx::AdelaideContext,
    lexer::Span,
    parser::{PEnum, PFunction, PGlobal, PImpl, PItem, PModule, PObject, PTrait, PTraitMember},
    util::{AError, AResult, Id, Intern, LId, LateLookup, TryCollectVec},
};

pub use expr::{LExpression, LExpressionData, LLiteral, LStatement, LStatementData};
pub use pattern::{LPattern, LPatternData};
pub use ty::{LTraitType, LTraitTypeWithBindings, LType, LTypeData};
pub use uses::{
    early_lookup_ctx, local_mod_items, lookup_item, lookup_item_early, lower_mod_base, mod_items,
    LEarlyContext, LUseError, LUseItem, LUseResult,
};

pub fn lower_program(ctx: &dyn AdelaideContext) -> AResult<Id<LModule>> {
    ctx.lower_mod(ctx.parse_program()?)
}

pub fn lower_mods(ctx: &dyn AdelaideContext) -> AResult<Arc<[Id<LModule>]>> {
    fn get(ctx: &dyn AdelaideContext, key: Id<PModule>, out: &mut Vec<Id<PModule>>) {
        out.push(key);

        for i in &key.lookup(ctx).items {
            match i {
                PItem::Module(m) => {
                    get(ctx, *m, out);
                },
                _ => {},
            }
        }
    }

    let mut out = vec![];
    get(ctx, ctx.parse_program()?, &mut out);

    Ok(out
        .into_iter()
        .map(|m| ctx.lower_mod(m))
        .try_collect_vec()?
        .into())
}

pub fn lower_mod(ctx: &dyn AdelaideContext, key: Id<PModule>) -> AResult<Id<LModule>> {
    LoweringContext::try_new(ctx, key)?.lower_mod()
}

struct LoweringContext<'ctx> {
    module: Id<PModule>,
    ctx: &'ctx dyn AdelaideContext,
    base_items: Arc<HashMap<Id<str>, LScopeItem>>,
    scopes: Vec<FunctionScope>,
    self_ty: Option<Id<LType>>,
}

impl<'ctx> LoweringContext<'ctx> {
    fn try_new(
        ctx: &'ctx dyn AdelaideContext,
        module: Id<PModule>,
    ) -> AResult<LoweringContext<'ctx>> {
        let base_items = ctx.mod_items(module)?;

        Ok(LoweringContext {
            module,
            ctx,
            base_items,
            scopes: vec![],
            self_ty: None,
        })
    }

    /// Look up a path "partially"; that is, following modules into submodules
    /// until the item in question is no longer path-accessible. Returns the
    /// remaining path items
    fn lookup_path<'p>(&mut self, path: &'p [(Span, Id<str>)]) -> AResult<LScopeItem> {
        assert!(!path.is_empty());
        let ((mut span, name), path) = path.split_first().unwrap();

        let mut item = self.lookup_scoped_item(span, *name)?;

        for (next_span, name) in path.iter() {
            span = span.unite(*next_span);

            match item {
                LScopeItem::Module(m) => {
                    item = self.ctx.lookup_item(span, m, *name)?;
                },
                LScopeItem::Enum(e) => {
                    let info = e.lookup(self.ctx);

                    if info.variants.iter().any(|(_, n, _)| n == name) {
                        item = LScopeItem::EnumVariant(e, *name);
                    } else {
                        return Err(AError::MissingSubItem {
                            parent_kind: "enum",
                            parent_name: info.name,
                            parent_span: info.span,
                            child_kind: "variant",
                            child_name: *name,
                            use_span: *next_span,
                        });
                    }
                },
                item => {
                    let (kind, name, def_span) = item.info(self.ctx);
                    return Err(AError::ItemIsNotAModule {
                        kind,
                        name,
                        def_span,
                        use_span: *next_span,
                    });
                },
            }
        }

        Ok(item)
    }

    fn lookup_scoped_item(&mut self, at: Span, name: Id<str>) -> AResult<LScopeItem> {
        if let Some(item) = Self::lookup_function_scoped_variable(name, &mut self.scopes) {
            Ok(LScopeItem::Variable(item))
        } else if let Some(item) = self.lookup_function_scoped_generic(name) {
            Ok(LScopeItem::Generic(item))
        } else if let Some(item) = self.base_items.get(&name) {
            Ok(*item)
        } else {
            Err(AError::MissingItem { span: at, name })
        }
    }

    fn lookup_function_scoped_variable(
        name: Id<str>,
        scopes: &mut [FunctionScope],
    ) -> Option<LVariable> {
        if scopes.is_empty() {
            return None;
        }

        let (scope, scopes) = scopes.split_last_mut().unwrap();

        if let Some(item) = scope.get(name) {
            Some(item)
        } else if let Some(captured_item) = Self::lookup_function_scoped_variable(name, scopes) {
            Some(scope.capture(captured_item))
        } else {
            None
        }
    }

    fn lookup_function_scoped_generic(&self, name: Id<str>) -> Option<LGeneric> {
        for s in self.scopes.iter().rev() {
            if let Some(g) = s.generics.get(&name) {
                return Some(*g);
            }
        }

        None
    }

    fn declare_variable(&mut self, name: Id<str>, span: Span, ty: Id<LType>) -> LVariable {
        let var = LVariable {
            id: fresh_id(),
            name,
            span,
            ty,
        };

        self.scopes
            .last_mut()
            .unwrap()
            .vcx
            .variables
            .insert(var.id, var);

        self.scopes
            .last_mut()
            .unwrap()
            .variable_scopes
            .last_mut()
            .unwrap()
            .insert(name, var);

        var
    }

    fn declare_generic(&mut self, name: Id<str>, span: Span) -> AResult<LGeneric> {
        let scope = self.scopes.last_mut().unwrap();
        let generic = LGeneric {
            id: fresh_id(),
            name,
            span,
        };

        debug!("Inserting generic {:?} = {}", name, name.lookup(self.ctx));
        if let Some(LGeneric { span: old_span, .. }) = scope.generics.insert(name, generic) {
            return Err(AError::DuplicatedDefinition {
                kind: "generic",
                name,
                span,
                span2: old_span,
            });
        }

        Ok(generic)
    }

    /// Enter a top-level item scope. If `return_allowed` is false, then we will
    /// fail if we try to lower the "return" operator. If `await_allowed` is
    /// unset, then we will fail if we try to lower the `:await` operator.
    fn enter_context(&mut self, kind: ScopeKind) {
        self.scopes.push(FunctionScope::new(kind));
        self.enter_block();
    }

    /// Exit a top-level item scope, returning the variable context that was
    /// processed in this scope.
    fn exit_context(&mut self) -> LVariableContext {
        self.exit_block();
        self.scopes.pop().unwrap().vcx
    }

    fn enter_block(&mut self) {
        self.scopes
            .last_mut()
            .unwrap()
            .variable_scopes
            .push(hashmap! {})
    }

    fn exit_block(&mut self) {
        self.scopes
            .last_mut()
            .unwrap()
            .variable_scopes
            .pop()
            .unwrap();
    }

    /// Declare a (possibly unnamed label) and give it a unique loop id
    fn enter_label(&mut self, label: Option<Id<str>>) -> LoopId {
        let label = label.unwrap_or_else(|| fresh_name("LABEL").intern(self.ctx));
        let id = fresh_id();

        self.scopes
            .last_mut()
            .unwrap()
            .label_scopes
            .push((label, id, false));
        id
    }

    /// Pop a label from the scope, meaning it cannot be referenced anymore
    fn exit_label(&mut self) -> bool {
        self.scopes
            .last_mut()
            .unwrap()
            .label_scopes
            .pop()
            .unwrap()
            .2
    }

    /// Look up a label, returning its unique loop id. If the label is `None`,
    /// then this function will return the last label, so (e.g.) a "break"
    /// breaks the nearest enclosing loop.
    fn lookup_label(&mut self, s: Span, l: Option<Id<str>>) -> AResult<LoopId> {
        for (n, i, used) in self
            .scopes
            .last_mut()
            .unwrap()
            .label_scopes
            .iter_mut()
            .rev()
        {
            if l.map_or(true, |l| l == *n) {
                *used = true;
                return Ok(*i);
            }
        }

        if let Some(l) = l {
            Err(AError::MissingLabel { span: s, name: l })
        } else {
            Err(AError::NotInLoop { span: s })
        }
    }

    /// Checks that the arity of the `given` generics is the same as the arity
    /// of the `expected`. If infers are allowed and zero generics are provided,
    /// then `expected` number of infers are returned instead.
    fn check_generics_parity(
        &self,
        given: Vec<Id<LType>>,
        given_span: Span,
        expected: usize,
        expected_span: Span,
        infer_allowed: bool,
    ) -> AResult<Vec<Id<LType>>> {
        if given.len() == expected {
            Ok(given)
        } else if given.is_empty() && infer_allowed {
            Ok((0..expected)
                .map(|_| self.fresh_infer_ty(given_span))
                .collect())
        } else {
            Err(AError::ParityDisparity {
                kind: "generic types",
                expected,
                expected_span,
                given: given.len(),
                given_span,
            })
        }
    }

    fn fresh_infer_ty(&self, span: Span) -> Id<LType> {
        LType {
            data: LTypeData::Infer(fresh_id()),
            span,
        }
        .intern(self.ctx)
    }

    fn fresh_infer_tys(&self, n: usize, span: Span) -> Vec<Id<LType>> {
        (0..n).map(|_| self.fresh_infer_ty(span)).collect()
    }
}

struct FunctionScope {
    kind: ScopeKind,
    variable_scopes: Vec<HashMap<Id<str>, LVariable>>,
    label_scopes: Vec<(Id<str>, LoopId, bool)>,
    generics: HashMap<Id<str>, LGeneric>,
    vcx: LVariableContext,
}
#[derive(Copy, Clone)]
enum ScopeKind {
    /// `return`/`:await`/`` all disallowed
    None,
    /// `return` allowed
    Returnable,
    /// `return` and `:await` allowed
    Async,
    /// `return` and `yield` allowed
    Generator,
    // `return`, `:await`, and `yield` are allowed
    AsyncGenerator,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LVariableContext {
    pub variables: BTreeMap<VariableId, LVariable>,
    pub captures: BTreeMap<VariableId, LVariable>,
}

impl FunctionScope {
    fn new(kind: ScopeKind) -> Self {
        FunctionScope {
            kind,
            variable_scopes: vec![],
            label_scopes: vec![],
            generics: hashmap! {},
            vcx: LVariableContext {
                variables: btreemap! {},
                captures: btreemap! {},
            },
        }
    }

    fn get(&self, name: Id<str>) -> Option<LVariable> {
        for s in self.variable_scopes.iter().rev() {
            if let Some(item) = s.get(&name) {
                return Some(*item);
            }
        }

        None
    }

    fn capture(&mut self, old_item: LVariable) -> LVariable {
        let fresh = fresh_id();

        self.vcx.captures.insert(fresh, old_item);

        let item = LVariable {
            id: fresh,
            name: old_item.name,
            ty: old_item.ty,
            span: old_item.span,
        };

        self.vcx.variables.insert(fresh, item);

        item
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LVariable {
    pub id: VariableId,
    pub name: Id<str>,
    pub ty: Id<LType>,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LGeneric {
    pub id: GenericId,
    pub name: Id<str>,
    pub span: Span,
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LModule {
    #[plain]
    pub source: Id<PModule>,
    pub parent: LId<LModule>,
    pub modules: BTreeMap<Id<PModule>, Id<LModule>>,
    pub globals: BTreeMap<Id<PGlobal>, Id<LGlobal>>,
    pub functions: BTreeMap<Id<PFunction>, Id<LFunction>>,
    pub objects: BTreeMap<Id<PObject>, Id<LObject>>,
    pub enums: BTreeMap<Id<PEnum>, Id<LEnum>>,
    pub traits: BTreeMap<Id<PTrait>, Id<LTrait>>,
    pub impls: BTreeMap<Id<PImpl>, Id<LImpl>>,
}

impl LateLookup for LModule {
    type Source = PModule;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .modules
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LGlobal {
    #[plain]
    pub source: Id<PGlobal>,
    pub span: Span,
    pub name: Id<str>,
    pub ty: Id<LType>,
    pub expr: Id<LExpression>,
    pub vcx: LVariableContext,
}

impl LateLookup for LGlobal {
    type Source = PGlobal;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .globals
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LFunction {
    #[plain]
    pub source: Id<PFunction>,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<LGeneric>,
    pub parameters: Vec<LVariable>,
    pub return_ty: Id<LType>,
    pub restrictions: Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>,
    pub body: Option<Id<LExpression>>,
    pub vcx: LVariableContext,
}

impl LateLookup for LFunction {
    type Source = PFunction;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .functions
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LObject {
    #[plain]
    pub source: Id<PObject>,
    pub span: Span,
    pub name: Id<str>,
    pub is_structural: bool,
    pub generics: Vec<LGeneric>,
    pub restrictions: Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>,
    pub members: LMembers,
}

impl LateLookup for LObject {
    type Source = PObject;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .objects
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LEnum {
    #[plain]
    pub source: Id<PEnum>,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<LGeneric>,
    pub restrictions: Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>,
    pub variants: BTreeMap<Id<str>, LMembers>,
}

impl LateLookup for LEnum {
    type Source = PEnum;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .enums
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LTrait {
    #[plain]
    pub source: Id<PTrait>,
    pub parent: LId<LModule>,
    pub span: Span,
    pub name: Id<str>,
    pub self_skolem: LGeneric,
    pub generics: Vec<LGeneric>,
    pub restrictions: Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>,
    pub types: BTreeMap<Id<str>, Vec<Id<LTraitTypeWithBindings>>>,
    pub methods: BTreeMap<Id<str>, LTraitMethod>,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LTraitMethod {
    pub parent: LId<LTrait>,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<LGeneric>,
    pub has_self: bool,
    pub parameters: Vec<LVariable>,
    pub restrictions: Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>,
    pub return_ty: Id<LType>,
}

impl LateLookup for LTrait {
    type Source = PTrait;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .traits
            .get(&id)
            .unwrap()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LImpl {
    #[plain]
    pub source: Id<PImpl>,
    pub span: Span,
    pub ty: Id<LType>,
    pub trait_ty: Option<Id<LTraitType>>,
    pub generics: Vec<LGeneric>,
    pub restrictions: Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>,
    pub types: BTreeMap<Id<str>, Id<LType>>,
    pub methods: BTreeMap<Id<str>, LImplMethod>,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LImplMethod {
    pub parent: LId<LImpl>,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<LGeneric>,
    pub has_self: bool,
    pub parameters: Vec<LVariable>,
    pub restrictions: Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>,
    pub return_ty: Id<LType>,
    pub body: Id<LExpression>,
    pub vcx: LVariableContext,
}

impl LateLookup for LImpl {
    type Source = PImpl;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        let source = id.lookup(ctx);
        *ctx.lower_mod(source.parent.get(ctx))
            .unwrap()
            .lookup(ctx)
            .impls
            .get(&id)
            .unwrap()
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LScopeItem {
    Module(Id<PModule>),
    Global(Id<PGlobal>),
    Function(Id<PFunction>),
    Object(Id<PObject>),
    Enum(Id<PEnum>),
    EnumVariant(Id<PEnum>, Id<str>),
    Trait(Id<PTrait>),
    Variable(LVariable),
    Generic(LGeneric),
}

impl LScopeItem {
    pub fn info(&self, ctx: &dyn AdelaideContext) -> (&'static str, Id<str>, Span) {
        match self {
            LScopeItem::Module(e) => {
                let e = e.lookup(ctx);
                ("module", e.name, e.span)
            },
            LScopeItem::Global(e) => {
                let e = e.lookup(ctx);
                ("global variable", e.name, e.span)
            },
            LScopeItem::Function(e) => {
                let e = e.lookup(ctx);
                ("function", e.name, e.span)
            },
            LScopeItem::Object(e) => {
                let e = e.lookup(ctx);
                (
                    if e.is_structural { "struct" } else { "object" },
                    e.name,
                    e.span,
                )
            },
            LScopeItem::Enum(e) => {
                let e = e.lookup(ctx);
                ("enum", e.name, e.span)
            },
            LScopeItem::EnumVariant(e, v) => {
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
            LScopeItem::Trait(e) => {
                let e = e.lookup(ctx);
                ("trait", e.name, e.span)
            },
            LScopeItem::Variable(LVariable { name, span, .. }) => ("variable", *name, *span),
            LScopeItem::Generic(LGeneric { name, span, .. }) => ("generic", *name, *span),
        }
    }
}

impl From<LUseItem> for LScopeItem {
    fn from(i: LUseItem) -> Self {
        match i {
            LUseItem::Module(m) => LScopeItem::Module(m),
            LUseItem::Global(g) => LScopeItem::Global(g),
            LUseItem::Function(f) => LScopeItem::Function(f),
            LUseItem::Object(o) => LScopeItem::Object(o),
            LUseItem::Enum(e) => LScopeItem::Enum(e),
            LUseItem::EnumVariant(e, v) => LScopeItem::EnumVariant(e, v),
            LUseItem::Trait(t) => LScopeItem::Trait(t),
            LUseItem::Imported { .. } => unreachable!(),
        }
    }
}

pub fn get_bound_names(
    ctx: &dyn AdelaideContext,
    tr: Id<PTrait>,
) -> AResult<Arc<HashMap<Id<str>, Span>>> {
    let mut bindings = hashmap! {};

    for i in &tr.lookup(ctx).members {
        match i {
            PTraitMember::Type(span, name, _) => {
                if let Some(old_span) = bindings.get(name) {
                    return Err(AError::DuplicatedDefinition {
                        kind: "trait bound",
                        name: *name,
                        span: *span,
                        span2: *old_span,
                    });
                }

                bindings.insert(*name, *span);
            },
            _ => { /* Ignore */ },
        }
    }

    Ok(Arc::new(bindings))
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LMembers {
    Empty(Span),
    Positional(Span, Vec<Id<LType>>),
    Named(Span, Vec<Id<LType>>, BTreeMap<Id<str>, (usize, Span)>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum LConstructorShape {
    Empty(Span),
    Positional(Span, usize),
    Named(Span, HashMap<Id<str>, (usize, Span)>),
}

impl LConstructorShape {
    fn info(&self) -> (&'static str, Span) {
        match self {
            LConstructorShape::Empty(s) => ("no", *s),
            LConstructorShape::Positional(s, _) => ("positional", *s),
            LConstructorShape::Named(s, _) => ("named", *s),
        }
    }
}

pub fn object_constructor(
    ctx: &dyn AdelaideContext,
    o: Id<PObject>,
) -> AResult<Arc<LConstructorShape>> {
    let data = match &o.lookup(ctx).members {
        crate::parser::PObjectMembers::Empty(s) => LConstructorShape::Empty(*s),
        crate::parser::PObjectMembers::Positional(s, n) =>
            LConstructorShape::Positional(*s, n.len()),
        crate::parser::PObjectMembers::Named(s, es) => {
            let mut members = hashmap! {};

            for (i, (s, n, _)) in es.iter().enumerate() {
                if let Some((_, old_s)) = members.insert(*n, (i, *s)) {
                    return Err(AError::DuplicatedDefinition {
                        kind: "object member",
                        name: *n,
                        span: *s,
                        span2: old_s,
                    });
                }
            }

            LConstructorShape::Named(*s, members)
        },
    };
    Ok(Arc::new(data))
}

pub fn enum_variant_constructor(
    ctx: &dyn AdelaideContext,
    e: Id<PEnum>,
    v: Id<str>,
) -> AResult<Arc<LConstructorShape>> {
    let info = e.lookup(ctx);

    for (_, variant, members) in &info.variants {
        if *variant == v {
            let data = match members {
                crate::parser::PObjectMembers::Empty(s) => LConstructorShape::Empty(*s),
                crate::parser::PObjectMembers::Positional(s, n) =>
                    LConstructorShape::Positional(*s, n.len()),
                crate::parser::PObjectMembers::Named(s, es) => {
                    let mut members = hashmap! {};

                    for (i, (s, n, _)) in es.iter().enumerate() {
                        if let Some((_, old_s)) = members.insert(*n, (i, *s)) {
                            return Err(AError::DuplicatedDefinition {
                                kind: "enum variant field",
                                name: *n,
                                span: *s,
                                span2: old_s,
                            });
                        }
                    }

                    LConstructorShape::Named(*s, members)
                },
            };

            return Ok(Arc::new(data));
        }
    }

    Err(AError::MissingSubItemNoUsage {
        parent_kind: "enum",
        parent_name: info.name,
        parent_span: info.span,
        child_kind: "variant",
        child_name: v,
    })
}

pub fn enum_variant_span(ctx: &dyn AdelaideContext, e: Id<PEnum>, v: Id<str>) -> AResult<Span> {
    let info = e.lookup(ctx);

    for (span, variant, _) in &info.variants {
        if *variant == v {
            return Ok(*span);
        }
    }

    Err(AError::MissingSubItemNoUsage {
        parent_kind: "enum",
        parent_name: info.name,
        parent_span: info.span,
        child_kind: "variant",
        child_name: v,
    })
}

#[derive(Debug, Eq, PartialEq)]
pub struct LTraitShape {
    types: HashMap<Id<str>, Span>,
    methods: HashMap<Id<str>, Span>,
    method_generics_and_parameters: HashMap<Id<str>, (usize, usize)>,
}

pub fn trait_shape(ctx: &dyn AdelaideContext, key: Id<PTrait>) -> AResult<Arc<LTraitShape>> {
    let mut seen = hashmap! {};
    let mut types = hashmap! {};
    let mut methods = hashmap! {};
    let mut method_generics = hashmap! {};

    for m in &key.lookup(ctx).members {
        match m {
            PTraitMember::Type(s, n, _) => {
                if let Some(old_s) = seen.insert(*n, *s) {
                    return Err(AError::DuplicatedDefinition {
                        kind: "associated type",
                        name: *n,
                        span: *s,
                        span2: old_s,
                    });
                }

                types.insert(*n, *s);
            },
            PTraitMember::Function {
                span,
                name,
                generics,
                parameters,
                has_self,
                ..
            } => {
                if let Some(old_s) = seen.insert(*name, *span) {
                    return Err(AError::DuplicatedDefinition {
                        kind: "method",
                        name: *name,
                        span: *span,
                        span2: old_s,
                    });
                }

                methods.insert(*name, *span);
                method_generics.insert(
                    *name,
                    (
                        generics.len(),
                        parameters.len() + if has_self.is_some() { 1 } else { 0 },
                    ),
                );
            },
        }
    }

    Ok(Arc::new(LTraitShape {
        types,
        methods,
        method_generics_and_parameters: method_generics,
    }))
}

pub fn std_item(ctx: &dyn AdelaideContext, name: &'static str) -> LScopeItem {
    // We expect none of these results to be an error, so just unwrap them.
    if let Some(i) = ctx
        .mod_items(ctx.parse_std().unwrap())
        .unwrap()
        .get(&ctx.static_name(name))
    {
        *i
    } else {
        panic!("Expected to find std item: {}", name)
    }
}

pub fn lower_pollstate_item(ctx: &dyn AdelaideContext) -> AResult<Id<LEnum>> {
    if let LScopeItem::Enum(e) = ctx
        .mod_items(ctx.parse_std()?)?
        .get(&ctx.static_name("PollState"))
        .unwrap()
    {
        let lowered_e: LId<LEnum> = (*e).into();
        Ok(lowered_e.get(ctx))
    } else {
        unreachable!()
    }
}

pub fn lower_generator_item(ctx: &dyn AdelaideContext) -> AResult<Id<LObject>> {
    if let LScopeItem::Object(e) = ctx
        .mod_items(ctx.parse_std()?)?
        .get(&ctx.static_name("Generator"))
        .unwrap()
    {
        let lowered_e: LId<LObject> = (*e).into();
        Ok(lowered_e.get(ctx))
    } else {
        unreachable!()
    }
}

pub fn lower_concrete_item(ctx: &dyn AdelaideContext) -> AResult<Id<LTrait>> {
    if let LScopeItem::Trait(e) = ctx
        .mod_items(ctx.parse_std()?)?
        .get(&ctx.static_name("Concrete"))
        .unwrap()
    {
        let lowered_e: LId<LTrait> = (*e).into();
        Ok(lowered_e.get(ctx))
    } else {
        unreachable!()
    }
}

pub fn lower_into_item(ctx: &dyn AdelaideContext) -> AResult<Id<LTrait>> {
    if let LScopeItem::Trait(e) = ctx
        .mod_items(ctx.parse_std()?)?
        .get(&ctx.static_name("Into"))
        .unwrap()
    {
        let lowered_e: LId<LTrait> = (*e).into();
        Ok(lowered_e.get(ctx))
    } else {
        unreachable!()
    }
}

static IDS: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, PrettyPrint)]
pub struct LoopId(usize);

impl From<usize> for LoopId {
    fn from(u: usize) -> Self {
        Self(u)
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, PrettyPrint)]
pub struct InferId(pub usize);

impl From<usize> for InferId {
    fn from(u: usize) -> Self {
        Self(u)
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, PrettyPrint)]
pub struct VariableId(usize);

impl From<usize> for VariableId {
    fn from(u: usize) -> Self {
        Self(u)
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, PrettyPrint)]
pub struct GenericId(pub usize);

impl From<usize> for GenericId {
    fn from(u: usize) -> Self {
        Self(u)
    }
}

pub fn fresh_id<T: From<usize>>() -> T {
    IDS.fetch_add(1, Ordering::Relaxed).into()
}

/// Returns a fresh identifier name that is unique and irrepresentable in code
/// (and therefore fine to be used in all scopes that can be aliased/shadowed)
fn fresh_name(kind: &'static str) -> String {
    format!("${}{}", kind, fresh_id::<usize>())
}
