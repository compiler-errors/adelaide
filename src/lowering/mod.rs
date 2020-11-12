mod expr;
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
    parser::{
        PEnum, PExpression, PFunction, PGlobal, PImpl, PImplMember, PItem, PModule, PObject,
        PObjectMembers, PTrait, PTraitMember,
    },
    util::{AError, AResult, Id, Intern, LId, LateLookup},
};

pub use expr::{LExpression, LExpressionData, LStatement, LStatementData};
pub use pattern::{LPattern, LPatternData};
pub use ty::{LTraitType, LType};
pub use uses::{
    early_lookup_ctx, local_mod_items, lookup_item, lookup_item_early, lower_mod_base, mod_items,
    LEarlyContext, LUseError, LUseItem, LUseResult,
};

static IDS: AtomicUsize = AtomicUsize::new(0);

fn fresh_id() -> usize {
    IDS.fetch_add(1, Ordering::Relaxed)
}

fn fresh_name(kind: &'static str) -> String {
    format!("${}{}", kind, fresh_id())
}

fn fresh_infer_ty() -> LType {
    LType::Infer(fresh_id())
}

pub fn lower_root(ctx: &dyn AdelaideContext) -> AResult<Id<LModule>> {
    ctx.lower_mod(ctx.parse_root()?)
}

pub fn lower_mod(ctx: &dyn AdelaideContext, key: Id<PModule>) -> AResult<Id<LModule>> {
    let info = key.lookup(ctx);

    let mut lcx = LoweringContext::try_new(ctx, key)?;

    let mut modules = btreemap! {};
    let mut globals = btreemap! {};
    let mut functions = btreemap! {};
    let mut objects = btreemap! {};
    let mut enums = btreemap! {};
    let mut traits = btreemap! {};
    let mut impls = btreemap! {};

    for (_, &i) in &*ctx.local_mod_items(key)? {
        match i {
            LScopeItem::Global(g) => {
                globals.insert(g, lcx.lower_global(g)?);
            },
            LScopeItem::Module(m) => {
                modules.insert(m, lower_mod(ctx, m)?);
            },
            LScopeItem::Function(f) => {
                functions.insert(f, lcx.lower_function(f)?);
            },
            LScopeItem::Object(o) => {
                objects.insert(o, lcx.lower_object(o)?);
            },
            LScopeItem::Enum(e) => {
                enums.insert(e, lcx.lower_enum(e)?);
            },
            LScopeItem::Trait(t) => {
                traits.insert(t, lcx.lower_trait(t)?);
            },
            LScopeItem::EnumVariant(..) | LScopeItem::Variable(_) | LScopeItem::Generic(_) =>
                unreachable!(),
        }
    }

    for i in &info.items {
        match i {
            PItem::Impl(i) => {
                impls.insert(*i, lcx.lower_impl(*i)?);
            },
            _ => {},
        }
    }

    Ok(LModule {
        source: key,
        parent: info.parent.get(ctx).into(),
        modules,
        globals,
        functions,
        objects,
        enums,
        traits,
        impls,
    }
    .intern(ctx))
}

struct LoweringContext<'ctx> {
    ctx: &'ctx dyn AdelaideContext,
    base_items: Arc<HashMap<Id<str>, LScopeItem>>,
    scopes: Vec<FunctionScope>,
}

impl<'ctx> LoweringContext<'ctx> {
    fn try_new(
        ctx: &'ctx dyn AdelaideContext,
        parent: Id<PModule>,
    ) -> AResult<LoweringContext<'ctx>> {
        let base_items = ctx.mod_items(parent)?;
        Ok(LoweringContext {
            ctx,
            base_items,
            scopes: vec![],
        })
    }

    fn enter_context(&mut self, return_allowed: bool, await_allowed: bool) {
        self.scopes
            .push(FunctionScope::new(return_allowed, await_allowed));
        self.enter_block();
    }

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

    fn lookup_std_item(&self, name: &'static str) -> LScopeItem {
        // We expect none of these results to be an error, so just unwrap them.
        if let Some(i) = self
            .ctx
            .mod_items(self.ctx.parse_std().unwrap())
            .unwrap()
            .get(&self.ctx.static_name(name))
        {
            *i
        } else {
            panic!("Expected to find std item: {}", name)
        }
    }

    fn lookup_path(&mut self, path: &[(Span, Id<str>)]) -> AResult<LScopeItem> {
        match self.lookup_path_partial(path)? {
            (_, i, &[]) => Ok(i),
            (_, i, &[(s, _), ..]) => {
                let (kind, name, def_span) = i.info(self.ctx);
                Err(AError::ItemIsNotAModule {
                    kind,
                    name,
                    def_span,
                    use_span: s,
                })
            },
        }
    }

    fn lookup_path_partial<'p>(
        &mut self,
        path: &'p [(Span, Id<str>)],
    ) -> AResult<(Span, LScopeItem, &'p [(Span, Id<str>)])> {
        assert!(!path.is_empty());
        let ((mut span, name), path) = path.split_first().unwrap();

        let mut item = self.lookup_scoped_item(span, *name)?;

        for (i, (next_span, name)) in path.iter().enumerate() {
            span = span.unite(*next_span);

            match item {
                LScopeItem::Module(m) => {
                    item = self.ctx.lookup_item(span, m, *name)?;
                },
                LScopeItem::Enum(e) => {
                    let info = e.lookup(self.ctx);

                    if info.variants.iter().any(|(_, n, _)| n == name) {
                        return Ok((span, LScopeItem::EnumVariant(e, *name), &path[i + 1..]));
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
                    return Ok((span, item, &path[i + 1..]));
                },
            }
        }

        Ok((span, item, &[]))
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

    fn lower_global(&mut self, g: Id<PGlobal>) -> AResult<Id<LGlobal>> {
        let PGlobal {
            span,
            name,
            ty,
            expr,
            ..
        } = &*g.lookup(self.ctx);

        self.enter_context(false, false);

        let ty = self.lower_ty(*ty, false)?;
        let expr = self.lower_expr(*expr)?;

        let vcx = self.exit_context();

        Ok(LGlobal {
            source: g,
            span: *span,
            name: *name,
            ty,
            expr,
            vcx,
        }
        .intern(self.ctx))
    }

    fn lower_function(&mut self, f: Id<PFunction>) -> AResult<Id<LFunction>> {
        let PFunction {
            parent: _,
            span,
            name,
            generics,
            parameters,
            return_ty,
            restrictions,
            body,
        } = &*f.lookup(self.ctx);

        self.enter_context(true, false);

        let mut gs = vec![];
        for (s, g) in generics {
            gs.push(self.declare_generic(*g, *s)?);
        }

        let mut ps = vec![];
        for (s, n, t) in parameters {
            let t = self.lower_ty(*t, false)?;
            ps.push(self.declare_variable(*n, *s, t));
        }

        let return_ty = self.lower_ty(*return_ty, false)?;

        let mut rs = vec![];
        for (t, trs) in restrictions {
            let t = self.lower_ty(*t, false)?;

            for tr in trs {
                rs.push((t, self.lower_trait_ty(*tr, false)?));
            }
        }

        let body = if let Some(body) = body {
            Some(self.lower_expr(*body)?)
        } else {
            None
        };

        let vcx = self.exit_context();

        Ok(LFunction {
            source: f,
            span: *span,
            name: *name,
            generics: gs,
            parameters: ps,
            return_ty,
            restrictions: rs,
            body,
            vcx,
        }
        .intern(self.ctx))
    }

    fn lower_object(&mut self, o: Id<PObject>) -> AResult<Id<LObject>> {
        let PObject {
            parent: _,
            is_structural,
            span,
            name,
            generics,
            restrictions,
            members,
        } = &*o.lookup(self.ctx);

        self.enter_context(false, false);

        let mut gs = vec![];
        for (s, g) in generics {
            gs.push(self.declare_generic(*g, *s)?);
        }

        let mut rs = vec![];
        for (t, trs) in restrictions {
            let t = self.lower_ty(*t, false)?;

            for tr in trs {
                rs.push((t, self.lower_trait_ty(*tr, false)?));
            }
        }

        let members = self.lower_object_members(members)?;

        self.exit_context();

        Ok(LObject {
            source: o,
            span: *span,
            is_structural: *is_structural,
            name: *name,
            generics: gs,
            restrictions: rs,
            members,
        }
        .intern(self.ctx))
    }

    fn lower_object_members(&mut self, c: &PObjectMembers) -> AResult<LMembers> {
        match c {
            PObjectMembers::Empty(s) => Ok(LMembers::Empty(*s)),
            PObjectMembers::Positional(s, es) =>
                Ok(LMembers::Positional(*s, self.lower_tys(es, false)?)),
            PObjectMembers::Named(s, es) => {
                let mut members = vec![];
                let mut mapping = btreemap! {};

                for (i, (s, n, t)) in es.iter().enumerate() {
                    members.push(self.lower_ty(*t, false)?);

                    if let Some((_, old_s)) = mapping.insert(*n, (i, *s)) {
                        return Err(AError::DuplicatedDefinition {
                            kind: "object member",
                            name: *n,
                            span: *s,
                            span2: old_s,
                        });
                    }
                }

                Ok(LMembers::Named(*s, members, mapping))
            },
        }
    }

    fn lower_enum(&mut self, e: Id<PEnum>) -> AResult<Id<LEnum>> {
        let PEnum {
            parent: _,
            span,
            name,
            generics,
            restrictions,
            variants,
        } = &*e.lookup(self.ctx);

        self.enter_context(false, false);

        let mut gs = vec![];
        for (s, g) in generics {
            gs.push(self.declare_generic(*g, *s)?);
        }

        let mut rs = vec![];
        for (t, trs) in restrictions {
            let t = self.lower_ty(*t, false)?;

            for tr in trs {
                rs.push((t, self.lower_trait_ty(*tr, false)?));
            }
        }

        let mut seen = hashmap! {};
        let mut vs = btreemap! {};

        for (s, n, v) in variants {
            if let Some(old_s) = seen.insert(*n, *s) {
                return Err(AError::DuplicatedDefinition {
                    kind: "enum variant",
                    name: *n,
                    span: *s,
                    span2: old_s,
                });
            }

            vs.insert(*n, self.lower_object_members(v)?);
        }

        self.exit_context();

        Ok(LEnum {
            source: e,
            span: *span,
            name: *name,
            generics: gs,
            restrictions: rs,
            variants: vs,
        }
        .intern(self.ctx))
    }

    fn lower_trait(&mut self, t: Id<PTrait>) -> AResult<Id<LTrait>> {
        let PTrait {
            parent: _,
            span,
            name,
            generics,
            restrictions,
            members,
        } = &*t.lookup(self.ctx);

        self.enter_context(false, false);

        let mut gs = vec![];
        for (s, g) in generics {
            gs.push(self.declare_generic(*g, *s)?);
        }

        let mut rs = vec![];
        for (t, trs) in restrictions {
            let t = self.lower_ty(*t, false)?;

            for tr in trs {
                rs.push((t, self.lower_trait_ty(*tr, false)?));
            }
        }

        let mut seen = hashmap! {};
        let mut types = btreemap! {};
        let mut methods = btreemap! {};

        for m in members {
            match m {
                PTraitMember::Type(s, n, ts) => {
                    if let Some(old_s) = seen.insert(*n, *s) {
                        return Err(AError::DuplicatedDefinition {
                            kind: "associated type",
                            name: *n,
                            span: *s,
                            span2: old_s,
                        });
                    }

                    types.insert(*n, self.lower_trait_tys(ts, false)?);
                },
                PTraitMember::Function {
                    span,
                    name,
                    generics,
                    has_self,
                    parameters,
                    return_ty,
                    restrictions,
                } => {
                    if let Some(old_s) = seen.insert(*name, *span) {
                        return Err(AError::DuplicatedDefinition {
                            kind: "method",
                            name: *name,
                            span: *span,
                            span2: old_s,
                        });
                    }

                    self.enter_context(false, false);

                    let mut gs = vec![];
                    for (s, g) in generics {
                        gs.push(self.declare_generic(*g, *s)?);
                    }

                    let mut ps = vec![];

                    if let Some(span) = has_self {
                        ps.push(self.declare_variable(
                            self.ctx.static_name("self"),
                            *span,
                            LType::SelfType.intern(self.ctx),
                        ));
                    }

                    for (s, n, t) in parameters {
                        let t = self.lower_ty(*t, false)?;
                        ps.push(self.declare_variable(*n, *s, t));
                    }

                    let return_ty = self.lower_ty(*return_ty, false)?;

                    let mut rs = vec![];
                    for (t, trs) in restrictions {
                        let t = self.lower_ty(*t, false)?;

                        for tr in trs {
                            rs.push((t, self.lower_trait_ty(*tr, false)?));
                        }
                    }

                    self.exit_context();

                    methods.insert(*name, LTraitMethod {
                        parent: t.into(),
                        span: *span,
                        name: *name,
                        generics: gs,
                        parameters: ps,
                        restrictions: rs,
                        return_ty,
                    });
                },
            }
        }

        self.exit_context();

        Ok(LTrait {
            source: t,
            span: *span,
            name: *name,
            generics: gs,
            restrictions: rs,
            types,
            methods,
        }
        .intern(self.ctx))
    }

    fn lower_impl(&mut self, i: Id<PImpl>) -> AResult<Id<LImpl>> {
        let PImpl {
            parent: _,
            span,
            generics,
            ty,
            trait_ty,
            restrictions,
            members,
        } = &*i.lookup(self.ctx);

        self.enter_context(false, false);

        let mut gs = vec![];
        for (s, g) in generics {
            gs.push(self.declare_generic(*g, *s)?);
        }

        let ty = self.lower_ty(*ty, false)?;
        let trait_ty = if let Some(trait_ty) = trait_ty {
            Some(self.lower_trait_ty(*trait_ty, false)?)
        } else {
            None
        };

        let mut rs = vec![];
        for (t, trs) in restrictions {
            let t = self.lower_ty(*t, false)?;

            for tr in trs {
                rs.push((t, self.lower_trait_ty(*tr, false)?));
            }
        }

        let mut seen = hashmap! {};
        let mut types = btreemap! {};
        let mut methods = btreemap! {};

        for m in members {
            match m {
                PImplMember::Type(s, n, t) => {
                    if let Some(old_s) = seen.insert(*n, *s) {
                        return Err(AError::DuplicatedDefinition {
                            kind: "associated type",
                            name: *n,
                            span: *s,
                            span2: old_s,
                        });
                    }

                    types.insert(*n, self.lower_ty(*t, false)?);
                },
                PImplMember::Function {
                    span,
                    name,
                    generics,
                    has_self,
                    parameters,
                    return_ty,
                    restrictions,
                    body,
                } => {
                    if let Some(old_s) = seen.insert(*name, *span) {
                        return Err(AError::DuplicatedDefinition {
                            kind: "method",
                            name: *name,
                            span: *span,
                            span2: old_s,
                        });
                    }

                    self.enter_context(true, false);

                    let mut gs = vec![];
                    for (s, g) in generics {
                        gs.push(self.declare_generic(*g, *s)?);
                    }

                    let mut ps = vec![];

                    if let Some(span) = has_self {
                        ps.push(self.declare_variable(
                            self.ctx.static_name("self"),
                            *span,
                            LType::SelfType.intern(self.ctx),
                        ));
                    }

                    for (s, n, t) in parameters {
                        let t = self.lower_ty(*t, false)?;
                        ps.push(self.declare_variable(*n, *s, t));
                    }

                    let return_ty = self.lower_ty(*return_ty, false)?;

                    let mut rs = vec![];
                    for (t, trs) in restrictions {
                        let t = self.lower_ty(*t, false)?;

                        for tr in trs {
                            rs.push((t, self.lower_trait_ty(*tr, false)?));
                        }
                    }

                    let body = self.lower_expr(*body)?;

                    let vcx = self.exit_context();

                    methods.insert(*name, LImplMethod {
                        parent: i.into(),
                        span: *span,
                        name: *name,
                        generics: gs,
                        parameters: ps,
                        restrictions: rs,
                        return_ty,
                        body,
                        vcx,
                    });
                },
            }
        }

        self.exit_context();

        if let Some(trait_ty) = trait_ty {
            fn compare<T>(
                parent_name: Id<str>,
                item_kind: &'static str,
                expected: &HashMap<Id<str>, Span>,
                given: &BTreeMap<Id<str>, T>,
                seen: &HashMap<Id<str>, Span>,
            ) -> AResult<()> {
                for (n, s) in expected {
                    if !given.contains_key(n) {
                        return Err(AError::ExpectedSubItem {
                            parent_kind: "impl for trait",
                            parent_name,
                            item_kind,
                            item_name: *n,
                            span: *s,
                        });
                    }
                }

                for (n, _) in given {
                    if !expected.contains_key(n) {
                        let s = seen[n];
                        return Err(AError::UnexpectedSubItem {
                            parent_kind: "impl for trait",
                            parent_name,
                            item_kind,
                            item_name: *n,
                            span: s,
                        });
                    }
                }

                Ok(())
            }

            let parent = trait_ty.lookup(self.ctx).tr.source();
            let parent_name = parent.lookup(self.ctx).name;
            let LTraitShape {
                types: expected_types,
                methods: expected_methods,
            } = &*self.ctx.trait_shape(parent)?;

            compare(parent_name, "type", expected_types, &types, &seen)?;
            compare(parent_name, "method", expected_methods, &methods, &seen)?;
        }

        Ok(LImpl {
            source: i,
            span: *span,
            trait_ty,
            ty,
            generics: gs,
            restrictions: rs,
            types,
            methods,
        }
        .intern(self.ctx))
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

    fn declare_label(&mut self, label: Option<Id<str>>) -> usize {
        let label = label.unwrap_or_else(|| fresh_name("LABEL").intern(self.ctx));
        let id = fresh_id();

        self.scopes
            .last_mut()
            .unwrap()
            .label_scopes
            .push((label, id));
        id
    }

    fn lookup_label(&mut self, s: Span, l: Option<Id<str>>) -> AResult<usize> {
        for (n, i) in self.scopes.last_mut().unwrap().label_scopes.iter().rev() {
            if l.map_or(true, |l| l == *n) {
                return Ok(*i);
            }
        }

        if let Some(l) = l {
            Err(AError::MissingLabel { span: s, name: l })
        } else {
            Err(AError::NotInLoop { span: s })
        }
    }

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
            Ok((0..expected).map(|_| self.fresh_infer_ty()).collect())
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

    fn get_range_bound(
        &self,
        span: Span,
        source: Id<PExpression>,
        kind: &'static str,
        es: Vec<Id<LExpression>>,
    ) -> Id<LExpression> {
        if let LScopeItem::Enum(bound) = self.lookup_std_item("Bound") {
            LExpression {
                source,
                span,
                data: LExpressionData::EnumConstructor(
                    bound.into(),
                    self.fresh_infer_tys(1),
                    self.ctx.static_name(kind),
                    es.into_iter().enumerate().collect(),
                ),
            }
            .intern(self.ctx)
        } else {
            unreachable!()
        }
    }

    fn fresh_infer_ty(&self) -> Id<LType> {
        fresh_infer_ty().intern(self.ctx)
    }

    fn fresh_infer_tys(&self, n: usize) -> Vec<Id<LType>> {
        (0..n).map(|_| fresh_infer_ty().intern(self.ctx)).collect()
    }
}

struct FunctionScope {
    return_allowed: bool,
    await_allowed: bool,
    variable_scopes: Vec<HashMap<Id<str>, LVariable>>,
    label_scopes: Vec<(Id<str>, usize)>,
    generics: HashMap<Id<str>, LGeneric>,
    vcx: LVariableContext,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LVariableContext {
    variables: BTreeMap<usize, LVariable>,
    captures: BTreeMap<usize, LVariable>,
}

impl FunctionScope {
    fn new(return_allowed: bool, await_allowed: bool) -> Self {
        FunctionScope {
            return_allowed,
            await_allowed,
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

    fn capture(&mut self, item: LVariable) -> LVariable {
        let fresh = fresh_id();

        self.vcx.captures.insert(fresh, item);

        let item = LVariable {
            id: fresh,
            name: item.name,
            ty: item.ty,
            span: item.span,
        };

        self.vcx.variables.insert(fresh, item);

        item
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LVariable {
    id: usize,
    name: Id<str>,
    ty: Id<LType>,
    span: Span,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LGeneric {
    id: usize,
    name: Id<str>,
    span: Span,
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LModule {
    #[plain]
    source: Id<PModule>,
    parent: LId<LModule>,
    modules: BTreeMap<Id<PModule>, Id<LModule>>,
    globals: BTreeMap<Id<PGlobal>, Id<LGlobal>>,
    functions: BTreeMap<Id<PFunction>, Id<LFunction>>,
    objects: BTreeMap<Id<PObject>, Id<LObject>>,
    enums: BTreeMap<Id<PEnum>, Id<LEnum>>,
    traits: BTreeMap<Id<PTrait>, Id<LTrait>>,
    impls: BTreeMap<Id<PImpl>, Id<LImpl>>,
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
    source: Id<PGlobal>,
    span: Span,
    name: Id<str>,
    ty: Id<LType>,
    expr: Id<LExpression>,
    vcx: LVariableContext,
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
    source: Id<PFunction>,
    span: Span,
    name: Id<str>,
    generics: Vec<LGeneric>,
    parameters: Vec<LVariable>,
    return_ty: Id<LType>,
    restrictions: Vec<(Id<LType>, Id<LTraitType>)>,
    body: Option<Id<LExpression>>,
    vcx: LVariableContext,
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
    source: Id<PObject>,
    span: Span,
    name: Id<str>,
    is_structural: bool,
    generics: Vec<LGeneric>,
    restrictions: Vec<(Id<LType>, Id<LTraitType>)>,
    members: LMembers,
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
    source: Id<PEnum>,
    span: Span,
    name: Id<str>,
    generics: Vec<LGeneric>,
    restrictions: Vec<(Id<LType>, Id<LTraitType>)>,
    variants: BTreeMap<Id<str>, LMembers>,
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
    source: Id<PTrait>,
    span: Span,
    name: Id<str>,
    generics: Vec<LGeneric>,
    restrictions: Vec<(Id<LType>, Id<LTraitType>)>,
    types: BTreeMap<Id<str>, Vec<Id<LTraitType>>>,
    methods: BTreeMap<Id<str>, LTraitMethod>,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LTraitMethod {
    parent: LId<LTrait>,
    span: Span,
    name: Id<str>,
    generics: Vec<LGeneric>,
    parameters: Vec<LVariable>,
    restrictions: Vec<(Id<LType>, Id<LTraitType>)>,
    return_ty: Id<LType>,
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
    source: Id<PImpl>,
    span: Span,
    ty: Id<LType>,
    trait_ty: Option<Id<LTraitType>>,
    generics: Vec<LGeneric>,
    restrictions: Vec<(Id<LType>, Id<LTraitType>)>,
    types: BTreeMap<Id<str>, Id<LType>>,
    methods: BTreeMap<Id<str>, LImplMethod>,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct LImplMethod {
    parent: LId<LImpl>,
    span: Span,
    name: Id<str>,
    generics: Vec<LGeneric>,
    parameters: Vec<LVariable>,
    restrictions: Vec<(Id<LType>, Id<LTraitType>)>,
    return_ty: Id<LType>,
    body: Id<LExpression>,
    vcx: LVariableContext,
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
    let mut bounds = hashmap! {};

    for i in &tr.lookup(ctx).members {
        match i {
            PTraitMember::Type(span, name, _) => {
                if let Some(old_span) = bounds.get(name) {
                    return Err(AError::DuplicatedDefinition {
                        kind: "trait bound",
                        name: *name,
                        span: *span,
                        span2: *old_span,
                    });
                }

                bounds.insert(*name, *span);
            },
            _ => { /* Ignore */ },
        }
    }

    Ok(Arc::new(bounds))
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

#[derive(Debug, Eq, PartialEq)]
pub struct LTraitShape {
    types: HashMap<Id<str>, Span>,
    methods: HashMap<Id<str>, Span>,
}

pub fn trait_shape(ctx: &dyn AdelaideContext, key: Id<PTrait>) -> AResult<Arc<LTraitShape>> {
    let mut seen = hashmap! {};
    let mut types = hashmap! {};
    let mut methods = hashmap! {};

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
            PTraitMember::Function { span, name, .. } => {
                if let Some(old_s) = seen.insert(*name, *span) {
                    return Err(AError::DuplicatedDefinition {
                        kind: "method",
                        name: *name,
                        span: *span,
                        span2: old_s,
                    });
                }

                methods.insert(*name, *span);
            },
        }
    }

    Ok(Arc::new(LTraitShape { types, methods }))
}
