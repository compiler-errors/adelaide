use std::collections::{BTreeMap, HashMap};

use crate::{
    lexer::Span,
    parser::{
        PEnum, PFunction, PGlobal, PImpl, PImplMember, PItem, PModule, PObject, PObjectMembers,
        PTrait, PTraitMember, PTraitTypeWithBindings, PType,
    },
    util::{AError, AResult, Id, Intern, Pretty, TryCollectVec},
};

use super::{
    fresh_id, ty::LTypeData, LEnum, LFunction, LGeneric, LGlobal, LImpl, LImplMethod, LMembers,
    LModule, LObject, LScopeItem, LTrait, LTraitMethod, LTraitShape, LTraitTypeWithBindings, LType,
    LoweringContext, ScopeKind,
};

impl LoweringContext<'_> {
    pub fn lower_mod(&mut self) -> AResult<Id<LModule>> {
        let info = self.module.lookup(self.ctx);

        let mut modules = btreemap! {};
        let mut globals = btreemap! {};
        let mut functions = btreemap! {};
        let mut objects = btreemap! {};
        let mut enums = btreemap! {};
        let mut traits = btreemap! {};
        let mut impls = btreemap! {};

        for (_, &i) in &*self.ctx.local_mod_items(self.module)? {
            match i {
                LScopeItem::Global(g) => {
                    globals.insert(g, self.lower_global(g)?);
                },
                LScopeItem::Module(m) => {
                    modules.insert(m, self.ctx.lower_mod(m)?);
                },
                LScopeItem::Function(f) => {
                    functions.insert(f, self.lower_function(f)?);
                },
                LScopeItem::Object(o) => {
                    objects.insert(o, self.lower_object(o)?);
                },
                LScopeItem::Enum(e) => {
                    enums.insert(e, self.lower_enum(e)?);
                },
                LScopeItem::Trait(t) => {
                    traits.insert(t, self.lower_trait(t)?);
                },
                LScopeItem::EnumVariant(..) | LScopeItem::Variable(_) | LScopeItem::Generic(_) =>
                    unreachable!(),
            }
        }

        for i in &info.items {
            match i {
                PItem::Impl(i) => {
                    impls.insert(*i, self.lower_impl(*i)?);
                },
                _ => {},
            }
        }

        Ok(LModule {
            source: self.module,
            parent: info.parent.get(self.ctx).into(),
            modules,
            globals,
            functions,
            objects,
            enums,
            traits,
            impls,
        }
        .intern(self.ctx))
    }

    fn lower_global(&mut self, g: Id<PGlobal>) -> AResult<Id<LGlobal>> {
        self.self_ty = None;

        let PGlobal {
            span,
            name,
            ty,
            expr,
            ..
        } = &*g.lookup(self.ctx);

        self.enter_context(ScopeKind::None);

        let ty = self.lower_ty(*ty, false, true)?;
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
        self.self_ty = None;

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

        self.enter_context(ScopeKind::Returnable);

        let gs = generics
            .iter()
            .map(|(s, g)| self.declare_generic(*g, *s))
            .try_collect_vec()?;

        let ps = parameters
            .iter()
            .map(|(s, n, t)| -> AResult<_> {
                let t = self.lower_ty(*t, false, true)?;
                Ok(self.declare_variable(*n, *s, t))
            })
            .try_collect_vec()?;

        let return_ty = self.lower_ty(*return_ty, false, true)?;

        let rs = self.lower_restrictions(&restrictions)?;

        let body = body.map(|body| self.lower_expr(body)).transpose()?;

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
        self.self_ty = None;

        let PObject {
            parent: _,
            is_structural,
            span,
            name,
            generics,
            restrictions,
            members,
        } = &*o.lookup(self.ctx);

        self.enter_context(ScopeKind::None);

        let gs = generics
            .iter()
            .map(|(s, g)| self.declare_generic(*g, *s))
            .try_collect_vec()?;

        self.self_ty = Some(
            LType {
                span: *span,
                data: LTypeData::Object(
                    o.into(),
                    gs.iter()
                        .map(|g| {
                            LType {
                                span: g.span,
                                data: LTypeData::Generic(*g),
                            }
                            .intern(self.ctx)
                        })
                        .collect(),
                ),
            }
            .intern(self.ctx),
        );

        let rs = self.lower_restrictions(&restrictions)?;

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
                Ok(LMembers::Positional(*s, self.lower_tys(es, false, true)?)),
            PObjectMembers::Named(s, es) => {
                let mut mapping = btreemap! {};

                let members = es
                    .iter()
                    .enumerate()
                    .map(|(i, (s, n, t))| {
                        if let Some((_, old_s)) = mapping.insert(*n, (i, *s)) {
                            return Err(AError::DuplicatedDefinition {
                                kind: "object member",
                                name: *n,
                                span: *s,
                                span2: old_s,
                            });
                        }

                        self.lower_ty(*t, false, true)
                    })
                    .try_collect_vec()?;

                Ok(LMembers::Named(*s, members, mapping))
            },
        }
    }

    fn lower_enum(&mut self, e: Id<PEnum>) -> AResult<Id<LEnum>> {
        self.self_ty = None;

        let PEnum {
            parent: _,
            span,
            name,
            generics,
            restrictions,
            variants,
        } = &*e.lookup(self.ctx);

        self.enter_context(ScopeKind::None);

        let gs = generics
            .iter()
            .map(|(s, g)| self.declare_generic(*g, *s))
            .try_collect_vec()?;

        self.self_ty = Some(
            LType {
                span: *span,
                data: LTypeData::Enum(
                    e.into(),
                    gs.iter()
                        .map(|g| {
                            LType {
                                span: g.span,
                                data: LTypeData::Generic(*g),
                            }
                            .intern(self.ctx)
                        })
                        .collect(),
                ),
            }
            .intern(self.ctx),
        );

        let rs = self.lower_restrictions(&restrictions)?;

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
        self.self_ty = None;

        let PTrait {
            parent: _,
            span,
            name,
            generics,
            restrictions,
            members,
        } = &*t.lookup(self.ctx);

        self.enter_context(ScopeKind::None);

        let self_skolem = LGeneric {
            id: fresh_id(),
            name: format!("Self ({:?})", Pretty(name, self.ctx)).intern(self.ctx),
            span: *span,
        };
        self.self_ty = Some(
            LType {
                data: LTypeData::SelfSkolem(self_skolem),
                span: *span,
            }
            .intern(self.ctx),
        );

        let gs = generics
            .iter()
            .map(|(s, g)| self.declare_generic(*g, *s))
            .try_collect_vec()?;

        let rs = self.lower_restrictions(&restrictions)?;

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

                    types.insert(*n, self.lower_trait_tys_with_bindings(ts, false, true)?);
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

                    self.enter_context(ScopeKind::Returnable);

                    let gs = generics
                        .iter()
                        .map(|(s, g)| self.declare_generic(*g, *s))
                        .try_collect_vec()?;

                    let mut ps = parameters
                        .iter()
                        .map(|(s, n, t)| -> AResult<_> {
                            let t = self.lower_ty(*t, false, true)?;
                            Ok(self.declare_variable(*n, *s, t))
                        })
                        .try_collect_vec()?;

                    if let Some(span) = has_self {
                        ps.insert(
                            0,
                            self.declare_variable(
                                self.ctx.static_name("self"),
                                *span,
                                self.self_ty.unwrap(),
                            ),
                        );
                    }

                    let return_ty = self.lower_ty(*return_ty, false, true)?;

                    let rs = self.lower_restrictions(&restrictions)?;

                    self.exit_context();

                    methods.insert(*name, LTraitMethod {
                        parent: t.into(),
                        span: *span,
                        name: *name,
                        generics: gs,
                        has_self: has_self.is_some(),
                        parameters: ps,
                        restrictions: rs,
                        return_ty,
                    });
                },
            }
        }

        self.exit_context();

        Ok(LTrait {
            parent: self.module.into(),
            source: t,
            span: *span,
            name: *name,
            self_skolem,
            generics: gs,
            restrictions: rs,
            types,
            methods,
        }
        .intern(self.ctx))
    }

    fn lower_impl(&mut self, i: Id<PImpl>) -> AResult<Id<LImpl>> {
        self.self_ty = None;

        let PImpl {
            parent: _,
            span,
            generics,
            ty,
            trait_ty,
            restrictions,
            members,
        } = &*i.lookup(self.ctx);

        self.enter_context(ScopeKind::None);

        let gs = generics
            .iter()
            .map(|(s, g)| self.declare_generic(*g, *s))
            .try_collect_vec()?;

        self.self_ty = None;

        let ty = self.lower_ty(*ty, false, true)?;

        self.self_ty = Some(ty);

        let trait_ty = if let Some(trait_ty) = trait_ty {
            let trait_ty = self.lower_trait_ty(*trait_ty, false, true)?;

            if LScopeItem::Trait(trait_ty.lookup(self.ctx).tr.source())
                == self.ctx.std_item("Concrete")
            {
                return Err(AError::CannotImplementConcrete { span: *span });
            }

            let trait_mod = trait_ty
                .lookup(self.ctx)
                .tr
                .source()
                .lookup(self.ctx)
                .parent
                .get(self.ctx);
            // We're either in trait's module, or ty's module, or in std::lang
            if self.module != trait_mod
                && Some(self.module) != self.module_for_ty(ty)
                && self.module.lookup(self.ctx).parent.get(self.ctx) != self.ctx.parse_lang()?
            {
                return Err(AError::Orphan { span: *span });
            }

            Some(trait_ty)
        } else {
            None
        };

        let rs = self.lower_restrictions(&restrictions)?;

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

                    types.insert(*n, self.lower_ty(*t, false, true)?);
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

                    self.enter_context(ScopeKind::Returnable);

                    let gs = generics
                        .iter()
                        .map(|(s, g)| self.declare_generic(*g, *s))
                        .try_collect_vec()?;

                    let mut ps = parameters
                        .iter()
                        .map(|(s, n, t)| -> AResult<_> {
                            let t = self.lower_ty(*t, false, true)?;
                            Ok(self.declare_variable(*n, *s, t))
                        })
                        .try_collect_vec()?;

                    if let Some(span) = has_self {
                        ps.insert(
                            0,
                            self.declare_variable(
                                self.ctx.static_name("self"),
                                *span,
                                self.self_ty.unwrap(),
                            ),
                        );
                    }

                    let return_ty = self.lower_ty(*return_ty, false, true)?;

                    let rs = self.lower_restrictions(&restrictions)?;

                    let body = self.lower_expr(*body)?;

                    let vcx = self.exit_context();

                    methods.insert(*name, LImplMethod {
                        parent: i.into(),
                        span: *span,
                        name: *name,
                        generics: gs,
                        has_self: has_self.is_some(),
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
                method_generics_and_parameters: expected_method_generics,
            } = &*self.ctx.trait_shape(parent)?;

            compare(parent_name, "type", expected_types, &types, &seen)?;
            compare(parent_name, "method", expected_methods, &methods, &seen)?;

            for (n, m) in &methods {
                if m.generics.len() != expected_method_generics[n].0 {
                    return Err(AError::ParityDisparity {
                        kind: "method generics",
                        expected: expected_method_generics[n].0,
                        expected_span: expected_methods[n],
                        given: m.generics.len(),
                        given_span: m.span,
                    });
                }

                if m.parameters.len() != expected_method_generics[n].1 {
                    return Err(AError::ParityDisparity {
                        kind: "parameters",
                        expected: expected_method_generics[n].1,
                        expected_span: expected_methods[n],
                        given: m.parameters.len(),
                        given_span: m.span,
                    });
                }
            }
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

    fn lower_restrictions(
        &mut self,
        restrictions: &[(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)],
    ) -> AResult<Vec<(Id<LType>, Id<LTraitTypeWithBindings>)>> {
        let mut rs = vec![];

        for (t, trs) in restrictions {
            let t = self.lower_ty(*t, false, true)?;

            for tr in trs {
                rs.push((t, self.lower_trait_ty_with_bindings(*tr, false, true)?));
            }
        }

        Ok(rs)
    }

    fn module_for_ty(&self, ty: Id<LType>) -> Option<Id<PModule>> {
        match &ty.lookup(self.ctx).data {
            LTypeData::Infer(_) | LTypeData::Associated(_, _, _) | LTypeData::SelfSkolem(_) =>
                unreachable!(),
            LTypeData::Generic(_)
            | LTypeData::Int
            | LTypeData::Float
            | LTypeData::Char
            | LTypeData::Bool
            | LTypeData::String
            | LTypeData::Never
            | LTypeData::Array(_)
            | LTypeData::Tuple(_)
            | LTypeData::Closure(_, _)
            | LTypeData::FnPtr(_, _)
            | LTypeData::Dynamic(_) => None,
            LTypeData::Object(o, _) => Some(o.source().lookup(self.ctx).parent.get(self.ctx)),
            LTypeData::Enum(e, _) => Some(e.source().lookup(self.ctx).parent.get(self.ctx)),
        }
    }
}
