use crate::{
    lexer::Span,
    parser::{PLiteral, PPattern, PPatternConstructorArguments, PPatternData},
    util::{AError, AResult, Id, Intern, LId},
};

use super::{LConstructorShape, LEnum, LObject, LScopeItem, LType, LVariable, LoweringContext};

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct LPattern {
    #[plain]
    pub source: Option<Id<PPattern>>,
    pub span: Span,
    pub ty: Option<Id<LType>>,
    pub data: LPatternData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum LPatternData {
    Underscore(Id<LType>),
    Literal(PLiteral),
    Variable(LVariable),
    Tuple(Vec<Id<LPattern>>),
    EnumVariantPattern(LId<LEnum>, Vec<Id<LType>>, Id<str>, Vec<Id<LPattern>>),
    StructPattern(LId<LObject>, Vec<Id<LType>>, Vec<Id<LPattern>>),
}

impl LoweringContext<'_> {
    pub fn lower_pattern(&mut self, p: Id<PPattern>) -> AResult<Id<LPattern>> {
        let PPattern { span, ty, data } = &*p.lookup(self.ctx);

        let ty = if let Some(ty) = ty {
            Some(self.lower_ty(*ty, true, true)?)
        } else {
            None
        };

        let data = match data {
            PPatternData::Underscore => LPatternData::Underscore(self.fresh_infer_ty(*span)),
            PPatternData::Literal(l) => LPatternData::Literal(*l),
            PPatternData::Identifier(v) => {
                let v = self.declare_variable(
                    *v,
                    *span,
                    ty.unwrap_or_else(|| self.fresh_infer_ty(*span)),
                );
                LPatternData::Variable(v)
            },
            PPatternData::Tuple(ps) => LPatternData::Tuple(self.lower_patterns(ps)?),
            PPatternData::StructuralAmbiguous(p, g, a) => match self.lookup_path(p)? {
                LScopeItem::Object(o) => {
                    let info = o.lookup(self.ctx);
                    if !info.is_structural {
                        return Err(AError::TriedDestructuringObject {
                            parent_name: info.name,
                            parent_span: info.span,
                            use_span: *span,
                        });
                    }

                    let g = self.lower_tys(g, true, true)?;
                    let g =
                        self.check_generics_parity(g, *span, info.generics.len(), info.span, true)?;

                    let s = self.ctx.object_constructor(o)?;
                    let a = self.lower_destructor("object", info.name, &s, a)?;

                    LPatternData::StructPattern(o.into(), g, a)
                },
                LScopeItem::EnumVariant(e, v) => {
                    if !g.is_empty() {
                        return Err(AError::BareEnumGenerics {
                            enum_name: e.lookup(self.ctx).name,
                            variant_name: v,
                            use_span: *span,
                            def_span: self.ctx.enum_variant_span(e, v)?,
                        });
                    }

                    let g = self.fresh_infer_tys(e.lookup(self.ctx).generics.len(), *span);

                    let s = self.ctx.enum_variant_constructor(e, v)?;
                    let a = self.lower_destructor("enum variant", v, &s, a)?;

                    LPatternData::EnumVariantPattern(e.into(), g, v, a)
                },
                i => {
                    let (kind, name, def_span) = i.info(self.ctx);

                    return Err(AError::CannotConstruct {
                        kind,
                        name,
                        def_span,
                        use_span: *span,
                    });
                },
            },
            PPatternData::StructuralVariant(p, g, v, a) => match self.lookup_path(p)? {
                LScopeItem::Enum(e) => {
                    let info = e.lookup(self.ctx);

                    let g = self.lower_tys(g, true, true)?;
                    let g =
                        self.check_generics_parity(g, *span, info.generics.len(), info.span, true)?;

                    let s = self.ctx.enum_variant_constructor(e, *v)?;
                    let a = self.lower_destructor("enum variant", *v, &s, a)?;

                    LPatternData::EnumVariantPattern(e.into(), g, *v, a)
                },
                i => {
                    let (kind, name, _) = i.info(self.ctx);
                    return Err(AError::NotAnEnumVariant {
                        kind,
                        name,
                        variant: *v,
                        use_span: *span,
                    });
                },
            },
        };
        Ok(LPattern {
            source: Some(p),
            span: *span,
            ty,
            data,
        }
        .intern(self.ctx))
    }

    fn lower_destructor(
        &mut self,
        parent_kind: &'static str,
        parent_name: Id<str>,
        shape: &LConstructorShape,
        constructor: &PPatternConstructorArguments,
    ) -> AResult<Vec<Id<LPattern>>> {
        match (shape, constructor) {
            (LConstructorShape::Empty(_), PPatternConstructorArguments::Empty(_)) => Ok(vec![]),
            (
                LConstructorShape::Positional(s, n),
                PPatternConstructorArguments::Positional(s2, ps, ignore),
            ) => {
                if (!ignore && *n < ps.len()) || (*n > ps.len()) {
                    return Err(AError::ParityDisparity {
                        kind: "pattern arguments",
                        expected: *n,
                        expected_span: *s,
                        given: ps.len(),
                        given_span: *s2,
                    });
                }

                let mut ps = self.lower_patterns(ps)?;
                // Extend with any missing members
                ps.extend((ps.len()..*n).map(|_| self.fresh_empty_pattern(*s2)));

                Ok(ps)
            },
            (
                LConstructorShape::Named(_, expected),
                PPatternConstructorArguments::Named(s2, given, ignore),
            ) => {
                let mut seen = hashmap! {};
                let mut args = vec![None; expected.len()];

                for (is, n, p) in given {
                    if let Some(old_is) = seen.insert(*n, *is) {
                        return Err(AError::DuplicatedDefinition {
                            kind: "constructor field",
                            name: *n,
                            span: *is,
                            span2: old_is,
                        });
                    }
                    if let Some((pos, _)) = expected.get(n) {
                        args[*pos] = Some(self.lower_pattern(*p)?);
                    } else {
                        return Err(AError::UnexpectedSubItem {
                            parent_kind,
                            parent_name,
                            item_kind: "field",
                            item_name: *n,
                            span: *is,
                        });
                    }
                }

                if !ignore {
                    for (n, (_, is)) in expected {
                        if !seen.contains_key(n) {
                            return Err(AError::ExpectedSubItem {
                                parent_kind,
                                parent_name,
                                item_kind: "field",
                                item_name: *n,
                                span: *is,
                            });
                        }
                    }
                }

                // Unwrap each None hole into an `_` pattern
                Ok(args
                    .into_iter()
                    .map(|p| p.unwrap_or_else(|| self.fresh_empty_pattern(*s2)))
                    .collect())
            },
            (expected, given) => {
                let (expected_kind, expected_span) = expected.info();
                let (given_kind, given_span) = given.info();

                return Err(AError::IncorrectConstructor {
                    parent_kind,
                    parent_name,
                    expected_kind,
                    expected_span,
                    given_kind,
                    given_span,
                });
            },
        }
    }

    pub fn fresh_empty_pattern(&self, span: Span) -> Id<LPattern> {
        let ty = self.fresh_infer_ty(span);

        LPattern {
            span,
            source: None,
            ty: Some(ty),
            data: LPatternData::Underscore(ty),
        }
        .intern(self.ctx)
    }

    pub fn lower_patterns(&mut self, ps: &[Id<PPattern>]) -> AResult<Vec<Id<LPattern>>> {
        let mut ret = vec![];

        for p in ps {
            ret.push(self.lower_pattern(*p)?);
        }

        Ok(ret)
    }
}
