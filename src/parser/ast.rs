use std::collections::VecDeque;

use either::Either;

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    lexer::{Span, Token},
    util::{Id, LId, LateLookup, PrettyPrint},
};

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PItem {
    Module(Id<PModule>),
    Use(Id<PUse>),
    Global(Id<PGlobal>),
    Function(Id<PFunction>),
    Object(Id<PObject>),
    Enum(Id<PEnum>),
    Trait(Id<PTrait>),
    Impl(Id<PImpl>),
}

impl PItem {
    /*pub fn parent(self, ctx: &dyn AdelaideContext) -> Id<PModule> {
        match self {
            PItem::Module(m) => m.lookup(ctx).parent.get(ctx),
            PItem::Use(u) => u.lookup(ctx).parent.get(ctx),
            PItem::Global(g) => g.lookup(ctx).parent.get(ctx),
            PItem::Function(f) => f.lookup(ctx).parent.get(ctx),
            PItem::Object(o) => o.lookup(ctx).parent.get(ctx),
            PItem::Enum(e) => e.lookup(ctx).parent.get(ctx),
            PItem::Trait(t) => t.lookup(ctx).parent.get(ctx),
            PItem::Impl(i) => i.lookup(ctx).parent.get(ctx),
        }
    }*/
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PModule {
    #[plain]
    pub source: Id<AFile>,
    pub parent: LId<PModule>,
    pub span: Span,
    pub name: Id<str>,
    pub items: Vec<PItem>,
}

impl LateLookup for PModule {
    type Source = AFile;

    fn late_lookup(id: Id<Self::Source>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.parse_mod(id).unwrap()
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct PModPath(pub Id<PModule>);

impl PrettyPrint for PModPath {
    fn fmt(&self, f: &mut std::fmt::Formatter, ctx: &dyn AdelaideContext) -> std::fmt::Result {
        match self.0.lookup(ctx).source.lookup(ctx).mod_path.as_slice() {
            &[] => write!(f, "<ROOT>"),
            path => {
                for (i, p) in path.iter().enumerate() {
                    if i != 0 {
                        write!(f, "::")?;
                    }

                    write!(f, "{}", p)?;
                }

                Ok(())
            },
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PUse {
    pub parent: LId<PModule>,
    pub span: Span,
    pub absolute: bool,
    pub elements: Vec<PUseElement>,
}

impl PUse {
    pub fn new(
        span: Span,
        parent: LId<PModule>,
        absolute: bool,
        elements: Vec<PUseElement>,
    ) -> PUse {
        PUse {
            parent,
            span,
            absolute,
            elements,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PUseElement {
    UseAll(VecDeque<(Span, Id<str>)>, Span),
    UseSelf(VecDeque<(Span, Id<str>)>, Span, Option<(Span, Id<str>)>),
    Use(VecDeque<(Span, Id<str>)>, Option<(Span, Id<str>)>),
}

impl PUseElement {
    pub fn in_mod(span: Span, id: Id<str>, mut e: Vec<PUseElement>) -> Vec<PUseElement> {
        for i in e.iter_mut() {
            match i {
                PUseElement::UseAll(v, _)
                | PUseElement::UseSelf(v, _, _)
                | PUseElement::Use(v, _) => {
                    v.push_front((span, id));
                },
            }
        }

        e
    }

    pub fn plain_use(span: Span, i: Id<str>, r: Option<(Span, Id<str>)>) -> Vec<PUseElement> {
        vec![PUseElement::Use(vec![(span, i)].into(), r)]
    }

    pub fn self_use(span: Span, r: Option<(Span, Id<str>)>) -> Vec<PUseElement> {
        vec![PUseElement::UseSelf(vec![].into(), span, r)]
    }

    pub fn use_all(span: Span) -> Vec<PUseElement> {
        vec![PUseElement::UseAll(vec![].into(), span)]
    }
}

impl PGlobal {
    pub fn new(
        span: Span,
        parent: LId<PModule>,
        name: Id<str>,
        ty: Id<PType>,
        expr: Id<PExpression>,
    ) -> PGlobal {
        PGlobal {
            parent,
            span,
            name,
            ty,
            expr,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PFunction {
    pub parent: LId<PModule>,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub parameters: Vec<(Span, Id<str>, Id<PType>)>,
    pub return_ty: Id<PType>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
    pub body: Option<Id<PExpression>>,
}

impl PFunction {
    pub fn new(
        span: Span,
        parent: LId<PModule>,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
        body: Option<Id<PExpression>>,
    ) -> PFunction {
        PFunction {
            parent,
            span,
            name,
            generics,
            parameters,
            return_ty,
            restrictions,
            body,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PObject {
    pub parent: LId<PModule>,
    pub is_structural: bool,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
    pub members: PObjectMembers,
}

impl PObject {
    pub fn new(
        is_structural: bool,
        span: Span,
        parent: LId<PModule>,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
        members: PObjectMembers,
    ) -> PObject {
        PObject {
            parent,
            is_structural,
            span,
            name,
            generics,
            restrictions,
            members,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PObjectMembers {
    Empty(Span),
    Positional(Span, Vec<Id<PType>>),
    Named(Span, Vec<(Span, Id<str>, Id<PType>)>),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PTrait {
    pub parent: LId<PModule>,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
    pub members: Vec<PTraitMember>,
}

impl PTrait {
    pub fn new(
        span: Span,
        parent: LId<PModule>,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
        members: Vec<PTraitMember>,
    ) -> PTrait {
        PTrait {
            parent,
            span,
            name,
            generics,
            restrictions,
            members,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PTraitMember {
    Type(Span, Id<str>, Vec<Id<PTraitTypeWithBindings>>),
    Function {
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        has_self: Option<Span>,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
    },
}

impl PTraitMember {
    pub fn type_member(
        span: Span,
        name: Id<str>,
        restrictions: Vec<Id<PTraitTypeWithBindings>>,
    ) -> PTraitMember {
        PTraitMember::Type(span, name, restrictions)
    }

    pub fn function_member(
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        has_self: Option<Span>,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
    ) -> PTraitMember {
        PTraitMember::Function {
            span,
            name,
            generics,
            has_self,
            parameters,
            return_ty,
            restrictions,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PImpl {
    pub parent: LId<PModule>,
    pub span: Span,
    pub generics: Vec<(Span, Id<str>)>,
    pub ty: Id<PType>,
    pub trait_ty: Option<Id<PTraitType>>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
    pub members: Vec<PImplMember>,
}

impl PImpl {
    pub fn new(
        span: Span,
        parent: LId<PModule>,
        generics: Vec<(Span, Id<str>)>,
        ty: Id<PType>,
        trait_ty: Option<Id<PTraitType>>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
        members: Vec<PImplMember>,
    ) -> PImpl {
        PImpl {
            parent,
            span,
            generics,
            ty,
            trait_ty,
            restrictions,
            members,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PImplMember {
    Type(Span, Id<str>, Id<PType>),
    Function {
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        has_self: Option<Span>,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
        body: Id<PExpression>,
    },
}

impl PImplMember {
    pub fn type_member(span: Span, name: Id<str>, ty: Id<PType>) -> PImplMember {
        PImplMember::Type(span, name, ty)
    }

    pub fn function_member(
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        has_self: Option<Span>,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
        body: Id<PExpression>,
    ) -> PImplMember {
        PImplMember::Function {
            span,
            name,
            generics,
            has_self,
            parameters,
            return_ty,
            restrictions,
            body,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PEnum {
    pub parent: LId<PModule>,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
    pub variants: Vec<(Span, Id<str>, PObjectMembers)>,
}

impl PEnum {
    pub fn new(
        span: Span,
        parent: LId<PModule>,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitTypeWithBindings>>)>,
        variants: Vec<(Span, Id<str>, PObjectMembers)>,
    ) -> PEnum {
        PEnum {
            parent,
            span,
            name,
            generics,
            restrictions,
            variants,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PType {
    pub span: Span,
    pub data: PTypeData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PTypeData {
    Infer,

    Int,
    Float,
    Char,
    Bool,
    String,
    SelfType,
    Never,

    AmbiguousPath(Vec<(Span, Id<str>)>, Vec<Id<PType>>),

    Array(Id<PType>),
    Tuple(Vec<Id<PType>>),

    Closure(Vec<Id<PType>>, Id<PType>),
    FnPtr(Vec<Id<PType>>, Id<PType>),

    Associated(Id<PType>, Id<str>),

    Elaborated(Id<PType>, Id<PTraitType>),

    Dynamic(Id<PTraitTypeWithBindings>),

    AsyncBlock(Id<PType>),
}

impl PType {
    pub fn infer(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::Infer,
        }
    }

    pub fn int_type(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::Int,
        }
    }

    pub fn bool_type(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::Bool,
        }
    }

    pub fn float_type(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::Float,
        }
    }

    pub fn string_type(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::String,
        }
    }

    pub fn char_type(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::Char,
        }
    }

    pub fn self_type(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::SelfType,
        }
    }

    pub fn never_type(span: Span) -> PType {
        PType {
            span,
            data: PTypeData::Never,
        }
    }

    pub fn array(span: Span, elem: Id<PType>) -> PType {
        PType {
            span,
            data: PTypeData::Array(elem),
        }
    }

    pub fn tuple(span: Span, elems: Vec<Id<PType>>) -> PType {
        PType {
            span,
            data: PTypeData::Tuple(elems),
        }
    }

    pub fn closure_type(span: Span, params: Vec<Id<PType>>, ret: Id<PType>) -> PType {
        PType {
            span,
            data: PTypeData::Closure(params, ret),
        }
    }

    pub fn fn_ptr_type(span: Span, params: Vec<Id<PType>>, ret: Id<PType>) -> PType {
        PType {
            span,
            data: PTypeData::FnPtr(params, ret),
        }
    }

    pub fn elaborated_type(span: Span, t: Id<PType>, tr: Id<PTraitType>) -> PType {
        PType {
            span,
            data: PTypeData::Elaborated(t, tr),
        }
    }

    pub fn dynamic_type(span: Span, tr: Id<PTraitTypeWithBindings>) -> PType {
        PType {
            span,
            data: PTypeData::Dynamic(tr),
        }
    }

    pub fn ambiguous(span: Span, p: Vec<(Span, Id<str>)>, g: Vec<Id<PType>>) -> PType {
        PType {
            span,
            data: PTypeData::AmbiguousPath(p, g),
        }
    }

    pub fn associated_type(span: Span, t: Id<PType>, a: Id<str>) -> PType {
        PType {
            span,
            data: PTypeData::Associated(t, a),
        }
    }

    pub fn awaitable(span: Span, t: Id<PType>) -> PType {
        PType {
            span,
            data: PTypeData::AsyncBlock(t),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PTraitType {
    pub span: Span,
    pub path: Vec<(Span, Id<str>)>,
    pub generics: Vec<Id<PType>>,
}

impl PTraitType {
    pub fn new(span: Span, path: Vec<(Span, Id<str>)>, generics: Vec<Id<PType>>) -> PTraitType {
        PTraitType {
            span,
            path,
            generics,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub enum PTraitTypeWithBindings {
    Plain {
        span: Span,
        path: Vec<(Span, Id<str>)>,
        generics: Vec<Id<PType>>,
        bindings: Vec<(Span, Id<str>, Id<PType>)>,
    },
    Function {
        span: Span,
        params: Vec<Id<PType>>,
        ret: Id<PType>,
    },
}

impl PTraitTypeWithBindings {
    pub fn new(
        span: Span,
        path: Vec<(Span, Id<str>)>,
        generics_and_bindings: Vec<Either<Id<PType>, (Span, Id<str>, Id<PType>)>>,
    ) -> PTraitTypeWithBindings {
        let mut generics = vec![];
        let mut bindings = vec![];

        // I wish I could use Iterator::partition here lol
        for e in generics_and_bindings {
            match e {
                Either::Left(l) => generics.push(l),
                Either::Right(r) => bindings.push(r),
            }
        }

        PTraitTypeWithBindings::Plain {
            span,
            path,
            generics,
            bindings,
        }
    }

    pub fn fn_trait(span: Span, params: Vec<Id<PType>>, ret: Id<PType>) -> PTraitTypeWithBindings {
        PTraitTypeWithBindings::Function { span, params, ret }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PGlobal {
    pub parent: LId<PModule>,
    pub span: Span,
    pub name: Id<str>,
    pub ty: Id<PType>,
    pub expr: Id<PExpression>,
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PExpression {
    pub span: Span,
    pub data: PExpressionData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PExpressionData {
    SelfRef,
    Unimplemented,
    Identifiers(Vec<(Span, Id<str>)>, Vec<Id<PType>>),
    Block(Vec<Id<PStatement>>, Id<PExpression>),
    AsyncBlock(Id<PExpression>),
    GeneratorBlock(
        bool,
        Option<(Span, Id<str>, Id<PType>)>,
        Id<PType>,
        Id<PType>,
        Id<PExpression>,
    ),
    Tuple(Vec<Id<PExpression>>),
    ArrayLiteral(Vec<Id<PExpression>>),
    Array(Id<PType>, Id<PExpression>),
    Literal(PLiteral),
    BinOp(Id<PExpression>, PBinopKind, Id<PExpression>),
    Assign(Id<PExpression>, Id<PExpression>),
    Not(Id<PExpression>),
    Neg(Id<PExpression>),
    InfiniteRange(Id<PExpression>),
    InterpolationBegin(Id<str>, Id<PExpression>),
    InterpolationContinue(Id<PExpression>, Id<str>, Id<PExpression>),
    InterpolationEnd(Id<PExpression>, Id<str>),
    Call(Id<PExpression>, Vec<Id<PExpression>>),
    StaticCall(Id<PType>, Id<str>, Vec<Id<PType>>, Vec<Id<PExpression>>),
    ObjectCall(
        Id<PExpression>,
        Id<str>,
        Vec<Id<PType>>,
        Vec<Id<PExpression>>,
    ),
    If(Id<PExpression>, Id<PExpression>, Id<PExpression>),
    IfLet(
        Id<PPattern>,
        Id<PExpression>,
        Id<PExpression>,
        Id<PExpression>,
    ),
    Loop(Option<Id<str>>, Id<PExpression>),
    While(
        Option<Id<str>>,
        Id<PExpression>,
        Id<PExpression>,
        Id<PExpression>,
    ),
    WhileLet(
        Option<Id<str>>,
        Id<PPattern>,
        Id<PExpression>,
        Id<PExpression>,
        Id<PExpression>,
    ),
    For(
        Option<Id<str>>,
        Id<PPattern>,
        Id<PExpression>,
        Id<PExpression>,
        Id<PExpression>,
    ),
    Match(Id<PExpression>, Vec<(Id<PPattern>, Id<PExpression>)>),
    StructuralAmbiguous(Vec<(Span, Id<str>)>, Vec<Id<PType>>, PConstructorArguments),
    StructuralVariant(
        Vec<(Span, Id<str>)>,
        Vec<Id<PType>>,
        Id<str>,
        PConstructorArguments,
    ),
    Allocate(Vec<(Span, Id<str>)>, Vec<Id<PType>>, PConstructorArguments),
    Return(Id<PExpression>),
    Yield(Id<PExpression>),
    Assert(Id<PExpression>),
    Break(Option<Id<PExpression>>, Option<(Span, Id<str>)>),
    Continue(Option<(Span, Id<str>)>),
    Closure(Vec<Id<PPattern>>, Id<PType>, Id<PExpression>),
    Throw(Id<PExpression>),
    Index(Id<PExpression>, Id<PExpression>),
    NamedAccess(Id<PExpression>, Span, Id<str>),
    IndexAccess(Id<PExpression>, Span, Id<str>),
    Await(Id<PExpression>),
}

impl PExpression {
    pub fn identifiers(span: Span, i: Vec<(Span, Id<str>)>, g: Vec<Id<PType>>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Identifiers(i, g),
        }
    }

    pub fn block(
        span: Span,
        statements: Vec<Id<PStatement>>,
        expression: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Block(statements, expression),
        }
    }

    pub fn async_block(span: Span, expression: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::AsyncBlock(expression),
        }
    }

    pub fn generator_block(
        span: Span,
        is_async: bool,
        in_param: Option<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        yield_ty: Id<PType>,
        expr: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::GeneratorBlock(is_async, in_param, return_ty, yield_ty, expr),
        }
    }

    pub fn unit(span: Span) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Tuple(vec![]),
        }
    }

    pub fn self_ref(span: Span) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::SelfRef,
        }
    }

    pub fn unimplemented(span: Span) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Unimplemented,
        }
    }

    pub fn literal(span: Span, data: PLiteral) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Literal(data),
        }
    }

    pub fn tuple(span: Span, data: Vec<Id<PExpression>>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Tuple(data),
        }
    }

    pub fn array_literal(span: Span, data: Vec<Id<PExpression>>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::ArrayLiteral(data),
        }
    }

    pub fn allocate_array(span: Span, ty: Id<PType>, count: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Array(ty, count),
        }
    }

    pub fn interpolation_begin(span: Span, s: Id<str>, e: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::InterpolationBegin(s, e),
        }
    }

    pub fn interpolation_continue(
        span: Span,
        e: Id<PExpression>,
        c: Id<str>,
        s: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::InterpolationContinue(e, c, s),
        }
    }

    pub fn interpolation_end(span: Span, e: Id<PExpression>, c: Id<str>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::InterpolationEnd(e, c),
        }
    }

    pub fn expr_call(span: Span, c: Id<PExpression>, p: Vec<Id<PExpression>>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Call(c, p),
        }
    }

    pub fn static_call(
        span: Span,
        t: Id<PType>,
        f: Id<str>,
        g: Vec<Id<PType>>,
        p: Vec<Id<PExpression>>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::StaticCall(t, f, g, p),
        }
    }

    pub fn object_call(
        span: Span,
        e: Id<PExpression>,
        f: Id<str>,
        g: Vec<Id<PType>>,
        p: Vec<Id<PExpression>>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::ObjectCall(e, f, g, p),
        }
    }

    pub fn if_statement(
        span: Span,
        c: Id<PExpression>,
        t: Id<PExpression>,
        e: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::If(c, t, e),
        }
    }

    pub fn if_let_statement(
        span: Span,
        p: Id<PPattern>,
        c: Id<PExpression>,
        t: Id<PExpression>,
        e: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::IfLet(p, c, t, e),
        }
    }

    pub fn plain_loop(span: Span, label: Option<Id<str>>, e: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Loop(label, e),
        }
    }

    pub fn while_loop(
        span: Span,
        label: Option<Id<str>>,
        c: Id<PExpression>,
        t: Id<PExpression>,
        e: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::While(label, c, t, e),
        }
    }

    pub fn while_let_loop(
        span: Span,
        label: Option<Id<str>>,
        p: Id<PPattern>,
        c: Id<PExpression>,
        t: Id<PExpression>,
        e: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::WhileLet(label, p, c, t, e),
        }
    }

    pub fn for_loop(
        span: Span,
        label: Option<Id<str>>,
        p: Id<PPattern>,
        c: Id<PExpression>,
        t: Id<PExpression>,
        e: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::For(label, p, c, t, e),
        }
    }

    pub fn match_statement(
        span: Span,
        e: Id<PExpression>,
        b: Vec<(Id<PPattern>, Id<PExpression>)>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Match(e, b),
        }
    }

    pub fn binop(span: Span, a: Id<PExpression>, k: PBinopKind, b: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::BinOp(a, k, b),
        }
    }

    pub fn assign(span: Span, a: Id<PExpression>, b: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Assign(a, b),
        }
    }

    pub fn not(span: Span, a: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Not(a),
        }
    }

    pub fn neg(span: Span, a: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Neg(a),
        }
    }

    pub fn infinite_range(span: Span, a: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::InfiniteRange(a),
        }
    }

    pub fn structural_ambiguous(
        span: Span,
        t: Vec<(Span, Id<str>)>,
        g: Vec<Id<PType>>,
        c: PConstructorArguments,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::StructuralAmbiguous(t, g, c),
        }
    }

    pub fn structural_variant(
        span: Span,
        t: Vec<(Span, Id<str>)>,
        g: Vec<Id<PType>>,
        v: Id<str>,
        c: PConstructorArguments,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::StructuralVariant(t, g, v, c),
        }
    }

    pub fn allocate(
        span: Span,
        t: Vec<(Span, Id<str>)>,
        g: Vec<Id<PType>>,
        c: PConstructorArguments,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Allocate(t, g, c),
        }
    }

    pub fn return_statement(span: Span, data: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Return(data),
        }
    }

    pub fn yield_statement(span: Span, data: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Yield(data),
        }
    }

    pub fn assert_statement(span: Span, data: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Assert(data),
        }
    }

    pub fn break_stmt(
        span: Span,
        e: Option<Id<PExpression>>,
        l: Option<(Span, Id<str>)>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Break(e, l),
        }
    }

    pub fn continue_stmt(span: Span, l: Option<(Span, Id<str>)>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Continue(l),
        }
    }

    pub fn closure(
        span: Span,
        p: Vec<Id<PPattern>>,
        r: Id<PType>,
        e: Id<PExpression>,
    ) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Closure(p, r, e),
        }
    }

    pub fn throw(span: Span, data: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Throw(data),
        }
    }

    pub fn index(span: Span, a: Id<PExpression>, i: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Index(a, i),
        }
    }

    pub fn await_expr(span: Span, a: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Await(a),
        }
    }

    pub fn named_access(span: Span, a: Id<PExpression>, i_span: Span, i: Id<str>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::NamedAccess(a, i_span, i),
        }
    }

    pub fn index_access(span: Span, a: Id<PExpression>, i_span: Span, i: Id<str>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::IndexAccess(a, i_span, i),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PBinopKind {
    RangeInclusive,
    RangeExclusive,
    OrCircuit,
    AndCircuit,
    Or,
    And,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

impl From<Token> for PBinopKind {
    fn from(t: Token) -> Self {
        match t {
            Token::DotDotEq => PBinopKind::RangeInclusive,
            Token::DotDot => PBinopKind::RangeExclusive,
            Token::PipeShort => PBinopKind::OrCircuit,
            Token::AndShort => PBinopKind::AndCircuit,
            Token::Pipe => PBinopKind::Or,
            Token::And => PBinopKind::And,
            Token::Lt => PBinopKind::Lt,
            Token::Gt => PBinopKind::Gt,
            Token::LessEqual => PBinopKind::Le,
            Token::GreaterEqual => PBinopKind::Ge,
            Token::EqualsEquals => PBinopKind::Eq,
            Token::NotEquals => PBinopKind::Ne,
            Token::Plus => PBinopKind::Plus,
            Token::Minus => PBinopKind::Minus,
            Token::Star => PBinopKind::Mul,
            Token::Slash => PBinopKind::Div,
            Token::Modulo => PBinopKind::Mod,
            _ => unreachable!("Unexpected token: {}", t),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PConstructorArguments {
    Empty(Span),
    Positional(Span, Vec<Id<PExpression>>),
    Named(Span, Vec<(Span, Id<str>, Id<PExpression>)>),
}

impl PConstructorArguments {
    pub fn info(&self) -> (&'static str, Span) {
        match self {
            PConstructorArguments::Empty(s) => ("no", *s),
            PConstructorArguments::Positional(s, _) => ("positional", *s),
            PConstructorArguments::Named(s, _) => ("named", *s),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PLiteral {
    True,
    False,
    String(Id<str>),
    Int(Id<str>),
    Float(Id<str>),
    Char(char),
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PStatement {
    pub span: Span,
    pub data: PStatementData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PStatementData {
    Expression(Id<PExpression>),
    Let(Id<PPattern>, Id<PExpression>),
}

impl PStatement {
    pub fn expression_statement(span: Span, data: Id<PExpression>) -> PStatement {
        PStatement {
            span,
            data: PStatementData::Expression(data),
        }
    }

    pub fn let_statement(span: Span, pattern: Id<PPattern>, data: Id<PExpression>) -> PStatement {
        PStatement {
            span,
            data: PStatementData::Let(pattern, data),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Lookup, PrettyPrint)]
pub struct PPattern {
    pub span: Span,
    pub ty: Option<Id<PType>>,
    pub data: PPatternData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PPatternData {
    Underscore,
    Literal(PLiteral),
    Identifier(Id<str>),
    Tuple(Vec<Id<PPattern>>),
    StructuralAmbiguous(
        Vec<(Span, Id<str>)>,
        Vec<Id<PType>>,
        PPatternConstructorArguments,
    ),
    StructuralVariant(
        Vec<(Span, Id<str>)>,
        Vec<Id<PType>>,
        Id<str>,
        PPatternConstructorArguments,
    ),
}

impl PPattern {
    pub fn underscore(span: Span, ty: Option<Id<PType>>) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Underscore,
        }
    }

    pub fn literal(span: Span, ty: Option<Id<PType>>, l: PLiteral) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Literal(l),
        }
    }

    pub fn identifier(span: Span, ty: Option<Id<PType>>, i: Id<str>) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Identifier(i),
        }
    }

    pub fn tuple(span: Span, ty: Option<Id<PType>>, t: Vec<Id<PPattern>>) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Tuple(t),
        }
    }

    pub fn structural_ambiguous(
        span: Span,
        ty: Option<Id<PType>>,
        t: Vec<(Span, Id<str>)>,
        g: Vec<Id<PType>>,
        c: PPatternConstructorArguments,
    ) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::StructuralAmbiguous(t, g, c),
        }
    }

    pub fn structural_variant(
        span: Span,
        ty: Option<Id<PType>>,
        t: Vec<(Span, Id<str>)>,
        g: Vec<Id<PType>>,
        v: Id<str>,
        c: PPatternConstructorArguments,
    ) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::StructuralVariant(t, g, v, c),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PPatternConstructorArguments {
    Empty(Span),
    Positional(Span, Vec<Id<PPattern>>, bool),
    Named(Span, Vec<(Span, Id<str>, Id<PPattern>)>, bool),
}

impl PPatternConstructorArguments {
    pub fn info(&self) -> (&'static str, Span) {
        match self {
            PPatternConstructorArguments::Empty(s) => ("no", *s),
            PPatternConstructorArguments::Positional(s, _, _) => ("positional", *s),
            PPatternConstructorArguments::Named(s, _, _) => ("named", *s),
        }
    }
}
