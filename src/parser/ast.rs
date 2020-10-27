use std::{collections::VecDeque, sync::Arc};

use either::Either;

use crate::{
    ctx::AdelaideContext,
    file::AFile,
    lexer::{Span, Token},
    util::{BackId, Id, Lookup},
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

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PModule {
    pub file_id: BackId<AFile>,
    pub span: Span,
    pub name: Id<str>,
    pub items: Vec<PItem>,
}

impl Lookup for PModule {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_pmodule(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_pmodule(self)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PUse {
    pub span: Span,
    pub absolute: bool,
    pub elements: Vec<PUseElement>,
}

impl Lookup for PUse {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_puse(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_puse(self)
    }
}

impl PUse {
    pub fn new(span: Span, absolute: bool, elements: Vec<PUseElement>) -> PUse {
        PUse {
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
    pub fn new(span: Span, name: Id<str>, ty: Id<PType>, expr: Id<PExpression>) -> PGlobal {
        PGlobal {
            span,
            name,
            ty,
            expr,
        }
    }
}

impl Lookup for PGlobal {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_pglobal(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_pglobal(self)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PFunction {
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub parameters: Vec<(Span, Id<str>, Id<PType>)>,
    pub return_ty: Id<PType>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
    pub body: Option<Id<PExpression>>,
}

impl Lookup for PFunction {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_pfunction(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_pfunction(self)
    }
}

impl PFunction {
    pub fn new(
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
        body: Option<Id<PExpression>>,
    ) -> PFunction {
        PFunction {
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

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PObject {
    pub is_structural: bool,
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
    pub members: PObjectMembers,
}

impl Lookup for PObject {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_pobject(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_pobject(self)
    }
}

impl PObject {
    pub fn new(
        is_structural: bool,
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
        members: PObjectMembers,
    ) -> PObject {
        PObject {
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

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PTrait {
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
    pub members: Vec<PTraitMember>,
}

impl Lookup for PTrait {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_ptrait(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_ptrait(self)
    }
}

impl PTrait {
    pub fn new(
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
        members: Vec<PTraitMember>,
    ) -> PTrait {
        PTrait {
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
    Type(Span, Id<str>, Vec<Id<PTraitType>>),
    Function {
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        has_self: bool,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
    },
}

impl PTraitMember {
    pub fn type_member(
        span: Span,
        name: Id<str>,
        restrictions: Vec<Id<PTraitType>>,
    ) -> PTraitMember {
        PTraitMember::Type(span, name, restrictions)
    }

    pub fn function_member(
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        has_self: bool,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
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

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PImpl {
    pub span: Span,
    pub generics: Vec<(Span, Id<str>)>,
    pub ty: Id<PType>,
    pub trait_ty: Option<Id<PTraitType>>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
    pub members: Vec<PImplMember>,
}

impl Lookup for PImpl {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_pimpl(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_pimpl(self)
    }
}

impl PImpl {
    pub fn new(
        span: Span,
        generics: Vec<(Span, Id<str>)>,
        ty: Id<PType>,
        trait_ty: Option<Id<PTraitType>>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
        members: Vec<PImplMember>,
    ) -> PImpl {
        PImpl {
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
        has_self: bool,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
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
        has_self: bool,
        parameters: Vec<(Span, Id<str>, Id<PType>)>,
        return_ty: Id<PType>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
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

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PEnum {
    pub span: Span,
    pub name: Id<str>,
    pub generics: Vec<(Span, Id<str>)>,
    pub restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
    pub variants: Vec<(Span, Id<str>, PObjectMembers)>,
}

impl Lookup for PEnum {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_penum(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_penum(self)
    }
}

impl PEnum {
    pub fn new(
        span: Span,
        name: Id<str>,
        generics: Vec<(Span, Id<str>)>,
        restrictions: Vec<(Id<PType>, Vec<Id<PTraitType>>)>,
        variants: Vec<(Span, Id<str>, PObjectMembers)>,
    ) -> PEnum {
        PEnum {
            span,
            name,
            generics,
            restrictions,
            variants,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
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

    AmbiguousPath(Vec<Id<str>>, Vec<Id<PType>>),

    Array(Id<PType>),
    Tuple(Vec<Id<PType>>),

    Closure(Vec<Id<PType>>, Id<PType>),
    FnPtr(Vec<Id<PType>>, Id<PType>),

    Associated(Id<PType>, Id<str>),

    Elaborated(Id<PType>, Id<PTraitType>),

    Dynamic(Vec<Id<PTraitType>>),

    Awaitable(Id<PType>),
}

impl Lookup for PType {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_ptype(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_ptype(self)
    }
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

    pub fn dynamic_type(span: Span, tr: Vec<Id<PTraitType>>) -> PType {
        PType {
            span,
            data: PTypeData::Dynamic(tr),
        }
    }

    pub fn ambiguous(span: Span, p: Vec<Id<str>>, g: Vec<Id<PType>>) -> PType {
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
            data: PTypeData::Awaitable(t),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PTraitType {
    Plain {
        span: Span,
        path: Vec<Id<str>>,
        generics: Vec<Id<PType>>,
        bounds: Vec<(Id<str>, Id<PType>)>,
    },
    Function {
        span: Span,
        params: Vec<Id<PType>>,
        ret: Id<PType>,
    },
}

impl Lookup for PTraitType {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_ptraittype(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_ptraittype(self)
    }
}

impl PTraitType {
    pub fn new_mixed(
        span: Span,
        path: Vec<Id<str>>,
        generics_and_bounds: Vec<Either<Id<PType>, (Id<str>, Id<PType>)>>,
    ) -> PTraitType {
        let mut generics = vec![];
        let mut bounds = vec![];

        // I wish I could use Iterator::partition here lol
        for e in generics_and_bounds {
            match e {
                Either::Left(l) => generics.push(l),
                Either::Right(r) => bounds.push(r),
            }
        }

        PTraitType::Plain {
            span,
            path,
            generics,
            bounds,
        }
    }

    pub fn fn_trait(span: Span, params: Vec<Id<PType>>, ret: Id<PType>) -> PTraitType {
        PTraitType::Function { span, params, ret }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PGlobal {
    pub span: Span,
    pub name: Id<str>,
    pub ty: Id<PType>,
    pub expr: Id<PExpression>,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PExpression {
    pub span: Span,
    pub data: PExpressionData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PExpressionData {
    SelfRef,
    Unimplemented,
    Identifiers(Vec<Id<str>>, Vec<Id<PType>>),
    Block(Vec<Id<PStatement>>, Id<PExpression>),
    AsyncBlock(Id<PExpression>),
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
    While(
        Option<Id<str>>,
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
    Structural(Id<PType>, PConstructorArguments),
    Allocate(Id<PType>, PConstructorArguments),
    Return(Id<PExpression>),
    Assert(Id<PExpression>),
    Break(Option<Id<PExpression>>, Option<(Span, Id<str>)>),
    Continue(Option<(Span, Id<str>)>),
    Closure(Vec<Id<PPattern>>, Id<PType>, Id<PExpression>),
    Throw(Id<PExpression>),
    Index(Id<PExpression>, Id<PExpression>),
    NamedAccess(Id<PExpression>, Id<str>),
    IndexAccess(Id<PExpression>, Id<str>),
    Await(Id<PExpression>),
}

impl PExpression {
    pub fn identifiers(span: Span, i: Vec<Id<str>>, g: Vec<Id<PType>>) -> PExpression {
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
            data: PExpressionData::Neg(a),
        }
    }

    pub fn neg(span: Span, a: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Not(a),
        }
    }

    pub fn infinite_range(span: Span, a: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::InfiniteRange(a),
        }
    }

    pub fn structural(span: Span, t: Id<PType>, c: PConstructorArguments) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Structural(t, c),
        }
    }

    pub fn allocate(span: Span, t: Id<PType>, c: PConstructorArguments) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Allocate(t, c),
        }
    }

    pub fn return_statement(span: Span, data: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Return(data),
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

    pub fn named_access(span: Span, a: Id<PExpression>, i: Id<str>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::NamedAccess(a, i),
        }
    }

    pub fn index_access(span: Span, a: Id<PExpression>, i: Id<str>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::IndexAccess(a, i),
        }
    }
}

impl Lookup for PExpression {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_pexpression(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_pexpression(self)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PBinopKind {
    RangeInclusive,
    RangeExclusive,
    Assign,
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
            Token::Equals => PBinopKind::Assign,
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

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PLiteral {
    True,
    False,
    String(Id<str>),
    Int(Id<str>),
    Float(Id<str>),
    Char(char),
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
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

impl Lookup for PStatement {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_pstatement(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_pstatement(self)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PPattern {
    pub span: Span,
    pub ty: Id<PType>,
    pub data: PPatternData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PPatternData {
    Underscore,
    Literal(PLiteral),
    Identifier(Id<str>),
    Tuple(Vec<Id<PPattern>>),
    Structural(Id<PType>, PPatternConstructorArguments),
}

impl PPattern {
    pub fn underscore(span: Span, ty: Id<PType>) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Underscore,
        }
    }

    pub fn literal(span: Span, ty: Id<PType>, l: PLiteral) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Literal(l),
        }
    }

    pub fn identifier(span: Span, ty: Id<PType>, i: Id<str>) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Identifier(i),
        }
    }

    pub fn tuple(span: Span, ty: Id<PType>, t: Vec<Id<PPattern>>) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Tuple(t),
        }
    }

    pub fn structural(
        span: Span,
        ty: Id<PType>,
        t: Id<PType>,
        c: PPatternConstructorArguments,
    ) -> PPattern {
        PPattern {
            span,
            ty,
            data: PPatternData::Structural(t, c),
        }
    }
}

impl Lookup for PPattern {
    fn lookup(id: Id<Self>, ctx: &dyn AdelaideContext) -> Arc<Self> {
        ctx.lookup_intern_ppattern(id)
    }

    fn intern_self(self: Arc<Self>, ctx: &dyn AdelaideContext) -> Id<Self> {
        ctx.intern_ppattern(self)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PPatternConstructorArguments {
    Empty(Span),
    Positional(Span, Vec<Id<PPattern>>, bool),
    Named(Span, Vec<(Span, Id<str>, Id<PPattern>)>, bool),
}
