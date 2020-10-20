use std::{collections::BTreeMap, sync::Arc};

use either::Either;

use crate::{ctx::AdelaideContext, file::AFile, lexer::Span, lexer::Token, util::{Id, Lookup}};

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub enum PItem {
    Global(Id<PGlobal>),
    Module(Id<PModule>),
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PModule {
    #[plain]
    pub file_id: Id<AFile>,
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
pub struct PType {
    span: Span,
    data: PTypeData,
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

    ClosureType(Vec<Id<PType>>, Id<PType>),
    FnPtrType(Vec<Id<PType>>, Id<PType>),

    AssociatedType(Id<PType>, Id<str>),

    ElaboratedType(Id<PType>, Id<PTraitType>),

    DynamicType(Vec<Id<PTraitType>>),
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
            data: PTypeData::ClosureType(params, ret),
        }
    }

    pub fn fn_ptr_type(span: Span, params: Vec<Id<PType>>, ret: Id<PType>) -> PType {
        PType {
            span,
            data: PTypeData::FnPtrType(params, ret),
        }
    }

    pub fn elaborated_type(span: Span, t: Id<PType>, tr: Id<PTraitType>) -> PType {
        PType {
            span,
            data: PTypeData::ElaboratedType(t, tr),
        }
    }

    pub fn dynamic_type(span: Span, tr: Vec<Id<PTraitType>>) -> PType {
        PType {
            span,
            data: PTypeData::DynamicType(tr),
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
            data: PTypeData::AssociatedType(t, a),
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

    pub fn new_generics(span: Span, path: Vec<Id<str>>, generics: Vec<Id<PType>>) -> PTraitType {
        PTraitType::Plain {
            span,
            path,
            generics,
            bounds: vec![],
        }
    }

    pub fn fn_trait(span: Span, params: Vec<Id<PType>>, ret: Id<PType>) -> PTraitType {
        PTraitType::Function { span, params, ret }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
pub struct PGlobal {
    span: Span,
    name: Id<str>,
    ty: Id<PType>,
    expr: Id<PExpression>,
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
pub struct PExpression {
    span: Span,
    data: PExpressionData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
enum PExpressionData {
    Block(Vec<Id<PStatement>>, Id<PExpression>),
    Tuple(Vec<Id<PExpression>>),
    Integer(Id<str>),
}

impl PExpression {
    pub fn block(span: Span, statements: Vec<Id<PStatement>>, expression: Id<PExpression>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Block(statements, expression),
        }
    }

    pub fn unit(span: Span) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Tuple(vec![]),
        }
    }

    pub fn integer(span: Span, data: Id<str>) -> PExpression {
        PExpression {
            span,
            data: PExpressionData::Integer(data),
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

pub enum PBinopKind {
    Range,
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
            Token::DotDot => PBinopKind::Range,
            Token::Equals => PBinopKind::Assign,
            Token::PipeShort => PBinopKind::OrCircuit,
            Token::AndShort => PBinopKind::AndCircuit,
            Token::Pipe => PBinopKind::Or,
            Token::And => PBinopKind::And,
            Token::LSqBracket => PBinopKind::Lt,
            Token::RSqBracket => PBinopKind::Gt,
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

pub enum PConstructorArguments {
    Empty(Span),
    Positional(Span, Vec<Id<PExpression>>),
    Named(Span, Vec<(Span, Id<str>, Id<PExpression>)>),
}

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
    span: Span,
    data: PStatementData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
enum PStatementData {
    Integer(Id<str>),
}

impl PStatement {
    pub fn integer(span: Span, data: Id<str>) -> PStatement {
        PStatement {
            span,
            data: PStatementData::Integer(data),
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
    span: Span,
    data: PPatternData,
}

#[derive(Debug, Hash, Eq, PartialEq, PrettyPrint)]
enum PPatternData {
    Integer(Id<str>),
}

impl PPattern {
    pub fn integer(span: Span, data: Id<str>) -> PPattern {
        PPattern {
            span,
            data: PPatternData::Integer(data),
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

pub enum PPatternConstructorArguments {
    Empty(Span),
    Positional(Span, Vec<Id<PPattern>>, bool),
    Named(Span, Vec<(Span, Id<str>, Id<PPattern>)>, bool),
}
