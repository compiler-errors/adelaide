use std::collections::HashMap;

use crate::{
    lowering::{LPattern, LPatternData, VariableId},
    typechecker::Typechecker,
    util::{AResult, Id, TryCollectVec},
};

use super::{
    expr::{CLiteral, CStackId},
    Translator,
};

#[derive(Debug, Copy, Clone)]
pub enum CPattern<'a> {
    Underscore,
    Literal(CLiteral<'a>),
    Variable(CStackId),
    Destructure(&'a [CPattern<'a>]),
    DestructureEnum(&'a str, &'a [CPattern<'a>]),
}

impl<'a> Translator<'_, 'a> {
    pub fn translate_pattern(
        &mut self,
        pattern: Id<LPattern>,
        tyck: &mut Typechecker,
        slots: &HashMap<VariableId, CStackId>,
    ) -> AResult<CPattern<'a>> {
        let info = pattern.lookup(self.ctx);

        let pattern = match &info.data {
            LPatternData::Underscore(_) => CPattern::Underscore,
            LPatternData::Literal(l) => CPattern::Literal(self.translate_literal(*l)),
            LPatternData::Variable(v) => {
                debug!("Looking for slot for ID {:?}", v.id);
                CPattern::Variable(slots[&v.id])
            },
            LPatternData::Tuple(ps) | LPatternData::StructPattern(_, _, ps) =>
                CPattern::Destructure(self.translate_sub_patterns(&ps, tyck, slots)?),
            LPatternData::EnumVariantPattern(_, _, v, ps) => CPattern::DestructureEnum(
                self.deintern_string(*v),
                self.translate_sub_patterns(&ps, tyck, slots)?,
            ),
        };

        Ok(pattern)
    }

    pub fn translate_sub_patterns(
        &mut self,
        patterns: &[Id<LPattern>],
        tyck: &mut Typechecker,
        slots: &HashMap<VariableId, CStackId>,
    ) -> AResult<&'a [CPattern<'a>]> {
        let patterns = patterns
            .iter()
            .map(|pattern| self.translate_pattern(*pattern, tyck, slots))
            .try_collect_vec()?;
        Ok(self.alloc.alloc_slice_copy(&patterns))
    }
}
