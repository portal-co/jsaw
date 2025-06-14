use std::collections::BTreeMap;

use id_arena::Id;

use crate::{SBlock, SCatch, SFunc, SCfg};

pub struct Idw {
    pub map: BTreeMap<Id<SBlock>, Id<SBlock>>,
}
impl Idw {
    pub fn trans(
        &mut self,
        orig: &SCfg,
        new: &mut SCfg,
        block_id: Id<SBlock>,
    ) -> anyhow::Result<Id<SBlock>> {
        loop {
            if let Some(mapped_id) = self.map.get(&block_id).cloned() {
                return Ok(mapped_id);
            }
            let new_block_id = new.blocks.alloc(Default::default());
            self.map.insert(block_id, new_block_id);
            let mut param_map = orig.blocks[block_id]
                .params
                .iter()
                .cloned()
                .map(|param| param.0)
                .map(|param_id| (param_id, new.add_blockparam(new_block_id)))
                .collect::<BTreeMap<_, _>>();
            let catch_clause = match &orig.blocks[block_id].postcedent.catch {
                crate::SCatch::Throw => SCatch::Throw,
                crate::SCatch::Just { target } => SCatch::Just {
                    target: crate::STarget {
                        block: self.trans(orig, new, target.block)?,
                        args: target
                            .args
                            .iter()
                            .filter_map(|arg| param_map.get(arg))
                            .cloned()
                            .collect(),
                    },
                },
            };
        }
    }
}
