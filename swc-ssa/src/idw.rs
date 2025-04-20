use std::collections::BTreeMap;

use id_arena::Id;

use crate::{SBlock, SCatch, SFunc, SwcFunc};

pub struct Idw {
    pub map: BTreeMap<Id<SBlock>, Id<SBlock>>,
}
impl Idw {
    pub fn trans(
        &mut self,
        orig: &SwcFunc,
        new: &mut SwcFunc,
        a: Id<SBlock>,
    ) -> anyhow::Result<Id<SBlock>> {
        loop {
            if let Some(a) = self.map.get(&a).cloned() {
                return Ok(a);
            }
            let n = new.blocks.alloc(Default::default());
            self.map.insert(a, n);
            let mut vals = orig.blocks[a]
                .params
                .iter()
                .cloned()
                .map(|a| a.0)
                .map(|a| (a, new.add_blockparam(n)))
                .collect::<BTreeMap<_, _>>();
            let c = match &orig.blocks[a].postcedent.catch {
                crate::SCatch::Throw => SCatch::Throw,
                crate::SCatch::Just { target } => SCatch::Just {
                    target: crate::STarget {
                        block: self.trans(orig, new, target.block)?,
                        args: target
                            .args
                            .iter()
                            .filter_map(|a| vals.get(a))
                            .cloned()
                            .collect(),
                    },
                },
            };
        }
    }
}
