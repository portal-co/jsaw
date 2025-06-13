use crate::*;

impl Cfg {
    pub fn simplify(&mut self) {
        for (_block_id, block_data) in self.blocks.iter_mut() {
            if let Term::CondJmp {
                cond,
                if_true,
                if_false,
            } = &mut block_data.end.term
            {
                if let Expr::Lit(Lit::Bool(boolean_literal)) = cond {
                    block_data.end.term = Term::Jmp(if boolean_literal.value { *if_true } else { *if_false })
                }
            };
        }
    }
}
