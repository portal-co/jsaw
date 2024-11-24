use crate::*;

impl Cfg {
    pub fn simplify(&mut self) {
        for (k, kd) in self.blocks.iter_mut() {
            if let Term::CondJmp {
                cond,
                if_true,
                if_false,
            } = &mut kd.end.term
            {
                if let Expr::Lit(Lit::Bool(b)) = cond {
                    kd.end.term = Term::Jmp(if b.value { *if_true } else { *if_false })
                }
            };
        }
    }
}
