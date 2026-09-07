//! Tail-callable-set computation.
//!
//! Neither the JVM nor Swift guarantees tail-call elimination, so the
//! mobile backends implement frame replacement themselves. Only a subset of
//! functions ever needs the trampoline protocol:
//!
//! - every static `return_call` target, and
//! - every function whose funcref can flow to a `return_call_ref` — i.e.
//!   every `RefFunc` target and every declared table element (a
//!   conservative superset of the dynamic `return_call_ref` targets).
//!
//! Everything else calls and is called plainly. Functions that *contain*
//! tail calls are not necessarily in the set (a self-tail-recursive
//! function compiles its self-call to a loop internally); membership is
//! about being *targeted*.

use std::collections::BTreeSet;

use portal_pc_waffle::{EntityRef, Func, FuncDecl, Module, Operator, Terminator, ValueDef};

/// Compute the set of functions that must be reachable through the
/// trampoline protocol.
pub fn tail_callable_set(module: &Module<'_>) -> BTreeSet<Func> {
    let mut set = BTreeSet::new();
    for decl in module.funcs.entries() {
        let FuncDecl::Body(_, _, body) = decl.1 else {
            continue;
        };
        for def in body.blocks.entries() {
            for inst in &def.1.insts {
                if let ValueDef::Operator(Operator::RefFunc { func_index }, _, _) =
                    &body.values[inst.value]
                {
                    set.insert(*func_index);
                }
            }
            if let Terminator::ReturnCall { func, .. } = &def.1.terminator.terminator {
                set.insert(*func);
            }
        }
    }
    for table in module.tables.entries() {
        if let Some(elems) = &table.1.func_elements {
            for &f in elems {
                if f != Func::invalid() {
                    set.insert(f);
                }
            }
        }
    }
    set
}
