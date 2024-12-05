use std::{collections::BTreeSet, iter::once};

use id_arena::Id;
use proc_macro2::{Span, TokenStream};
use quasiquote::quasiquote;
use quote::{format_ident, quote};
use swc_opt_ssa::{OptBlock, OptFunc, OptType, OptValueW};
use swc_ssa::{SBlock, SCatch, SFunc, SValue, SValueW};
use swc_tac::{Item, LId, TBlock, TCallee, TCfg, TFunc};
use syn::{Ident, Index, Path};
// pub mod wasm;

pub struct Opts {
    pub rt_path: Path,
}
pub fn rty(opts: &Opts, a: Option<OptType>) -> TokenStream {
    let root = &opts.rt_path;
    match a {
        Some(x) => match x {
            OptType::Number => quote! {
                f64
            },
            OptType::U32 { bits_usable } => quote! {
                u32
            },
            OptType::BigInt => quote! {
                #root::BigInt
            },
            OptType::U64 { bits_usable } => quote! {
                u64
            },
            OptType::Bool => quote! {
                bool
            },
            OptType::Array { elem_ty } => quasiquote! {
                #root::Vec<#{rty(opts, elem_ty.as_ref().clone())}>
            },
        },
        None => quote! {
            #root::O
        },
    }
}
pub fn emit(opts: &Opts, x: &OptFunc, old: &BTreeSet<swc_ecma_ast::Id>) -> TokenStream {
    let root = &opts.rt_path;
    let states = x.cfg.blocks.iter().map(|a| k(a.0)).collect::<Vec<_>>();
    let s2 = x.cfg.blocks.iter().map(|(a, b)| {
        let params = b.params.iter().map(|a| a.1.clone()).map(|p| rty(opts, p));
        quasiquote!(#{k(a)}((#(#params),*)))
    });
    let ids = x
        .cfg
        .decls
        .iter()
        .filter(|a| !old.contains(a))
        .map(i)
        .collect::<Vec<_>>();
    let vals = x
        .cfg
        .decls
        .iter()
        .filter(|a| !old.contains(a))
        .map(|v| quasiquote!(#root::OCell::default()))
        .collect::<Vec<_>>();
    let total = x
        .cfg
        .decls
        .iter()
        .chain(old.iter())
        .cloned()
        .collect::<BTreeSet<_>>();
    let blocks = x.cfg.blocks.iter().map(|(a, b)| {
        let catch = match &b.postcedent.catch {
            SCatch::Throw => quote! {
                return e.throw()
            },
            SCatch::Just { target } => {
                let k2 = k(target.block);
                // let val = i(pat);
                let args = once(quote! {o})
                    .chain(target.args.iter().map(|a| quasiquote!(#{si(*a)}.clone())));
                quote! {
                    state = S::#k2([#(#args),*])
                }
            }
        };
        let term = match &b.postcedent.term {
            swc_ssa::STerm::Return(ident) => todo!(),
            swc_ssa::STerm::Throw(ident) => quasiquote! {
                Err(#{si(*ident)}.error())
            },
            swc_ssa::STerm::Jmp(id) => {
                let args = id.args.iter().map(|a| quasiquote!(#{si(*a)}.clone()));
                quasiquote! {
                    Ok(S::#{k(id.block)}((#(#args),*)))
                }
            }
            swc_ssa::STerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => {
                let args_it = if_true.args.iter().map(|a| quasiquote!(#{si(*a)}.clone()));
                let args_if = if_false.args.iter().map(|a| quasiquote!(#{si(*a)}.clone()));
                quasiquote! {
                    Ok(match #{si(*cond)}.lock().unwrap().truthy(){
                        true => S::#{k(if_true.block)}((#(#args_it),*)),
                        false => S::#{k(if_false.block)}((#(#args_if),*))
                    })
                }
            }
            swc_ssa::STerm::Switch { x, blocks, default } => {
                let args = default.args.iter().map(|a| quasiquote!(#{si(*a)}.clone()));
                quasiquote! {
                    let i = #{si(*x)}.clone();
                    Ok([#{
                        let params = blocks.iter().map(|(a,b)|{
                            let args = b.args.iter().map(|a|quasiquote!(#{si(*a)}.clone()));
                            quasiquote!{
                            (#{si(*a)}.clone(),S::#{k(b.block)}((#(#args),*)))
                        }});
                        quote! {
                            #(#params),*
                        }
                    }].into_iter().find_map(|a|if a.0 == i{
                        Some(a.1)
                    }else{
                        None
                    }).unwrap_or(S::#{k(default.block)}(#(#args),*)))
                }
            }
            swc_ssa::STerm::Default => todo!(),
        };
        let stmts = b
            .params
            .iter()
            .map(|a| &a.0)
            .chain(b.insts.iter())
            .map(|(val)| {
                quasiquote! {
                    let #{si(*val)}: #{rty(opts, x.cfg.values[*val].ty(&x.cfg))} = #{v(opts,&x,*val,&total)};
                }
            });
        let body = quote! {#(#stmts);*};
        let body = quote! {
            #body;
            #term
        };
        quote! {
            match try{
                #body
            }{
                Ok(s) => state = s,
                Err(e) => match e.obj(){
                    None => return Err(e),
                    Some(o) => {
                        #catch
                    }
                }
            }
        }
    });
    let entry = k(x.entry);
    let args = x.cfg.blocks[x.entry].params.iter().enumerate().map(|a|a.0).map(|i|quasiquote!{
        arguments[#i].clone()
    });
    quote! {
        enum S{
            #(#s2),*
        }
        let mut state = S::#entry((#(#args),*));
        #(let mut #ids = #vals);*
        loop{
            match state{
                #(S::#states(params) => #blocks),*
            }
        }
    }
}
pub fn k(i: Id<OptBlock>) -> Ident {
    format_ident!("S{}", i.index())
}
pub fn i(i: &swc_ecma_ast::Id) -> Ident {
    format_ident!("v{}_{}", i.0.to_string(), i.1.as_u32())
}
pub fn si(a: Id<OptValueW>) -> Ident {
    format_ident!("s{}", a.index())
}
fn v(
    opts: &Opts,
    x: &OptFunc,
    val: Id<OptValueW>,
    total: &BTreeSet<swc_ecma_ast::Id>,
) -> TokenStream {
    match &x.cfg.values[val].0 {
        swc_opt_ssa::OptValue::Deopt(a) => quasiquote!{
            #{si(*a)}.clone().into()
        },
        swc_opt_ssa::OptValue::Assert { val, ty } => quasiquote!{
            #{si(*val)}.clone().try_into().unwrap()
        },
        swc_opt_ssa::OptValue::Emit { val, ty } => match val {
            SValue::Param { block, idx, ty } => quasiquote! {
                params.#{Index{span: Span::call_site(),index: *idx as u32}}.clone()
            },
            SValue::Item(item) => stmt(opts, item, total),
            SValue::Assign { target, val } => lid(opts, target, quasiquote!(#{si(*val)}.clone())),
            SValue::LoadId(j) => quasiquote! {
                #{i(j)}.lock().unwrap().clone()
            },
            SValue::StoreId { target, val } => quasiquote! {
                *#{i(target)}.lock().unwrap() = #{si(*val)}
            },
        },
    }
}
fn stmt(
    opts: &Opts,
    stmt: &Item<Id<OptValueW>>,
    total: &BTreeSet<swc_ecma_ast::Id>,
) -> TokenStream {
    let root = &opts.rt_path;
    match stmt {
        Item::Just { id } => quasiquote!(#{si(*id)}.clone()),
        Item::Bin { left, right, op } => quasiquote! {
            match #{si(*left)}.clone(){
                left => match #{si(*right)}.clone(){
                    right => (*left.#{format_ident!("BinaryOp_{op:?}")}(right))?
                }
            }
        },
        Item::Un { arg, op } => quasiquote! {
            match #{si(*arg)}.clone(){
                obj => (*obj.#{format_ident!("UnaryOp_{op:?}")}())?
            }
        },
        Item::Mem { obj, mem } => {
            quasiquote!(match #{si(*obj)}.get(&*#{si(*mem)}){
                Ok(a) => a,
                Err(_) => #root::synth::throw_reference_error()?,
            })
        }
        Item::Func { func } => {
            // let func: anyhow::Result<swc_cfg::Func> = func.clone().try_into();
            // let func: anyhow::Result<swc_tac::TFunc> = func.and_then(|a| a.try_into());
            let mut func = func.clone();
            func.cfg.update();
            let func: SFunc = func.try_into().unwrap();
            let func = func.try_into().unwrap();
            let k = emit(opts, &func, total);
            let total2 = total.iter().map(i).collect::<BTreeSet<_>>();
            return quasiquote! {
                #root::O::closure(&[#(#total2 .clone()),*],|rs,rsl,this,arguments, arguments_len|{
                    let rs = unsafe{
                        ::core::slice::from_raw_parts(rs,rsl)
                    };
                    let arguments = unsafe{
                        ::core::slice::from_raw_parts(arguments,arguments_len)
                    };
                    let Ok([#(#total2),*]) = rs.try_into() else{
                        unreachable!();
                    };
                    #(let #total2 = #total2.clone());*;
                    ::alloc::boxed::Box::new(try{
                        #k
                    })
                })
            };
        }
        Item::Lit { lit } => todo!(),
        Item::Call { callee, args } => {
            let args = args.iter().cloned().map(si);
            let obj = match callee {
                swc_tac::TCallee::Val(r#fn) => quasiquote! {#{si(*r#fn)}.clone()},
                swc_tac::TCallee::Member { r#fn, member } => quasiquote! {#{si(*r#fn)}.clone()},
                swc_tac::TCallee::Static(r#fn) => quasiquote! {#{i(r#fn)}.lock().unwrap().clone()},
            };
            let get = match callee {
                TCallee::Member { member, .. } => quasiquote! {item
                    match obj.get(&#{si(*member)}){
                        Ok(a) => a,
                        Err(_) => #root::synth::throw_reference_error()?,
                    }
                },
                _ => quote! {obj},
            };
            quasiquote!(
                match #obj{
                    obj => #get.call(obj,#[#(#args .clone()),*])?
                }
            )
        }
        Item::Obj { members } => todo!(),
        Item::Arr { members } => {
            let members = members.iter().cloned().map(si).collect::<Vec<_>>();
            return quasiquote! {
                #root::O::array([#(#members .clone()),*].into_iter())
            };
        }
        Item::Yield { value, delegate } => todo!(),
        Item::Await { value } => todo!(),
        Item::Undef => {
            return quasiquote! {
                #root::O::default()
            };
        }
    }
}
fn lid(opts: &Opts, lid: &LId<Id<OptValueW>>, val: TokenStream) -> TokenStream {
    match lid {
        LId::Id { id } => todo!(),
        LId::Member { obj, mem } => {
            quasiquote!(#{si(*obj)}.set(&#{si(*mem)}),&#val)
        }
    }
}
