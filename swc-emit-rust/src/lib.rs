use std::collections::BTreeSet;

use id_arena::Id;
use proc_macro2::TokenStream;
use quasiquote::quasiquote;
use quote::{format_ident, quote};
use swc_tac::{Item, LId, TBlock, TCfg, TFunc};
use syn::{Ident, Path};

pub struct Opts {
    pub rt_path: Path,
}
pub fn emit(opts: &Opts, x: &TFunc, old: &BTreeSet<swc_ecma_ast::Id>) -> TokenStream {
    let root = &opts.rt_path;
    let states = x.cfg.blocks.iter().map(|a| k(a.0)).collect::<Vec<_>>();
    let ids = x
        .cfg
        .regs
        .map
        .keys()
        .filter(|a| !old.contains(a))
        .map(i)
        .collect::<Vec<_>>();
    let vals = x
        .cfg
        .regs
        .map
        .keys()
        .filter(|a| !old.contains(a))
        .map(|v| {
            match x
                .params
                .iter()
                .enumerate()
                .find_map(|a| if a.1 == v { Some(a.0) } else { None })
            {
                Some(i) => quasiquote!(arguments[#{i}].clone()),
                None => quasiquote!(#root::OCell::default()),
            }
        })
        .collect::<Vec<_>>();
    let total = x
        .cfg
        .regs
        .map
        .keys()
        .chain(old.iter())
        .cloned()
        .collect::<BTreeSet<_>>();
    let blocks = x.cfg.blocks.iter().map(|(a, b)| {
        let catch = match &b.catch {
            swc_tac::TCatch::Throw => quote! {
                return e.throw()
            },
            swc_tac::TCatch::Jump { pat, k: k2 } => {
                let k2 = k(*k2);
                let val = i(pat);
                quote! {
                    state = S::#k2;
                    *#val.lock().unwrap() = o;
                }
            }
        };
        let term = match &b.term {
            swc_tac::TTerm::Return(ident) => todo!(),
            swc_tac::TTerm::Throw(ident) => quasiquote! {
                Err(#{i(ident)}.lock().unwrap().error())
            },
            swc_tac::TTerm::Jmp(id) => quasiquote! {
                Ok(#{k(*id)})
            },
            swc_tac::TTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => quasiquote! {
                Ok(match #{i(cond)}.lock().unwrap().truthy(){
                    true => #{k(*if_true)},
                    false => #{k(*if_false)}
                })
            },
            swc_tac::TTerm::Switch { x, blocks, default } => quasiquote! {
                let i = #{i(x)}.clone();
                Ok([#{
                    let params = blocks.iter().map(|(a,b)|quasiquote!{
                        (#{i(a)}.clone(),#{k(*b)})
                    });
                    quote! {
                        #(#params),*
                    }
                }].into_iter().find_map(|a|if a.0 == i{
                    Some(a.1)
                }else{
                    None
                }).unwrap_or(S::#{k(*default)}))
            },
            swc_tac::TTerm::Default => todo!(),
        };
        let stmts = b.stmts.iter().map(|(a, b)| {
            quasiquote! {
                let val = #{stmt(opts,b,&total)};
                #{lid(opts,a,quote! {val})};
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
    quote! {
        enum S{
            #(#states),*
        }
        let mut state = #entry;
        #(let mut #ids = #vals);*
        loop{
            match state{
                #(S::#states => #blocks),*
            }
        }
    }
}
pub fn k(i: Id<TBlock>) -> Ident {
    format_ident!("S{}", i.index())
}
pub fn i(i: &swc_ecma_ast::Id) -> Ident {
    format_ident!("v{}_{}", i.0.to_string(), i.1.as_u32())
}
fn stmt(opts: &Opts, stmt: &Item, total: &BTreeSet<swc_ecma_ast::Id>) -> TokenStream {
    let root = &opts.rt_path;
    match stmt {
        Item::Just { id } => quasiquote!(#{i(id)}.clone()),
        Item::Bin { left, right, op } => todo!(),
        Item::Un { arg, op } => todo!(),
        Item::Mem { obj, mem } => {
            quasiquote!(match #{i(obj)}.lock().unwrap().get(&*#{i(mem)}.lock().unwrap()){
                Ok(a) => a,
                Err(_) => #root::synth::throw_reference_error()?,
            })
        }
        Item::Func { func } => {
            // let func: anyhow::Result<swc_cfg::Func> = func.clone().try_into();
            // let func: anyhow::Result<swc_tac::TFunc> = func.and_then(|a| a.try_into());
            let mut func = func.clone();
            func.cfg.update();
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
                    ::alloc::boxed::Box::new((move||#k)())
                })
            };
        }
        Item::Lit { lit } => todo!(),
        Item::Call { r#fn, member, args } => {
            let args = args.iter().map(i);
            let obj = quasiquote!{#{i(r#fn)}.lock().unwrap()};
            let get = match member{
                Some(a) => quasiquote!{
                    match obj.get(&*#{i(a)}.lock().unwrap()){
                        Ok(a) => a,
                        Err(_) => #root::synth::throw_reference_error()?,   
                    }
                },
                None =>quote! {obj},
            };
            quasiquote!(
                match #obj{
                    obj => #get.call(obj,#[#(#args),*])?
                }
            )
        }
        Item::Obj { members } => todo!(),
        Item::Arr { members } => {
            let members = members.iter().map(i).collect::<Vec<_>>();
            return quasiquote! {
                #root::O::array([#(#members),*].into_iter())
            };
        }
        Item::Yield { value, delegate } => todo!(),
        Item::Await { value } => todo!(),
        Item::Undef => {
            return quasiquote!{
                #root::O::default()
            };
        },
    }
}
fn lid(opts: &Opts, lid: &LId, val: TokenStream) -> TokenStream {
    match lid {
        LId::Id { id } => quasiquote!(*#{i(id)}.lock().unwrap() = #val;),
        LId::Member { obj, mem } => {
            quasiquote!(#{i(obj)}.lock().unwrap().set(&*#{i(mem)}.lock().unwrap(),&#val))
        }
    }
}
