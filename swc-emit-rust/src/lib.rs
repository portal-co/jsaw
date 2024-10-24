use id_arena::Id;
use proc_macro2::TokenStream;
use quasiquote::quasiquote;
use quote::{format_ident, quote};
use swc_tac::{TBlock, TCfg};
use syn::{Ident, Path};

pub struct Opts {
    pub rt_path: Path,
}
pub fn emit(opts: &Opts, x: &TCfg, entry: Id<TBlock>) -> TokenStream {
    let states = x.blocks.iter().map(|a| k(a.0)).collect::<Vec<_>>();
    let ids = x.regs.map.keys().map(i).collect::<Vec<_>>();
    let root = &opts.rt_path;
    let blocks = x.blocks.iter().map(|(a, b)| {
        let catch = match &b.catch {
            swc_tac::TCatch::Throw => quote! {
                return e.throw()
            },
            swc_tac::TCatch::Jump { pat, k: k2 } => {
                let k2 = k(*k2);
                let val = i(pat);
                quote! {
                    state = S::#k2;
                    #val = o;
                }
            }
        };
        let term = match &b.term {
            swc_tac::TTerm::Return(ident) => todo!(),
            swc_tac::TTerm::Throw(ident) => quasiquote! {
                Err(#{i(ident)}.error())
            },
            swc_tac::TTerm::Jmp(id) => quasiquote! {
                Ok(#{k(*id)})
            },
            swc_tac::TTerm::CondJmp {
                cond,
                if_true,
                if_false,
            } => quasiquote! {
                Ok(match #{i(cond)}.truthy(){
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
        let stmts = b.stmts.iter().map(|(a,b)|quasiquote!{

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
    let entry = k(entry);
    quote! {
        enum S{
            #(#states),*
        }
        let mut state = #entry;
        #(let mut #ids = #root::O::default());*
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
pub fn i(i: &swc_ecma_ast::Ident) -> Ident {
    format_ident!("v{}_{}", i.sym.to_string(), i.ctxt.as_u32())
}
