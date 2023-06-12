use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Expr, ExprLit, ExprPath, Item, ItemFn, ItemMod, Lit, Path};

#[proc_macro_attribute]
pub fn boa_fn(_attrs: TokenStream, input: TokenStream) -> TokenStream {
    let fun = parse_macro_input!(input as ItemFn);
    let name = &fun.sig.ident;
    let arg_count = fun.sig.inputs.len();
    let ac = 0..arg_count;
    let ac2 = ac.clone();
    quote! {
        pub fn #name(_: &::boa_engine::JsValue,
            args: &[::boa_engine::JsValue],
            ctx: &mut ::boa_engine::Context) -> ::boa_engine::JsResult<boa_engine::JsValue> {
            use ::boa_engine::builtins::JsArgs;
            use self::helper::{AsJsResult, GetAs};
            #fun

            let ____args = [
                #(args.get_or_undefined(#ac).get_as(ctx)?),*
            ];
            #name (#(____args[#ac2]),*).as_js_result()
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn boa_mod(_attrs: TokenStream, input: TokenStream) -> TokenStream {
    let modu = parse_macro_input!(input as ItemMod);
    let (vals, keys): (Vec<_>, Vec<_>) = modu
        .content
        .as_ref()
        .unwrap()
        .1
        .iter()
        .flat_map(|it| match it {
            Item::Fn(f) => Some((
                (&f.sig.ident, f.sig.inputs.len()),
                f.attrs
                    .iter()
                    .filter_map(|a| {
                        a.path()
                            .is_ident("boa_fn")
                            .then(|| a.parse_args::<Expr>().ok())
                    })
                    .flatten()
                    .filter_map(|e| match e {
                        Expr::Assign(ea) => match *ea.left {
                            Expr::Path(ExprPath {
                                path: Path { segments, .. },
                                ..
                            }) => match *ea.right {
                                Expr::Lit(ExprLit {
                                    lit: Lit::Str(ls), ..
                                }) => Some((
                                    segments.into_iter().next()?.ident.to_string(),
                                    ls.value(),
                                )),
                                _ => None,
                            },
                            _ => None,
                        },
                        _ => None,
                    })
                    .find_map(|(attr, name)| (attr == "jsname").then_some(name))
                    .expect("boa function requires a jsname"),
            )),
            _ => None,
        })
        .unzip();
    let (vals, lens): (Vec<_>, Vec<_>) = vals.into_iter().unzip();
    let new_function = quote! {
        pub fn get_default_context() -> ::boa_engine::Context {
            static FUNCTIONS_MAP: ::phf::Map<&'static str, (::boa_engine::builtins::function::NativeFunctionSignature, usize)> = ::phf::phf_map! {
                #(#keys => (#vals, #lens)),*
            };
            let mut ctx = ::boa_engine::Context::default();
            for (k, (v, l)) in &FUNCTIONS_MAP {
                ctx.register_global_function(k, *l, *v);
            }
            ctx
        }
    };
    let mut modu = modu;
    modu.content
        .as_mut()
        .unwrap()
        .1
        .push(Item::Verbatim(new_function));
    quote! {
        #modu
    }
    .into()
}
