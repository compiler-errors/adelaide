use proc_macro2::{TokenStream, Ident, Span};
use syn::{Fields, Attribute};
use synstructure::{decl_derive, quote, Structure};

decl_derive!([PrettyPrint, attributes(plain)] => prettyprint_derive);
decl_derive!([Lookup] => lookup_derive);

fn prettyprint_derive(s: Structure) -> TokenStream {
    let variants = s.each_variant(|v| {
        let name = v.ast().ident;
        let name = if let Some(prefix) = v.prefix {
            format!("{}::{}", prefix, name)
        } else {
            name.to_string()
        };

        let debug_init = match &v.ast().fields {
            Fields::Named(_) => quote! { let mut h = f.debug_struct(#name); },
            Fields::Unnamed(_) => quote! { let mut h = f.debug_tuple(#name); },
            Fields::Unit => quote! { let mut h = f.debug_struct(#name); },
        };

        let fields = v.bindings().iter().map(|b| {
            let pretty = if b.ast().attrs.iter().any(is_plain_attr) {
                quote! {
                    #b
                }
            } else {
                quote! {
                    &::adelaide::util::Pretty(#b, ctx)
                }
            };

            if let Some(ident) = &b.ast().ident {
                let ident = ident.to_string();
                quote! {
                    h.field(#ident, #pretty);
                }
            } else {
                quote! {
                    h.field(#pretty);
                }
            }
        });

        quote! {
            #debug_init;
            #(#fields)*;
            h.finish()?;
        }
    });

    s.gen_impl(quote! {
        gen impl ::adelaide::util::PrettyPrint for @Self {
            fn fmt(&self, f: &mut ::core::fmt::Formatter, ctx: &dyn ::adelaide::ctx::AdelaideContext) -> ::core::fmt::Result {
                match self {
                    #variants
                }

                Ok(())
            }
        }
    })
}

fn is_plain_attr(attr: &Attribute) -> bool {
    attr.path == syn::parse_str("plain").unwrap()
}

fn lookup_derive(s: Structure) -> TokenStream {
    let lookup_fn = Ident::new(&format!("lookup_intern_{}", s.ast().ident.to_string().to_lowercase()), Span::call_site());
    let intern_fn = Ident::new(&format!("intern_{}", s.ast().ident.to_string().to_lowercase()), Span::call_site());

    s.gen_impl(quote! {
        gen impl ::adelaide::util::Lookup for @Self {
            fn lookup(id: ::adelaide::util::Id<Self>, ctx: &dyn ::adelaide::ctx::AdelaideContext) -> Arc<Self> {
                ::adelaide::ctx::AdelaideContext::#lookup_fn(ctx, id)
            }
        
            fn intern_self(self: Arc<Self>, ctx: &dyn ::adelaide::ctx::AdelaideContext) -> ::adelaide::util::Id<Self> {
                ::adelaide::ctx::AdelaideContext::#intern_fn(ctx, self)
            }
        }
    })
}