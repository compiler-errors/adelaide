use std::collections::HashSet;

use proc_macro2::{Ident, Span, TokenStream};
use syn::{Attribute, Fields, Lit, LitStr, Meta, MetaNameValue};
use synstructure::{decl_derive, quote, Structure};

decl_derive!([PrettyPrint, attributes(plain)] => prettyprint_derive);
decl_derive!([Diagnostic, attributes(message, note, span)] => diagnostic_derive);
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
    let lookup_fn = Ident::new(
        &format!("lookup_intern_{}", s.ast().ident.to_string().to_lowercase()),
        Span::call_site(),
    );
    let intern_fn = Ident::new(
        &format!("intern_{}", s.ast().ident.to_string().to_lowercase()),
        Span::call_site(),
    );

    s.gen_impl(quote! {
        gen impl ::adelaide::util::Lookup for @Self {
            fn lookup(id: ::adelaide::util::Id<Self>, ctx: &dyn ::adelaide::ctx::AdelaideContext) -> ::std::sync::Arc<Self> {
                ::adelaide::ctx::AdelaideContext::#lookup_fn(ctx, id)
            }

            fn intern_self(self: ::std::sync::Arc<Self>, ctx: &dyn ::adelaide::ctx::AdelaideContext) -> ::adelaide::util::Id<Self> {
                ::adelaide::ctx::AdelaideContext::#intern_fn(ctx, self)
            }
        }
    })
}

fn parse_message(message: LitStr) -> (LitStr, HashSet<Ident>) {
    let mut m: &str = &message.value();

    let mut idents = HashSet::new();
    let mut out = String::new();

    while !m.is_empty() {
        if let Some(i) = m.find('{') {
            out += &m[..i];
            m = &m[i + 1..];

            if &m[0..2] == "{" {
                out += "{{";
            } else if let Some(j) = m.find('}') {
                idents.insert(Ident::new(&m[0..j], Span::call_site()));

                out += "{";
                out += &m[0..j];
                out += ":?}";

                m = &m[j + 1..];
            } else {
                panic!("{}", "Unterminated `{{`");
            }
        } else {
            out += m;
            m = "";
        }
    }

    (LitStr::new(&out, message.span()), idents)
}

fn is_message_attr(attr: &&Attribute) -> bool {
    attr.path == syn::parse_str("message").expect("Can't parse message")
}

fn is_span_attr(attr: &&Attribute) -> bool {
    attr.path == syn::parse_str("span").expect("Can't parse span")
}

fn is_note_attr(attr: &&Attribute) -> bool {
    attr.path == syn::parse_str("note").expect("Can't parse note")
}

fn diagnostic_derive(s: Structure) -> TokenStream {
    let variants = s.each_variant(|v| {
        let why = if let Some(message_attr) = v.ast().attrs.iter().find(is_message_attr) {
            match message_attr.parse_meta().expect("Can't parse meta") {
                Meta::NameValue(MetaNameValue {
                    lit: Lit::Str(message),
                    ..
                }) => {
                    let (msg, attrs) = parse_message(message);

                    let mut args = quote! {,};
                    for attr in attrs {
                        let attr_binding = &v
                            .bindings()
                            .iter()
                            .find(|b| b.ast().ident.as_ref() == Some(&attr))
                            .unwrap_or_else(|| panic!("No such attr {}", attr))
                            .binding;
                        args.extend(quote! {
                            #attr = ::adelaide::util::Pretty(#attr_binding, ctx) ,
                        });
                    }

                    quote! {
                        format!(#msg #args)
                    }
                },
                _ => panic!("Expected `#[message = \"...\"]`"),
            }
        } else {
            panic!("Requires a message attribute");
        };

        let mut spans = quote! {};
        let mut notes = quote! {};

        for b in v.bindings() {
            let binding = &b.binding;

            if let Some(span_attr) = b.ast().attrs.iter().find(is_span_attr) {
                match span_attr.parse_meta().expect("Can't parse meta") {
                    Meta::Path(_) => {
                        spans.extend(quote! { #binding.into_label() , });
                    },
                    Meta::NameValue(MetaNameValue {
                        lit: Lit::Str(message),
                        ..
                    }) => {
                        let (msg, attrs) = parse_message(message);

                        let mut args = quote! {,};
                        for attr in attrs {
                            let attr_binding = &v
                                .bindings()
                                .iter()
                                .find(|b| b.ast().ident.as_ref() == Some(&attr))
                                .unwrap_or_else(|| panic!("No such attr {}", attr))
                                .binding;
                            args.extend(
                                quote! { #attr = ::adelaide::util::Pretty(#attr_binding, ctx) , },
                            );
                        }

                        spans.extend(quote! {
                            #binding.into_label().with_message(format!(#msg #args)) ,
                        });
                    },
                    _ => panic!("Expected `$[span]` or `#[span = \"...\"]`"),
                }
            }
        }

        for note_attr in v.ast().attrs.iter().filter(is_note_attr) {
            match note_attr.parse_meta().expect("Can't parse meta") {
                Meta::NameValue(MetaNameValue {
                    lit: Lit::Str(message),
                    ..
                }) => {
                    let (msg, attrs) = parse_message(message);

                    let mut args = quote! {,};
                    for attr in attrs {
                        let attr_binding = &v
                            .bindings()
                            .iter()
                            .find(|b| b.ast().ident.as_ref() == Some(&attr))
                            .unwrap_or_else(|| panic!("No such attr {}", attr))
                            .binding;
                        args.extend(quote! {
                            #attr = ::adelaide::util::Pretty(#attr_binding, ctx) ,
                        });
                    }

                    notes.extend(quote! {
                        format!(#msg #args) ,
                    });
                },
                _ => panic!("Expected `#[note = \"...\"]`"),
            }
        }

        let out = quote! {
            ::codespan_reporting::diagnostic::Diagnostic::error()
                .with_message(#why)
                .with_labels(vec![#spans])
                .with_notes(vec![#notes])
        };

        out
    });

    s.gen_impl(quote! {
        gen impl ::adelaide::util::IntoDiagnostic for @Self {
            fn into_diagnostic(self, ctx: &dyn ::adelaide::ctx::AdelaideContext)
                -> ::codespan_reporting::diagnostic::Diagnostic<::adelaide::util::Id<::adelaide::file::AFile>> {
                match self {
                    #variants
                }
            }
        }
    })
}
