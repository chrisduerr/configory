//! Rustdoc config documentation generation.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse::{self, Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    Data, DataStruct, DeriveInput, Error, Expr, ExprLit, Field, Fields, Generics, Ident, Lit,
    LitStr, Meta, MetaNameValue, Token, parse_macro_input,
};

/// Error if the derive was used on an unsupported type.
const UNSUPPORTED_ERROR: &str =
    "Docgen must be specified on a struct with fields, or use #[docgen(doc_type = \"…\")";

/// Derive the doc generation macro.
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // Parse container attributes.
    let mut doc = String::new();
    let mut doc_type = None;
    for attr in input.attrs.iter() {
        if attr.path().is_ident("doc") {
            if let Meta::NameValue(MetaNameValue {
                value: Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }),
                ..
            }) = &attr.meta
            {
                doc.push_str(lit.value().trim());
                doc.push('\n');
            }
        } else if attr.path().is_ident("docgen") {
            match attr.parse_args::<Attr>() {
                Ok(attr) if attr.ident == "doc_type" => doc_type = attr.param,
                Ok(Attr { ident, .. }) => {
                    let msg = format!("Invalid attribute `{ident}`; expected `doc_type`");
                    return Error::new(attr.span(), msg).to_compile_error().into();
                },
                Err(err) => {
                    return Error::new(attr.span(), err.to_string()).to_compile_error().into();
                },
            }
        }
    }
    doc.truncate(doc.len().saturating_sub(1));

    match (input.data, doc_type) {
        (Data::Struct(DataStruct { fields: Fields::Named(fields), .. }), None) => {
            derive_struct(input.ident, input.generics, fields.named, doc).into()
        },
        (_, Some(doc_type)) => derive_leaf(input.ident, input.generics, doc_type).into(),
        _ => Error::new(input.ident.span(), UNSUPPORTED_ERROR).to_compile_error().into(),
    }
}

/// Derive doc generation for a struct.
pub fn derive_struct<T>(
    ident: Ident,
    generics: Generics,
    fields: Punctuated<Field, T>,
    doc: String,
) -> TokenStream2 {
    let (field_inserts, requires_default) = field_inserts(&fields);

    // Create default unless all fields have `#[docgen(default = "…")]` annotations.
    let create_default = if requires_default {
        quote! { let default = Self::default(); }
    } else {
        TokenStream2::new()
    };

    quote! {
        impl <#generics> configory::docgen::Docgen for #ident <#generics> {
            fn doc_type() -> configory::docgen::DocType {
                #create_default
                let mut table = configory::docgen::Table::new(#doc);
                #field_inserts
                configory::docgen::DocType::Table(table)
            }

            fn format(&self) -> String {
                String::new()
            }
        }
    }
}

/// Get insert statement for table fields.
fn field_inserts<T>(fields: &Punctuated<Field, T>) -> (TokenStream2, bool) {
    let mut stream = TokenStream2::default();
    let mut requires_default = false;
    'fields: for field in fields {
        let ident = field.ident.as_ref().expect("unreachable tuple struct");
        let literal = ident.to_string();
        let ty = &field.ty;

        // Extract field attributes.
        let mut doc = String::new();
        let mut doc_type = None;
        let mut flatten = false;
        let mut default = None;
        for attr in field.attrs.iter() {
            let path = attr.path();
            if path.is_ident("doc") {
                if let Meta::NameValue(MetaNameValue {
                    value: Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }),
                    ..
                }) = &attr.meta
                {
                    doc.push_str(lit.value().trim());
                    doc.push('\n');
                }
            } else if path.is_ident("docgen") {
                // Parse comma separated list of field attributes.
                let attrs = match attr.parse_args::<Attrs>() {
                    Ok(Attrs { attrs }) => attrs,
                    Err(err) => {
                        let err = Error::new(attr.span(), err.to_string()).to_compile_error();
                        return (err, false);
                    },
                };

                // Handle attributes, erroring if any are unknown/typos.
                for parsed_attr in attrs.into_iter() {
                    match (&*parsed_attr.ident, parsed_attr.param) {
                        ("doc_type", Some(param)) => doc_type = Some(param),
                        ("default", Some(param)) => default = Some(quote! { #param.into() }),
                        ("flatten", None) => flatten = true,
                        ("skip", None) => continue 'fields,
                        (ident @ ("doc_type" | "default"), None) => {
                            let msg = format!("Missing parameter for attribute `{ident}`");
                            return (Error::new(attr.span(), msg).to_compile_error(), false);
                        },
                        (ident @ ("skip" | "flatten"), Some(_)) => {
                            let msg = format!("Unexpected parameter for attribute `{ident}`");
                            return (Error::new(attr.span(), msg).to_compile_error(), false);
                        },
                        (ident, _) => {
                            let msg = format!(
                                "Invalid attribute `{ident}`; expected `doc_type`, `default`, \
                                 `flatten`, or `skip`"
                            );
                            return (Error::new(attr.span(), msg).to_compile_error(), false);
                        },
                    }
                }
            }
        }
        requires_default |= default.is_none();
        let default = default.unwrap_or(quote! { default.#ident.format().into() });
        doc.truncate(doc.len().saturating_sub(1));

        match doc_type {
            Some(doc_type) => {
                stream.extend(quote! {
                    let leaf = configory::docgen::Leaf::new(#doc_type);
                    let doc_type = configory::docgen::DocType::Leaf(leaf);
                    table.push(configory::docgen::Field {
                        doc_type,
                        default: #default,
                        ident: #literal.into(),
                        doc: #doc.into(),
                    });
                });
            },
            None if flatten => {
                stream.extend(quote! {
                    let doc_type = <#ty>::doc_type();
                    match doc_type {
                        configory::docgen::DocType::Leaf(_) => {
                            table.push(configory::docgen::Field {
                                doc_type,
                                ident: #literal.into(),
                                default: #default,
                                doc: #doc.into(),
                            });
                        },
                        configory::docgen::DocType::Table(flat_table) => {
                            table.fields.extend(flat_table.fields);
                        },
                    }
                });
            },
            None => {
                stream.extend(quote! {
                    table.push(configory::docgen::Field {
                        doc_type: <#ty>::doc_type(),
                        ident: #literal.into(),
                        default: #default,
                        doc: #doc.into(),
                    });
                });
            },
        }
    }
    (stream, requires_default)
}

/// Derive doc generation for a leaf type.
pub fn derive_leaf(ident: Ident, generics: Generics, doc_type: LitStr) -> TokenStream2 {
    quote! {
        impl <#generics> configory::docgen::Docgen for #ident <#generics> {
            fn doc_type() -> configory::docgen::DocType {
                let leaf = configory::docgen::Leaf::new(#doc_type);
                configory::docgen::DocType::Leaf(leaf)
            }

            fn format(&self) -> String {
                format!("{:?}", self)
            }
        }
    }
}

/// Simple K/V attribute.
struct Attr {
    ident: String,
    param: Option<LitStr>,
}

impl Parse for Attr {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        let ident = input.parse::<Ident>()?.to_string();
        let param = input.parse::<Token![=]>().and_then(|_| input.parse()).ok();
        Ok(Self { ident, param })
    }
}

/// List of comma-separated K/V attributes.
struct Attrs {
    attrs: Punctuated<Attr, Token![,]>,
}

impl Parse for Attrs {
    fn parse(input: ParseStream<'_>) -> parse::Result<Self> {
        let attrs = input.parse_terminated(Attr::parse, Token![,])?;
        Ok(Self { attrs })
    }
}
