//! Derive macros for the configory crate.

use proc_macro::TokenStream;

mod docgen;

#[proc_macro_derive(Docgen, attributes(docgen))]
pub fn derive_docgen(input: TokenStream) -> TokenStream {
    docgen::derive(input)
}
