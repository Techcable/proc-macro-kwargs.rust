use proc_macro::TokenStream as RawTokenStream;
use syn::{parse_macro_input, DeriveInput};

pub(crate) mod kwargs;


#[proc_macro_derive(MacroKeywordArgs, attributes(kwarg))]
pub fn derive_keyword_args(input: RawTokenStream) -> RawTokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match self::kwargs::run_derive(&input) {
        Ok(res) => res.into(),
        Err(e) => e.into_compile_error().into()
    }
}