//! Keyword argument parsing for function-like procedural macros.
#![deny(missing_docs)]
pub use proc_macro_kwargs_derive::MacroKeywordArgs;

pub mod args;
pub mod parse;

pub use args::MacroKeywordArgs;
pub use parse::MacroArg;

/// Combine multiple `syn` errors into a single error struct
///
/// Panics if the specified vector is empty.
///
/// NOTE: This is an internal implementation detail
#[doc(hidden)]
pub fn combine_errors(errors: Vec<syn::Error>) -> syn::Error {
    let mut iter = errors.into_iter();
    let mut error = iter.next().expect("empty Vec");
    for extra in iter {
        error.combine(extra);
    }
    error
}
