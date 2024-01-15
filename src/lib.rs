//! Keyword argument parsing for function-like procedural macros.
#![deny(missing_docs)]
#![feature(trait_alias)]
pub use proc_macro_kwargs_derive::MacroKeywordArgs;

pub mod parse;
pub mod args;

pub use parse::{MacroArg};
pub use args::{MacroKeywordArgs};

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
