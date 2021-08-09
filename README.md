proc-macro-kwargs
==================
Keyword argument parsing for function-like procedural macros (Rust).

## Example
````rust
example_macro!(
    name => bar,
    foo => i32
);
````

And here is the corresponding code in the proc macro:


````rust
#[derive(MacroKeywordArgs)]
struct MacroArgs {
    name: Ident,
    #[kwarg(optional)]
    optional: Option<syn::Expr>,
    #[kwarg(rename = "foo")
    tp: Type
}

````

See the [tests](./tests/kwargs.rs) for more detailed examples
