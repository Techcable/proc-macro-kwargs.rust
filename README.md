proc-macro-kwargs
==================
Straightforward argument parsing for procedural macros.

## Example
````
example_macro!(
    name => bar,
    foo => i32
);
````

And here is the corresponding code in the proc macro:


````
#[derive(MacroKeywordArgs)]
struct MacroArgs {
    name: Ident,
    #[kwarg(optional)]
    optional: Option<syn::Expr>,
    #[kwarg(rename = "foo")
    tp: Type
}

````
