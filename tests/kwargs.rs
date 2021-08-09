use proc_macro_kwargs::{MacroKeywordArgs, parse::NestedList};

#[derive(MacroKeywordArgs, Debug, PartialEq)]
pub struct ExampleArgs {
    first: usize,
    #[kwarg(rename = "foo")]
    second: String,
    #[kwarg(optional)]
    opt: bool
}

#[test]
fn basic_example() {
    assert_eq!(syn::parse_str::<ExampleArgs>(r##"first => 1,
        foo => "str"
    "##).unwrap(), ExampleArgs {
        first: 1,
        second: "str".into(),
        opt: false
    });
    assert_eq!(syn::parse_str::<ExampleArgs>(r##"first => 17,
        foo => "str2",
        opt => true,
    "##).unwrap(), ExampleArgs {
        first: 17,
        second: "str2".into(),
        opt: true
    });
}

#[derive(MacroKeywordArgs, Debug, PartialEq, Default)]
struct NestedArgs {
    one: bool,
    #[kwarg(optional, with_wrapper = "NestedList<u32>")]
    list: Vec<u32>,
    #[kwarg(optional)]
    nested: Option<ExampleArgs>
}

#[test]
fn nesting() {
    assert_eq!(syn::parse_str::<NestedArgs>(r##"one => false,
    "##).unwrap(), Default::default());
    assert_eq!(syn::parse_str::<NestedArgs>(r##"one => true,
        list => [1, 4, 7]
    "##).unwrap(), NestedArgs {
        one: true,
        list: vec![1, 4, 7],
        nested: None
    });
    assert_eq!(syn::parse_str::<NestedArgs>(r##"one => true,
        nested => {
            first => 12,
            foo => "nested-str",
        }
    "##).unwrap(), NestedArgs {
        one: true,
        list: Default::default(),
        nested: Some(ExampleArgs {
            first: 12,
            second: "nested-str".into(),
            opt: false
        })
    });
}