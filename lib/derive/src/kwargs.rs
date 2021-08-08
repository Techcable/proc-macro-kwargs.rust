use syn::{
    DeriveInput, Error, Data, LitStr, Token, Fields,
    Attribute, spanned::Spanned,
    parse::{Parse, ParseStream}, 
};
use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned, format_ident};

pub fn run_derive(input: &DeriveInput) -> Result<TokenStream, syn::Error> {
    let s = match input.data {
        Data::Struct(ref s) => s,
        Data::Enum(ref e) => {
            return Err(Error::new(e.enum_token.span(), "Enums are currently unsupported"));
        },
        Data::Union(ref u) => {
            return Err(Error::new(u.union_token.span(), "Unions are unsupported"))
        }
    };
    let named_fields = match s.fields {
        Fields::Named(ref named) => named,
        Fields::Unnamed(ref s) => {
            return Err(Error::new(
                s.span(),
                "Unnamed fields are forbidden"
            ));
        },
        Fields::Unit => {
            return Err(Error::new(
                input.ident.span(),
                "Unit structs are forbidden"
            ))
        }
    };
    let original_ident = &input.ident;
    let original_vis = &input.vis;
    let id_enum_name = format_ident!("{}ArgId", input.ident); 
    let parsed_arg_name = format_ident!("{}ParsedArg", input.ident);
    let mut variant_names = Vec::new();
    let mut field_names = Vec::new();
    let mut field_declarations = Vec::new();
    let mut field_inits = Vec::new();
    let mut parsed_arg_types = Vec::new();
    let mut arg_name_strings = Vec::new();
    let mut parse_invocations = Vec::new();
    for field in named_fields.named.iter() {
        let attr = FieldAttrs::find_attr(&field.attrs)?
            .unwrap_or_default();
        let ident = field.ident.as_ref().unwrap();
        let arg_name = attr.rename.clone()
            .unwrap_or_else(|| ident.to_string());
        use heck::CamelCase;
        let variant_name = Ident::new(
            &ident.to_string().to_camel_case(),
            ident.span(),
        );
        variant_names.push(variant_name.clone());
        arg_name_strings.push(arg_name);
        parsed_arg_types.push(&field.ty);
        let field_ty = &field.ty;
        parse_invocations.push(quote_spanned!(
            field.ty.span() => <#field_ty as proc_macro_kwargs::MacroArg>::parse_macro_arg(stream)?
        ));
        field_names.push(ident.clone());
        /*
         * In order to produce good errors,
         * field initialization has three phases:
         * 1. Declaration
         * 2. Checking for errors (if any required args were missing)
         * 3. Struct Initialization
         *
         * Splitting it up allows us to combine multiple error
         * messages for missing required arguments.
         */
        let cast_failure = quote!(unreachable!(
            "got {:?} for {}", other.id(),
            stringify!(#variant_name))
        );
        if attr.optional {
            let default_val = quote_spanned!(
                field.ty.span() => <#field_ty as Default>::default()
            );
            field_declarations.push(quote! {
                let #ident: #field_ty = match argument_list
                    .take(#id_enum_name::#variant_name)
                    .map(|arg| arg.value) {
                    Some(#parsed_arg_name::#variant_name ( res )) => res,
                    Some(other) => #cast_failure,
                    None => #default_val
                };
            });
            field_inits.push(quote!(#ident));
        } else {
            field_declarations.push(quote! {
                let #ident: Option<#field_ty> = match argument_list.require(#id_enum_name::#variant_name).map(|arg| arg.value) {
                    Ok(#parsed_arg_name::#variant_name ( res ) ) => Some(res),
                    Ok(other) => #cast_failure,
                    Err(e) => {
                        missing_argument_errors.push(e);
                        None // temporary - will never be used
                    }
                };
            });
            field_inits.push(quote!(#ident.unwrap()))
        }
    }
    let (impl_generics, ty_generics, where_clause) = input.generics
        .split_for_impl();
    Ok(quote! {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        #[doc(hidden)]
        #original_vis enum #id_enum_name {
            #(#variant_names),*
        }
        impl proc_macro_kwargs::args::KeywordArgId for #id_enum_name {
            fn as_str(&self) -> &'_ str {
                match *self {
                    #(Self::#variant_names => #arg_name_strings),*
                }
            }
            fn from_name(name: &str) -> Option<Self> {
                match name {
                    #(#arg_name_strings => Some(Self::#variant_names),)*
                    _ => None
                }
            } 
        }
        #[doc(hidden)]
        #original_vis enum #parsed_arg_name {
            #(#variant_names ( #parsed_arg_types )),*
        }
        impl proc_macro_kwargs::args::ParsedArgValue<#id_enum_name> for #parsed_arg_name {
            fn id(&self) -> #id_enum_name {
                match *self {
                    #(#parsed_arg_name::#variant_names (_) => #id_enum_name::#variant_names),*
                }
            }
            fn parse_with_id(
                id: #id_enum_name ,
                id_span: proc_macro2::Span,
                stream: syn::parse::ParseStream,
            ) -> syn::Result<Self> {
                Ok(match id {
                    #(#id_enum_name::#variant_names => {
                        Self::#variant_names(#parse_invocations)
                    }),*
                })
            }
        }
        impl #impl_generics proc_macro_kwargs::MacroKeywordArgs
                for #original_ident #ty_generics #where_clause {
            type ArgId = #id_enum_name;
            type ParsedArg = #parsed_arg_name;
            fn from_keyword_args(mut argument_list: proc_macro_kwargs::args::ParsedKeywordArguments<Self>) -> syn::Result<Self> {
                #[allow(unused_imports)] // Possible if empty
                use proc_macro_kwargs::args::ParsedArgValue;
                let mut missing_argument_errors = Vec::new();
                #(#field_declarations)*
                if !missing_argument_errors.is_empty() {
                    return Err(proc_macro_kwargs::combine_errors(missing_argument_errors));
                }
                Ok(#original_ident {
                    #(#field_names: #field_inits),*
                })
            }
        }
        impl #impl_generics syn::parse::Parse
                for #original_ident #ty_generics #where_clause {
            fn parse(stream: syn::parse::ParseStream) -> syn::Result<Self> {
                Self::from_keyword_args(stream.parse()?)
            }
        }
        /// Parse as a nested value inside another set of arguments,
        /// by surrounding it with braces `{}`
        impl #impl_generics proc_macro_kwargs::MacroArg
                for #original_ident #ty_generics #where_clause {
            fn parse_macro_arg(stream: syn::parse::ParseStream) -> syn::Result<Self> {
                let content;
                syn::braced!(content in stream);
                Ok(content.parse()?)
            }
        }
    })
}

struct FieldAttrs {
    optional: bool,
    rename: Option<String>
}
impl Default for FieldAttrs {
    fn default() -> Self {
        FieldAttrs {
            optional: false,
            rename: None
        }
    }
}
impl FieldAttrs {
    fn find_attr(attrs: &[Attribute]) -> syn::Result<Option<Self>> {
        let mut res = None;
        for attr in attrs {
            if attr.path.is_ident("kwarg") {
                if res.is_some() {
                    return Err(Error::new(
                        attr.path.span(),
                        "Duplicate `kwarg` attributes"
                    ))
                }
                res = Some(attr.parse_args::<Self>()?);
            }
        }
        Ok(res)
    }
}
impl Parse for FieldAttrs {
    fn parse(stream: ParseStream) -> syn::Result<Self> {
        let mut res = Self::default();
        loop {
            let name: Ident = stream.parse()?;
            match &*name.to_string() {
                "optional" => {
                    if res.optional {
                        return Err(Error::new(
                            name.span(),
                            "Already specified `optional` attribute"
                        ));
                    }
                    res.optional = true;
                },
                "rename" => {
                    if res.rename.is_some() {
                        return Err(Error::new(
                            name.span(),
                            "Already specified `rename` option"
                        ))
                    }
                    stream.parse::<Token![=]>()?;
                    let renamed = stream.parse::<LitStr>()?;
                    res.rename = Some(renamed.value());
                },
                _ => {
                    return Err(Error::new(
                        name.span(),
                        "Unknown option name"
                    ))
                }
            }
            if stream.peek(Token![,]) {
                stream.parse::<Token![,]>()?;
                continue
            } else {
                break
            }
        }
        if !stream.is_empty() {
            return Err(stream.error("Unexpected token"))
        }
        Ok(res)
    }
}