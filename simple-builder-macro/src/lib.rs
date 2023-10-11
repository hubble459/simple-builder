//! A procedural macro for creating a Builder object for any struct.
//! This crate is meant to be used by the `simple-builder` crate and is not meant for direct consumption.

use proc_macro2::{self, Span, TokenStream};
use quote::quote;
use std::vec::Vec;
use syn::{
    parse_macro_input, Attribute, DeriveInput, Field, Fields, GenericArgument, Ident, Path,
    PathArguments, Type,
};

/// Macro that derives a Builder object for any given struct. E.g. `SomeType` -> `SomeTypeBuilder`.
///
/// Simple-Builder takes ownership of inputs and stores them in `Option<T>` for each field. Fields that
/// are marked `#[builder(required)]` will be part of the `new()` call so they're guaranteed
/// to be set on the final object.
///
///
/// # Example: Builder on a Simple Object
/// ```
/// # use simple_builder_macro::Builder;
///
/// // Debug, PartialEq, Eq are only for assertions
/// #[derive(Debug, PartialEq, Eq, Builder)]
/// struct Breakfast {
///     #[builder(required)]
///     pub coffee_oz: i64, // coffee is required, and therefore not Option<T>
///     pub toast: Option<i64>,
///     pub eggs: Option<i64>,
///     pub bacon: Option<i64>,
/// }
///
/// pub fn main() {
///     let desired_breakfast = Breakfast {
///         coffee_oz: 16,
///         toast: None,
///         eggs: Some(2),
///         bacon: Some(2),
///     };
///
///     // semantically equivalent to `Breakfast::builder(16)`
///     let mut builder = BreakfastBuilder::new(16);
///
///     let breakfast = builder.eggs(2).bacon(2).build();
///
///     assert_eq!(desired_breakfast, breakfast);
/// }
/// ```
///
/// ## Attributes
/// Builder supports attributes under `#[builder(...)]` on individual fields to carry metadata.
/// At this time, the available attributes are:
/// - required -- marks a field as required, meaning it can be `T` instead of `Option<T>` on the struct
/// and will be an argument to the `StructBuilder::new()` or `Struct::builder()` methods.
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: DeriveInput = parse_macro_input!(input);
    let (vis, ident, generics) = (&ast.vis, &ast.ident, &ast.generics);
    let builder_ident = Ident::new(&(ident.to_string() + "Builder"), Span::call_site());

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields: &Fields = match ast.data {
        syn::Data::Struct(ref s) => &s.fields,
        _ => panic!("Can only derive Builder for structs."),
    };

    let named_fields: Vec<&Field> = fields
        .iter()
        .filter(|field| field.ident.is_some())
        .collect();

    let required_fields: Vec<&Field> = named_fields
        .iter()
        .filter_map(|field| {
            if has_attr(field, "required") {
                Some(*field)
            } else {
                None
            }
        })
        .collect();

    let optional_fields: Vec<&Field> = named_fields
        .iter()
        .filter_map(|field| {
            if has_attr(field, "required") {
                None
            } else {
                Some(*field)
            }
        })
        .collect();

    let builder_setter_methods: TokenStream = optional_fields
        .iter()
        .map(|field| {
            let field_ident = &field.ident;
            let field_ty = &field.ty;

            let type_of_option = extract_type_from_option(field_ty);

            quote! {
                pub fn #field_ident(&mut self, #field_ident: #type_of_option) -> &mut Self {
                    self.#field_ident = ::std::option::Option::Some(#field_ident);
                    self
                }
            }
        })
        .collect();

    let required_new_fields: TokenStream = required_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            quote! {
                #ident: ::std::option::Option::Some(#ident),
            }
        })
        .collect();

    let empty_new_fields: TokenStream = optional_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            quote! {
                #ident: None,
            }
        })
        .collect();

    let builder_required_fields: TokenStream = required_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let ty = &field.ty;
            quote! {
                #ident: ::std::option::Option<#ty>,
            }
        })
        .collect();

    let builder_optional_fields: TokenStream = optional_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let ty = &field.ty;
            quote! {
                #ident: #ty,
            }
        })
        .collect();

    let builder_struct_fields: TokenStream = builder_required_fields
        .into_iter()
        .chain(builder_optional_fields)
        .collect();

    let new_method_params: TokenStream = required_fields
        .iter()
        .map(|field| {
            let (arg, ty) = (&field.ident, &field.ty);
            quote! {
                #arg: #ty,
            }
        })
        .collect();

    let build_fn_struct_fields: TokenStream = named_fields
        .iter()
        .map(|field| {
            let is_required = has_attr(field, "required");

            let ident = &field.ident;

            if is_required {
                // .expect() should be possible only when build is called twice, since these are required private fields set by `new`
                quote! {
                    #ident: self.#ident.take().expect("Option must be Some(T) for required fields. Builder may have already been consumed by calling `build`"),
                }
            } else {
                quote! {
                    #ident: self.#ident.take(),
                }
            }
        })
        .collect();

    let struct_impl = quote! {
        impl #impl_generics #ident #ty_generics #where_clause {
            pub fn builder(#new_method_params) -> #builder_ident #ty_generics {
                #builder_ident {
                    #required_new_fields
                    #empty_new_fields
                }
            }
        }
    };

    let builder_struct = quote! {
        #vis struct #builder_ident #ty_generics #where_clause {
            #builder_struct_fields
        }

        impl #impl_generics #builder_ident #ty_generics #where_clause {

            pub fn new(#new_method_params) -> #builder_ident #ty_generics {
                #builder_ident {
                    #required_new_fields
                    #empty_new_fields
                }
            }

            pub fn build(&mut self) -> #ident #ty_generics {
                #ident {
                    #build_fn_struct_fields
                }
            }

            #builder_setter_methods
        }
    };

    let output = quote! {
        #struct_impl
        #builder_struct
    };

    output.into()
}

fn has_attr(field: &Field, attr: &'static str) -> bool {
    field.attrs.iter().any(|a| has_nested_attr(a, attr))
}

fn has_nested_attr(attr: &Attribute, name: &'static str) -> bool {
    let mut has_attr: bool = false;

    if attr.path().is_ident("builder") {
        attr.parse_nested_meta(|m| {
            if m.path.is_ident(name) {
                has_attr = true;
            }

            Ok(())
        })
        .expect("Parsing nested meta within #[builder(...)] failed.");
    }

    has_attr
}

fn extract_type_from_option(ty: &Type) -> &Type {
    fn path_is_option(path: &Path) -> bool {
        path.leading_colon.is_none()
            && path.segments.len() == 1
            && path.segments.iter().next().unwrap().ident == "Option"
    }

    match ty {
        Type::Path(type_path) if type_path.qself.is_none() && path_is_option(&type_path.path) => {
            let type_params: &PathArguments = &(type_path.path.segments.first().unwrap()).arguments;

            let generic_arg = match type_params {
                PathArguments::AngleBracketed(params) => params.args.first().unwrap(),
                _ => panic!("Could not find generic parameter in Option<...>"),
            };

            match generic_arg {
                GenericArgument::Type(ty) => ty,
                _ => panic!(
                    "Found something other than a type as a generic parameter to Option<...>"
                ),
            }
        }
        _ => panic!(
            "Struct fields must be of type Option<...>, or have #[builder(required)] attribute."
        ),
    }
}
