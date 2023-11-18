//! A procedural macro for creating a Builder object for any struct.
//! This crate is meant to be used by the `simple-builder` crate and is not meant for direct consumption.

use proc_macro2::{self, Span, TokenStream};
use quote::quote;
use std::vec::Vec;
use syn::{
    parse_macro_input, DeriveInput, Field, Fields, GenericArgument, Ident, Path, PathArguments,
    Type, Attribute,
};

/// Macro that derives a Builder object for any given struct. E.g. `SomeType` -> `SomeTypeBuilder`.
///
/// Simple-Builder takes ownership of inputs and stores them in an `Option<T>` for each field. Fields
/// that are marked `#[builder(required)]` will be part of the `new()` call so they're guaranteed
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
///     pub coffee: i64, // coffee is required, and therefore not Option<T>
///     pub toast: Option<i64>,
///     pub eggs: Option<i64>,
///     pub bacon: Option<i64>,
/// }
///
/// pub fn main() {
///     let desired_breakfast = Breakfast {
///         coffee: 1,
///         toast: None,
///         eggs: Some(3),
///         bacon: Some(2),
///     };
///
///     // semantically equivalent to `Breakfast::builder(16)`
///     let mut builder = BreakfastBuilder::new(16);
///
///     let breakfast = builder.eggs(3).bacon(2).build();
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

    let builder_setter_methods: TokenStream = named_fields
        .iter()
        .map(|field| {
            let field_ident = &field.ident;
            let field_has_ident = Ident::new(
                &(String::from("has_") + &field_ident.clone().unwrap().to_string()),
                Span::call_site(),
            );
            let field_ty = &field.ty;

            let type_of_option = extract_type_from_option(field_ty);

            quote! {
                pub fn #field_ident(&mut self, #field_ident: #type_of_option) -> &mut Self {
                    self.#field_ident = ::std::option::Option::Some(#field_ident);
                    self
                }

                pub fn #field_has_ident(&mut self) -> bool {
                    self.#field_ident.is_some()
                }
            }
        })
        .collect();

    let builder_new_fields: TokenStream = named_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            if has_attr(field, "default") {
                return quote! {
                    #ident: ::std::option::Option::Some(Default::default()),
                };
            }
            quote! {
                #ident: ::std::option::Option::None,
            }
        })
        .collect();

    let builder_fields: TokenStream = named_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let ty = &field.ty;
            if type_is_option(ty) {
                quote! {
                    #ident: #ty,
                }
            } else {
                quote! {
                    #ident: ::std::option::Option<#ty>,
                }
            }
        })
        .collect();

    let build_fn_struct_fields: TokenStream = named_fields
        .iter()
        .map(|field| {
            let ident = &field.ident;

            if type_is_option(&field.ty) {
                quote! {
                    #ident: self.#ident.take(),
                }
            } else {
                quote! {
                    #ident: self.#ident.take().unwrap(),
                }
            }
        })
        .collect();

    let build_required_checks: TokenStream = named_fields
        .iter()
        .filter_map(|field| {
            if !type_is_option(&field.ty) {
                let ident = &field.ident;
                let ident_str = ident.as_ref().unwrap().to_string();
                Some(quote! {
                    if self.#ident.is_none() { errors.push(String::from(#ident_str)); }
                })
            } else {
                None
            }
        })
        .collect();

    let struct_impl = quote! {
        impl #impl_generics #ident #ty_generics #where_clause {
            pub fn builder() -> #builder_ident #ty_generics {
                #builder_ident {
                    #builder_new_fields
                }
            }
        }
    };

    let builder_struct = quote! {
        #vis struct #builder_ident #ty_generics #where_clause {
            #builder_fields
        }

        impl #impl_generics #builder_ident #ty_generics #where_clause {
            pub fn new() -> #builder_ident #ty_generics {
                #builder_ident {
                    #builder_new_fields
                }
            }

            pub fn build(&mut self) -> std::result::Result<#ident #ty_generics, ::simple_builder::SimpleBuilderError> {
                let mut errors = vec![];
                #build_required_checks
                if !errors.is_empty() {
                    return std::result::Result::Err(::simple_builder::SimpleBuilderError::BuildError(errors));
                }

                std::result::Result::Ok(#ident {
                    #build_fn_struct_fields
                })
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

fn path_is_option(path: &Path) -> bool {
    path.leading_colon.is_none()
        && path.segments.len() == 1
        && path.segments.iter().next().unwrap().ident == "Option"
}

fn type_is_option(ty: &Type) -> bool {
    match ty {
        Type::Path(type_path) if type_path.qself.is_none() && path_is_option(&type_path.path) => {
            let type_params: &PathArguments = &(type_path.path.segments.first().unwrap()).arguments;

            if let PathArguments::AngleBracketed(params) = type_params {
                return matches!(params.args.first().unwrap(), GenericArgument::Type(_ty));
            } else {
                return false;
            }
        }
        _ => false,
    }
}

fn extract_type_from_option(ty: &Type) -> &Type {
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
        _ => ty,
    }
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