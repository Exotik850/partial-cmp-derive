//! `PartialEq` and enum equality expansion using shared common helpers.

use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::expand::common::{
    FieldAccess, build_explicit_order_eq_checks, build_implicit_eq_checks, chain_eq_checks,
    generate_bindings, generate_named_bindings,
};
use crate::types::{OrdDerive, OrdVariant};

/// Expands the `PartialEq` implementation for the given input type.
pub fn expand_partial_eq(
    input: &OrdDerive,
    name: &Ident,
    impl_generics: &syn::ImplGenerics,
    ty_generics: &syn::TypeGenerics,
    where_clause: Option<&syn::WhereClause>,
) -> TokenStream {
    let eq_body = match &input.data {
        darling::ast::Data::Struct(fields) => {
            if let Some(ref order_list) = input.field_order {
                chain_eq_checks(build_explicit_order_eq_checks(order_list, &fields.fields))
            } else {
                chain_eq_checks(build_implicit_eq_checks(&fields.fields))
            }
        }
        darling::ast::Data::Enum(variants) => expand_enum_eq(name, variants),
    };

    quote! {
        #[automatically_derived]
        impl #impl_generics ::core::cmp::PartialEq for #name #ty_generics #where_clause {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                #eq_body
            }
        }
    }
}

/// Builds equality for enums by matching on variants and comparing fields.
pub fn expand_enum_eq(enum_name: &Ident, variants: &[OrdVariant]) -> TokenStream {
    if variants.is_empty() {
        return quote! { true };
    }

    let match_arms = variants.iter().map(|v| build_variant_eq_arm(enum_name, v));

    quote! {
        match (self, other) {
            #(#match_arms,)*
            _ => false,
        }
    }
}

/// Builds a single match arm for enum equality, comparing non-skipped fields.
fn build_variant_eq_arm(enum_name: &Ident, variant: &OrdVariant) -> TokenStream {
    let (self_pat, other_pat, bindings) = build_variant_patterns(enum_name, variant);
    let checks = variant
        .fields
        .iter()
        .zip(bindings.iter())
        .filter(|(f, _)| !f.skip.is_present())
        .map(|(field, (self_id, other_id))| {
            crate::expand::common::build_eq_check(
                field,
                &FieldAccess::Bindings { self_id, other_id },
            )
        });
    let eq_expr = chain_eq_checks(checks);
    quote! { (#self_pat, #other_pat) => { #eq_expr } }
}

/// Builds self/other patterns and binding pairs for a variant.
fn build_variant_patterns(
    enum_name: &Ident,
    variant: &OrdVariant,
) -> (TokenStream, TokenStream, Vec<(Ident, Ident)>) {
    let variant_name = &variant.ident;
    let fields = &variant.fields;

    if fields.is_empty() {
        return (
            quote! { #enum_name::#variant_name },
            quote! { #enum_name::#variant_name },
            Vec::new(),
        );
    }

    let is_tuple = fields.iter().next().is_some_and(|f| f.ident.is_none());

    if is_tuple {
        build_tuple_patterns(enum_name, variant_name, fields.len())
    } else {
        let field_idents: Vec<_> = fields.iter().filter_map(|f| f.ident.as_ref()).collect();
        build_named_patterns(enum_name, variant_name, &field_idents)
    }
}

fn build_tuple_patterns(
    enum_name: &Ident,
    variant_name: &Ident,
    field_count: usize,
) -> (TokenStream, TokenStream, Vec<(Ident, Ident)>) {
    let self_bindings = generate_bindings("self", field_count);
    let other_bindings = generate_bindings("other", field_count);

    let self_pattern = quote! { #enum_name::#variant_name(#(#self_bindings),*) };
    let other_pattern = quote! { #enum_name::#variant_name(#(#other_bindings),*) };

    let bindings: Vec<_> = self_bindings.into_iter().zip(other_bindings).collect();

    (self_pattern, other_pattern, bindings)
}

fn build_named_patterns(
    enum_name: &Ident,
    variant_name: &Ident,
    field_idents: &[&Ident],
) -> (TokenStream, TokenStream, Vec<(Ident, Ident)>) {
    let self_bindings = generate_named_bindings("self", field_idents);
    let other_bindings = generate_named_bindings("other", field_idents);

    let self_renames = field_idents
        .iter()
        .zip(&self_bindings)
        .map(|(orig, binding)| quote! { #orig: #binding });
    let other_renames = field_idents
        .iter()
        .zip(&other_bindings)
        .map(|(orig, binding)| quote! { #orig: #binding });

    let self_pattern = quote! { #enum_name::#variant_name { #(#self_renames),* } };
    let other_pattern = quote! { #enum_name::#variant_name { #(#other_renames),* } };

    let bindings: Vec<_> = self_bindings.into_iter().zip(other_bindings).collect();

    (self_pattern, other_pattern, bindings)
}
