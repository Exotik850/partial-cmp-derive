/// Top-level expansion and comparison-related code generation for the `PartialCmp` derive.
///
/// This module contains:
/// - `expand_derive`: orchestrates generation of `PartialEq` (delegated), Eq, `PartialOrd`, Ord, and Hash (delegated)
/// - `PartialOrd` and `Ord` expansion
/// - Struct and enum comparison helpers used by the above
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::expand::common::{
    FieldAccess, build_explicit_order_cmp_checks, build_implicit_cmp_checks, chain_cmp_checks,
    generate_bindings, generate_named_bindings,
};
use crate::expand::eq::expand_partial_eq;
use crate::expand::hash::expand_hash;
use crate::types::{OrdDerive, OrdField, OrdVariant};

//=============================================================================
// Main Entry Point
//=============================================================================

/// Expands the derive macro into trait implementations.
pub fn expand_derive(input: &OrdDerive) -> TokenStream {
    let config = input.trait_config();
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut output = TokenStream::new();

    if config.partial_eq {
        let partial_eq_impl =
            expand_partial_eq(input, name, &impl_generics, &ty_generics, where_clause);
        output.extend(partial_eq_impl);
    }

    if config.eq {
        output.extend(quote! {
            #[automatically_derived]
            impl #impl_generics ::core::cmp::Eq for #name #ty_generics #where_clause {}
        });
    }

    if config.partial_ord {
        let partial_ord_impl =
            expand_partial_ord(input, name, &impl_generics, &ty_generics, where_clause);
        output.extend(partial_ord_impl);
    }

    if config.ord {
        let ord_impl = expand_ord(input, name, &impl_generics, &ty_generics, where_clause);
        output.extend(ord_impl);
    }

    if config.hash {
        let hash_impl = expand_hash(
            input,
            name,
            &impl_generics,
            &ty_generics,
            where_clause,
            &config,
        );
        output.extend(hash_impl);
    }

    output
}

//=============================================================================
// PartialOrd Implementation
//=============================================================================

fn expand_partial_ord(
    input: &OrdDerive,
    name: &Ident,
    impl_generics: &syn::ImplGenerics,
    ty_generics: &syn::TypeGenerics,
    where_clause: Option<&syn::WhereClause>,
) -> TokenStream {
    let config = input.trait_config();

    // If Ord will be generated, implement PartialOrd in terms of Ord for consistency.
    if config.ord {
        return quote! {
            #[automatically_derived]
            impl #impl_generics ::core::cmp::PartialOrd for #name #ty_generics #where_clause {
                #[inline]
                fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                    ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
                }
            }
        };
    }

    let cmp_body = build_cmp_body(input);
    let final_cmp = apply_reverse(input, cmp_body);

    quote! {
        #[automatically_derived]
        impl #impl_generics ::core::cmp::PartialOrd for #name #ty_generics #where_clause {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(#final_cmp)
            }
        }
    }
}

//=============================================================================
// Ord Implementation
//=============================================================================

fn expand_ord(
    input: &OrdDerive,
    name: &Ident,
    impl_generics: &syn::ImplGenerics,
    ty_generics: &syn::TypeGenerics,
    where_clause: Option<&syn::WhereClause>,
) -> TokenStream {
    let cmp_body = build_cmp_body(input);
    let final_cmp = apply_reverse(input, cmp_body);

    quote! {
        #[automatically_derived]
        impl #impl_generics ::core::cmp::Ord for #name #ty_generics #where_clause {
            #[inline]
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                #final_cmp
            }
        }
    }
}

//=============================================================================
// Comparison Body Builders
//=============================================================================

fn build_cmp_body(input: &OrdDerive) -> TokenStream {
    match &input.data {
        darling::ast::Data::Struct(fields) => {
            if let Some(ref order_list) = input.field_order {
                chain_cmp_checks(build_explicit_order_cmp_checks(order_list, &fields.fields))
            } else {
                chain_cmp_checks(build_implicit_cmp_checks(&fields.fields))
            }
        }
        darling::ast::Data::Enum(variants) => expand_enum_cmp(input, variants),
    }
}

fn apply_reverse(input: &OrdDerive, cmp_body: TokenStream) -> TokenStream {
    if input.reverse.is_present() {
        quote! { { let result = { #cmp_body }; result.reverse() } }
    } else {
        cmp_body
    }
}

//=============================================================================
// Enum Comparison
//=============================================================================

fn expand_enum_cmp(input: &OrdDerive, variants: &[OrdVariant]) -> TokenStream {
    if variants.is_empty() {
        return quote! { ::core::cmp::Ordering::Equal };
    }

    let enum_name = &input.ident;
    let variant_ranks = compute_variant_ranks(variants);
    let match_arms = variants.iter().map(|v| build_variant_cmp_arm(enum_name, v));
    let discriminant_fn = build_discriminant_fn(enum_name, variants, &variant_ranks);

    quote! {
        {
            #discriminant_fn

            let self_disc = __discriminant(self);
            let other_disc = __discriminant(other);

            if self_disc != other_disc {
                ::core::cmp::Ord::cmp(&self_disc, &other_disc)
            } else {
                match (self, other) {
                    #(#match_arms,)*
                    _ => ::core::cmp::Ordering::Equal,
                }
            }
        }
    }
}

fn compute_variant_ranks(variants: &[OrdVariant]) -> std::collections::HashMap<Ident, usize> {
    let mut indexed: Vec<_> = variants.iter().enumerate().collect();
    indexed.sort_by_key(|(idx, v)| v.effective_rank(*idx));

    indexed
        .iter()
        .enumerate()
        .map(|(rank, (_, v))| (v.ident.clone(), rank))
        .collect()
}

fn build_variant_cmp_arm(enum_name: &Ident, variant: &OrdVariant) -> TokenStream {
    let (self_pat, other_pat, bindings) = build_variant_patterns(enum_name, variant);

    let comparisons = build_binding_cmp_checks(&variant.fields.fields, &bindings);
    let cmp_expr = chain_cmp_checks(comparisons);

    quote! { (#self_pat, #other_pat) => { #cmp_expr } }
}

fn build_binding_cmp_checks(fields: &[OrdField], bindings: &[(Ident, Ident)]) -> Vec<TokenStream> {
    let sorted = crate::expand::common::sorted_field_indices(fields);

    sorted
        .into_iter()
        .map(|(idx, field)| {
            let (ref self_id, ref other_id) = bindings[idx];
            crate::expand::common::build_cmp_check(
                field,
                &FieldAccess::Bindings { self_id, other_id },
                field.effective_order(),
            )
        })
        .collect()
}

//=============================================================================
// Variant Pattern Building
//=============================================================================

fn build_discriminant_fn(
    enum_name: &Ident,
    variants: &[OrdVariant],
    variant_ranks: &std::collections::HashMap<Ident, usize>,
) -> TokenStream {
    let arms = variants.iter().map(|v| {
        let variant_name = &v.ident;
        let rank = variant_ranks[variant_name];
        let pattern = build_wildcard_pattern(enum_name, v);
        quote! { #pattern => #rank }
    });

    quote! {
        #[inline]
        fn __discriminant(value: &#enum_name) -> usize {
            match value {
                #(#arms,)*
            }
        }
    }
}

fn build_wildcard_pattern(enum_name: &Ident, variant: &OrdVariant) -> TokenStream {
    let variant_name = &variant.ident;
    let field_count = variant.fields.len();

    if field_count == 0 {
        quote! { #enum_name::#variant_name }
    } else if variant
        .fields
        .iter()
        .next()
        .is_some_and(|f| f.ident.is_none())
    {
        let underscores: Vec<_> = (0..field_count).map(|_| quote! { _ }).collect();
        quote! { #enum_name::#variant_name(#(#underscores),*) }
    } else {
        quote! { #enum_name::#variant_name { .. } }
    }
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
