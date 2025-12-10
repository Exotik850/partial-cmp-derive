//! Code generation for the `PartialCmpDerive` macro.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::types::{
    FieldOrderList, NoneOrder, OrdDerive, OrdField, OrdVariant, SortOrder, TraitConfig,
};

//=============================================================================
// Field Access Abstraction
//=============================================================================

/// Represents how to access a field for comparison.
enum FieldAccess<'a> {
    /// Access via `self.field` and `other.field`
    StructField(&'a TokenStream),
    /// Access via bound identifiers
    Bindings {
        self_id: &'a Ident,
        other_id: &'a Ident,
    },
}

impl FieldAccess<'_> {
    fn self_expr(&self) -> TokenStream {
        match self {
            Self::StructField(access) => quote! { self.#access },
            Self::Bindings { self_id, .. } => quote! { #self_id },
        }
    }

    fn other_expr(&self) -> TokenStream {
        match self {
            Self::StructField(access) => quote! { other.#access },
            Self::Bindings { other_id, .. } => quote! { #other_id },
        }
    }

    fn as_refs(&self) -> (TokenStream, TokenStream) {
        match self {
            Self::StructField(access) => (quote! { &self.#access }, quote! { &other.#access }),
            Self::Bindings { self_id, other_id } => (quote! { #self_id }, quote! { #other_id }),
        }
    }
}

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
// Common Helpers
//=============================================================================

/// Builds field access token for a field at the given index.
fn field_access(field: &OrdField, index: usize) -> TokenStream {
    if let Some(ref ident) = field.ident {
        quote! { #ident }
    } else {
        let idx = syn::Index::from(index);
        quote! { #idx }
    }
}

/// Collects fields sorted by priority, filtering out skipped fields.
fn sorted_field_indices(fields: &[OrdField]) -> Vec<(usize, &OrdField)> {
    let mut indexed: Vec<_> = fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !f.skip.is_present())
        .collect();

    indexed.sort_by(|(idx_a, a), (idx_b, b)| {
        a.effective_priority()
            .cmp(&b.effective_priority())
            .then(idx_a.cmp(idx_b))
    });

    indexed
}

/// Generates binding identifiers for a variant's fields.
fn generate_bindings(prefix: &str, count: usize) -> Vec<Ident> {
    (0..count)
        .map(|i| format_ident!("__{prefix}_{i}"))
        .collect()
}

/// Generates binding identifiers for named fields.
fn generate_named_bindings(prefix: &str, idents: &[&Ident]) -> Vec<Ident> {
    idents
        .iter()
        .map(|i| format_ident!("__{prefix}_{i}"))
        .collect()
}

//=============================================================================
// Unified Field Comparison Builders
//=============================================================================

/// Builds an equality check for a single field.
fn build_eq_check(field: &OrdField, access: &FieldAccess) -> TokenStream {
    let (self_ref, other_ref) = access.as_refs();

    if let Some(ref key) = field.key {
        let path = key.as_ref();
        quote! { #path(#self_ref) == #path(#other_ref) }
    } else if field.none_order.is_some() {
        build_option_eq(access)
    } else {
        let self_expr = access.self_expr();
        let other_expr = access.other_expr();
        quote! { #self_expr == #other_expr }
    }
}

/// Builds a comparison expression for a single field.
fn build_cmp_check(field: &OrdField, access: &FieldAccess, order: SortOrder) -> TokenStream {
    let (self_ref, other_ref) = access.as_refs();

    let base_cmp = if let Some(ref key) = field.key {
        let path = key.as_ref();
        quote! { ::core::cmp::Ord::cmp(&#path(#self_ref), &#path(#other_ref)) }
    } else if let Some(none_order) = field.none_order {
        build_option_cmp(access, none_order)
    } else {
        quote! { ::core::cmp::Ord::cmp(#self_ref, #other_ref) }
    };

    match order {
        SortOrder::Asc => base_cmp,
        SortOrder::Desc => quote! { #base_cmp.reverse() },
    }
}

/// Builds Option equality check.
fn build_option_eq(access: &FieldAccess) -> TokenStream {
    let self_expr = access.self_expr();
    let other_expr = access.other_expr();
    quote! {
        match (&#self_expr, &#other_expr) {
            (::core::option::Option::None, ::core::option::Option::None) => true,
            (::core::option::Option::Some(a), ::core::option::Option::Some(b)) => a == b,
            _ => false,
        }
    }
}

/// Builds Option comparison with custom None ordering.
fn build_option_cmp(access: &FieldAccess, none_order: NoneOrder) -> TokenStream {
    let self_expr = access.self_expr();
    let other_expr = access.other_expr();

    let (none_some, some_none) = match none_order {
        NoneOrder::First => (
            quote! { ::core::cmp::Ordering::Less },
            quote! { ::core::cmp::Ordering::Greater },
        ),
        NoneOrder::Last => (
            quote! { ::core::cmp::Ordering::Greater },
            quote! { ::core::cmp::Ordering::Less },
        ),
    };

    quote! {
        match (&#self_expr, &#other_expr) {
            (::core::option::Option::None, ::core::option::Option::None) => ::core::cmp::Ordering::Equal,
            (::core::option::Option::None, ::core::option::Option::Some(_)) => #none_some,
            (::core::option::Option::Some(_), ::core::option::Option::None) => #some_none,
            (::core::option::Option::Some(a), ::core::option::Option::Some(b)) => ::core::cmp::Ord::cmp(a, b),
        }
    }
}

/// Chains equality checks with short-circuit AND.
fn chain_eq_checks(checks: impl IntoIterator<Item = TokenStream>) -> TokenStream {
    let mut iter = checks.into_iter();
    if let Some(first) = iter.next() {
        iter.fold(first, |acc, check| quote! { #acc && #check })
    } else {
        quote! { true }
    }
}

/// Chains comparisons with early return on non-Equal.
fn chain_cmp_checks(comparisons: impl IntoIterator<Item = TokenStream>) -> TokenStream {
    let mut iter = comparisons.into_iter();
    let Some(first) = iter.next() else {
        return quote! { ::core::cmp::Ordering::Equal };
    };
    iter.fold(first, |acc, cmp| {
        quote! {
            match #acc {
                ::core::cmp::Ordering::Equal => #cmp,
                other => other,
            }
        }
    })
}
// fn chain_cmp_checks(comparisons: &[TokenStream]) -> TokenStream {
//     match comparisons.len() {
//         0 => quote! { ::core::cmp::Ordering::Equal },
//         1 => comparisons[0].clone(),
//         _ => {
//             let mut result = comparisons.last().unwrap().clone();
//             for cmp in comparisons.iter().rev().skip(1) {
//                 result = quote! {
//                     match #cmp {
//                         ::core::cmp::Ordering::Equal => #result,
//                         other => other,
//                     }
//                 };
//             }
//             result
//         }
//     }
// }

//=============================================================================
// PartialEq Implementation
//=============================================================================

fn expand_partial_eq(
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

fn build_explicit_order_eq_checks(
    order_list: &FieldOrderList,
    fields: &[OrdField],
) -> impl Iterator<Item = TokenStream> {
    let field_map: std::collections::HashMap<Ident, (usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter_map(|(i, f)| f.ident.as_ref().map(|id| (id.clone(), (i, f))))
        .collect();

    order_list.0.iter().filter_map(move |entry| {
        field_map.get(&entry.ident).map(|&(idx, field)| {
            let access = field_access(field, idx);
            build_eq_check(field, &FieldAccess::StructField(&access))
        })
    })
}

fn build_implicit_eq_checks(fields: &[OrdField]) -> impl Iterator<Item = TokenStream> + '_ {
    fields.iter().enumerate().filter_map(move |(idx, field)| {
        if field.skip.is_present() {
            None
        } else {
            let access = field_access(field, idx);
            Some(build_eq_check(field, &FieldAccess::StructField(&access)))
        }
    })
}

fn expand_enum_eq(enum_name: &Ident, variants: &[OrdVariant]) -> TokenStream {
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

fn build_variant_eq_arm(enum_name: &Ident, variant: &OrdVariant) -> TokenStream {
    let (self_pat, other_pat, bindings) = build_variant_patterns(enum_name, variant);
    let checks = variant
        .fields
        .iter()
        .zip(bindings.iter())
        .filter(|(f, _)| !f.skip.is_present())
        .map(|(field, (self_id, other_id))| {
            build_eq_check(field, &FieldAccess::Bindings { self_id, other_id })
        });
    let eq_expr = chain_eq_checks(checks);
    quote! { (#self_pat, #other_pat) => { #eq_expr } }
}

//=============================================================================
// Hash Implementation
//=============================================================================

fn expand_hash(
    input: &OrdDerive,
    name: &Ident,
    impl_generics: &syn::ImplGenerics,
    ty_generics: &syn::TypeGenerics,
    where_clause: Option<&syn::WhereClause>,
    config: &TraitConfig,
) -> TokenStream {
    let hash_body = match &input.data {
        darling::ast::Data::Struct(fields) => {
            if let Some(ref order_list) = input.field_order {
                build_explicit_order_hash(order_list, &fields.fields, config)
            } else {
                build_implicit_hash(&fields.fields, config)
            }
        }
        darling::ast::Data::Enum(variants) => expand_enum_hash(name, variants, config),
    };

    quote! {
        #[automatically_derived]
        impl #impl_generics ::core::hash::Hash for #name #ty_generics #where_clause {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                #hash_body
            }
        }
    }
}

/// Builds hash contribution for a single field.
fn build_hash_contribution(
    field: &OrdField,
    access: &FieldAccess,
    _config: &TraitConfig,
) -> TokenStream {
    let (self_ref, _) = access.as_refs();

    if let Some(ref key) = field.key {
        let path = key.as_ref();
        quote! { ::core::hash::Hash::hash(&#path(#self_ref), state); }
    } else if field.none_order.is_some() {
        build_option_hash(access)
    } else {
        quote! { ::core::hash::Hash::hash(#self_ref, state); }
    }
}

/// Builds Option hash with consistent handling.
fn build_option_hash(access: &FieldAccess) -> TokenStream {
    let self_expr = access.self_expr();
    quote! {
        match &#self_expr {
            ::core::option::Option::None => {
                ::core::hash::Hash::hash(&0u8, state);
            }
            ::core::option::Option::Some(__val) => {
                ::core::hash::Hash::hash(&1u8, state);
                ::core::hash::Hash::hash(__val, state);
            }
        }
    }
}

fn build_explicit_order_hash(
    order_list: &FieldOrderList,
    fields: &[OrdField],
    config: &TraitConfig,
) -> TokenStream {
    let field_map: std::collections::HashMap<Ident, (usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter_map(|(i, f)| f.ident.as_ref().map(|id| (id.clone(), (i, f))))
        .collect();

    let hash_stmts: Vec<_> = order_list
        .0
        .iter()
        .filter_map(|entry| {
            field_map.get(&entry.ident).map(|&(idx, field)| {
                let access = field_access(field, idx);
                build_hash_contribution(field, &FieldAccess::StructField(&access), config)
            })
        })
        .collect();

    quote! { #(#hash_stmts)* }
}

fn build_implicit_hash(fields: &[OrdField], config: &TraitConfig) -> TokenStream {
    let hash_stmts: Vec<_> = fields
        .iter()
        .enumerate()
        .filter_map(|(idx, field)| {
            if field.skip.is_present() {
                None
            } else {
                let access = field_access(field, idx);
                Some(build_hash_contribution(
                    field,
                    &FieldAccess::StructField(&access),
                    config,
                ))
            }
        })
        .collect();

    quote! { #(#hash_stmts)* }
}

fn expand_enum_hash(
    enum_name: &Ident,
    variants: &[OrdVariant],
    config: &TraitConfig,
) -> TokenStream {
    if variants.is_empty() {
        return quote! {};
    }

    let match_arms: Vec<_> = variants
        .iter()
        .map(|v| build_variant_hash_arm(enum_name, v, config))
        .collect();

    quote! {
        ::core::hash::Hash::hash(&::core::mem::discriminant(self), state);
        match self {
            #(#match_arms,)*
        }
    }
}

fn build_variant_hash_arm(
    enum_name: &Ident,
    variant: &OrdVariant,
    config: &TraitConfig,
) -> TokenStream {
    let variant_name = &variant.ident;
    let fields = &variant.fields;

    if fields.is_empty() {
        return quote! { #enum_name::#variant_name => {} };
    }

    let is_tuple = fields.iter().next().is_some_and(|f| f.ident.is_none());

    if is_tuple {
        let bindings = generate_bindings("self", fields.len());
        let pattern = quote! { #enum_name::#variant_name(#(#bindings),*) };

        let hash_stmts: Vec<_> = fields
            .iter()
            .zip(bindings.iter())
            .filter(|(f, _)| !f.skip.is_present())
            .map(|(field, binding)| {
                let access = FieldAccess::Bindings {
                    self_id: binding,
                    other_id: binding, // Not used for hashing
                };
                build_hash_contribution(field, &access, config)
            })
            .collect();

        quote! { #pattern => { #(#hash_stmts)* } }
    } else {
        let field_idents: Vec<_> = fields.iter().filter_map(|f| f.ident.as_ref()).collect();
        let bindings = generate_named_bindings("self", &field_idents);

        let renames = field_idents
            .iter()
            .zip(&bindings)
            .map(|(orig, binding)| quote! { #orig: #binding });

        let pattern = quote! { #enum_name::#variant_name { #(#renames),* } };

        let hash_stmts: Vec<_> = fields
            .iter()
            .zip(bindings.iter())
            .filter(|(f, _)| !f.skip.is_present())
            .map(|(field, binding)| {
                let access = FieldAccess::Bindings {
                    self_id: binding,
                    other_id: binding, // Not used for hashing
                };
                build_hash_contribution(field, &access, config)
            })
            .collect();

        quote! { #pattern => { #(#hash_stmts)* } }
    }
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
// Struct Comparison
//=============================================================================

fn build_explicit_order_cmp_checks(
    order_list: &FieldOrderList,
    fields: &[OrdField],
) -> impl Iterator<Item = TokenStream> {
    let field_map: std::collections::HashMap<Ident, (usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter_map(|(i, f)| f.ident.as_ref().map(|id| (id.clone(), (i, f))))
        .collect();

    order_list.0.iter().filter_map(move |entry| {
        field_map.get(&entry.ident).map(|&(idx, field)| {
            let access = field_access(field, idx);
            build_cmp_check(field, &FieldAccess::StructField(&access), entry.order)
        })
    })
}

fn build_implicit_cmp_checks(fields: &[OrdField]) -> impl Iterator<Item = TokenStream> + '_ {
    let sorted = sorted_field_indices(fields);
    sorted.into_iter().map(|(idx, field)| {
        let access = field_access(field, idx);
        build_cmp_check(
            field,
            &FieldAccess::StructField(&access),
            field.effective_order(),
        )
    })
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

fn compute_variant_ranks(variants: &[OrdVariant]) -> std::collections::HashMap<String, usize> {
    let mut indexed: Vec<_> = variants.iter().enumerate().collect();
    indexed.sort_by_key(|(idx, v)| v.effective_rank(*idx));

    indexed
        .iter()
        .enumerate()
        .map(|(rank, (_, v))| (v.ident.to_string(), rank))
        .collect()
}

fn build_variant_cmp_arm(enum_name: &Ident, variant: &OrdVariant) -> TokenStream {
    let (self_pat, other_pat, bindings) = build_variant_patterns(enum_name, variant);

    let comparisons = build_binding_cmp_checks(&variant.fields.fields, &bindings);
    let cmp_expr = chain_cmp_checks(comparisons);

    quote! { (#self_pat, #other_pat) => { #cmp_expr } }
}

fn build_binding_cmp_checks(fields: &[OrdField], bindings: &[(Ident, Ident)]) -> Vec<TokenStream> {
    let sorted = sorted_field_indices(fields);

    sorted
        .into_iter()
        .map(|(idx, field)| {
            let (ref self_id, ref other_id) = bindings[idx];
            build_cmp_check(
                field,
                &FieldAccess::Bindings { self_id, other_id },
                field.effective_order(),
            )
        })
        .collect()
}

fn build_discriminant_fn(
    enum_name: &Ident,
    variants: &[OrdVariant],
    variant_ranks: &std::collections::HashMap<String, usize>,
) -> TokenStream {
    let arms: Vec<_> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            let rank = variant_ranks[&variant_name.to_string()];
            let pattern = build_wildcard_pattern(enum_name, v);
            quote! { #pattern => #rank }
        })
        .collect();

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

//=============================================================================
// Variant Pattern Building
//=============================================================================

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
