//! Code generation for the `PartialCmpDerive` macro.

use darling::{Result, ast::Fields};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::types::{FieldOrderList, NoneOrder, OrdDerive, OrdField, OrdVariant, SortOrder};

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
pub fn expand_derive(input: &OrdDerive) -> Result<TokenStream> {
    let config = input.trait_config();
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut output = TokenStream::new();

    if config.partial_eq {
        let partial_eq_impl =
            expand_partial_eq(input, name, &impl_generics, &ty_generics, where_clause)?;
        output.extend(partial_eq_impl);
    }

    if config.eq {
        output.extend(quote! {
            impl #impl_generics ::core::cmp::Eq for #name #ty_generics #where_clause {}
        });
    }

    if config.partial_ord {
        let partial_ord_impl =
            expand_partial_ord(input, name, &impl_generics, &ty_generics, where_clause)?;
        output.extend(partial_ord_impl);
    }

    if config.ord {
        let ord_impl = expand_ord(input, name, &impl_generics, &ty_generics, where_clause)?;
        output.extend(ord_impl);
    }

    Ok(output)
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

    if let Some(ref eq_with) = field.eq_with {
        let path = eq_with.as_ref();
        quote! { #path(#self_ref, #other_ref) }
    } else if let Some(ref compare_with) = field.compare_with {
        let path = compare_with.as_ref();
        quote! { #path(#self_ref, #other_ref) == ::core::cmp::Ordering::Equal }
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

    let base_cmp = if let Some(ref compare_with) = field.compare_with {
        let path = compare_with.as_ref();
        quote! { #path(#self_ref, #other_ref) }
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
fn chain_eq_checks(checks: &[TokenStream]) -> TokenStream {
    match checks.len() {
        0 => quote! { true },
        1 => checks[0].clone(),
        _ => {
            let first = &checks[0];
            let rest = &checks[1..];
            quote! { #first #(&& #rest)* }
        }
    }
}

/// Chains comparisons with early return on non-Equal.
fn chain_cmp_checks(comparisons: &[TokenStream]) -> TokenStream {
    match comparisons.len() {
        0 => quote! { ::core::cmp::Ordering::Equal },
        1 => comparisons[0].clone(),
        _ => {
            let mut result = comparisons.last().unwrap().clone();
            for cmp in comparisons.iter().rev().skip(1) {
                result = quote! {
                    match #cmp {
                        ::core::cmp::Ordering::Equal => #result,
                        other => other,
                    }
                };
            }
            result
        }
    }
}

//=============================================================================
// PartialEq Implementation
//=============================================================================

fn expand_partial_eq(
    input: &OrdDerive,
    name: &Ident,
    impl_generics: &syn::ImplGenerics,
    ty_generics: &syn::TypeGenerics,
    where_clause: Option<&syn::WhereClause>,
) -> Result<TokenStream> {
    let eq_body = match &input.data {
        darling::ast::Data::Struct(fields) => expand_struct_eq(input, fields)?,
        darling::ast::Data::Enum(variants) => expand_enum_eq(name, variants)?,
    };

    Ok(quote! {
        impl #impl_generics ::core::cmp::PartialEq for #name #ty_generics #where_clause {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                #eq_body
            }
        }
    })
}

fn expand_struct_eq(input: &OrdDerive, fields: &Fields<OrdField>) -> Result<TokenStream> {
    let checks = build_struct_eq_checks(input, fields)?;
    Ok(chain_eq_checks(&checks))
}

fn build_struct_eq_checks(
    input: &OrdDerive,
    fields: &Fields<OrdField>,
) -> Result<Vec<TokenStream>> {
    let fields_vec: Vec<_> = fields.iter().cloned().collect();

    if let Some(ref order_list) = input.field_order {
        build_explicit_order_eq_checks(order_list, &fields_vec)
    } else {
        build_implicit_eq_checks(&fields_vec)
    }
}

fn build_explicit_order_eq_checks(
    order_list: &FieldOrderList,
    fields: &[OrdField],
) -> Result<Vec<TokenStream>> {
    let field_map: std::collections::HashMap<String, (usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter_map(|(i, f)| f.ident.as_ref().map(|id| (id.to_string(), (i, f))))
        .collect();

    let mut checks = Vec::new();
    for entry in &order_list.0 {
        if let Some(&(idx, field)) = field_map.get(&entry.ident.to_string()) {
            let access = field_access(field, idx);
            checks.push(build_eq_check(field, &FieldAccess::StructField(&access)));
        }
    }
    Ok(checks)
}

fn build_implicit_eq_checks(fields: &[OrdField]) -> Result<Vec<TokenStream>> {
    let mut checks = Vec::new();
    for (idx, field) in fields.iter().enumerate() {
        if field.skip.is_present() {
            continue;
        }
        let access = field_access(field, idx);
        checks.push(build_eq_check(field, &FieldAccess::StructField(&access)));
    }
    Ok(checks)
}

fn expand_enum_eq(enum_name: &Ident, variants: &[OrdVariant]) -> Result<TokenStream> {
    if variants.is_empty() {
        return Ok(quote! { true });
    }

    let match_arms = build_enum_eq_arms(enum_name, variants)?;

    Ok(quote! {
        match (self, other) {
            #(#match_arms,)*
            _ => false,
        }
    })
}

fn build_enum_eq_arms(enum_name: &Ident, variants: &[OrdVariant]) -> Result<Vec<TokenStream>> {
    variants
        .iter()
        .map(|v| build_variant_eq_arm(enum_name, v))
        .collect()
}

fn build_variant_eq_arm(enum_name: &Ident, variant: &OrdVariant) -> Result<TokenStream> {
    let (self_pat, other_pat, bindings) = build_variant_patterns(enum_name, variant);
    let fields_vec: Vec<_> = variant.fields.iter().cloned().collect();

    let checks = build_binding_eq_checks(&fields_vec, &bindings);
    let eq_expr = chain_eq_checks(&checks);

    Ok(quote! { (#self_pat, #other_pat) => { #eq_expr } })
}

fn build_binding_eq_checks(fields: &[OrdField], bindings: &[(Ident, Ident)]) -> Vec<TokenStream> {
    fields
        .iter()
        .zip(bindings)
        .filter(|(f, _)| !f.skip.is_present())
        .map(|(field, (self_id, other_id))| {
            build_eq_check(field, &FieldAccess::Bindings { self_id, other_id })
        })
        .collect()
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
) -> Result<TokenStream> {
    let config = input.trait_config();

    if config.ord {
        return Ok(quote! {
            impl #impl_generics ::core::cmp::PartialOrd for #name #ty_generics #where_clause {
                #[inline]
                fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                    ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
                }
            }
        });
    }

    let cmp_body = build_cmp_body(input)?;
    let final_cmp = apply_reverse(input, cmp_body);

    Ok(quote! {
        impl #impl_generics ::core::cmp::PartialOrd for #name #ty_generics #where_clause {
            #[inline]
            fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(#final_cmp)
            }
        }
    })
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
) -> Result<TokenStream> {
    let cmp_body = build_cmp_body(input)?;
    let final_cmp = apply_reverse(input, cmp_body);

    Ok(quote! {
        impl #impl_generics ::core::cmp::Ord for #name #ty_generics #where_clause {
            #[inline]
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                #final_cmp
            }
        }
    })
}

fn build_cmp_body(input: &OrdDerive) -> Result<TokenStream> {
    match &input.data {
        darling::ast::Data::Struct(fields) => expand_struct_cmp(input, fields),
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

fn expand_struct_cmp(input: &OrdDerive, fields: &Fields<OrdField>) -> Result<TokenStream> {
    let comparisons = build_struct_cmp_checks(input, fields)?;
    Ok(chain_cmp_checks(&comparisons))
}

fn build_struct_cmp_checks(
    input: &OrdDerive,
    fields: &Fields<OrdField>,
) -> Result<Vec<TokenStream>> {
    let fields_vec: Vec<_> = fields.iter().cloned().collect();

    if let Some(ref order_list) = input.field_order {
        build_explicit_order_cmp_checks(order_list, &fields_vec)
    } else {
        build_implicit_cmp_checks(&fields_vec)
    }
}

fn build_explicit_order_cmp_checks(
    order_list: &FieldOrderList,
    fields: &[OrdField],
) -> Result<Vec<TokenStream>> {
    let field_map: std::collections::HashMap<String, (usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter_map(|(i, f)| f.ident.as_ref().map(|id| (id.to_string(), (i, f))))
        .collect();

    let mut comparisons = Vec::new();
    for entry in &order_list.0 {
        if let Some(&(idx, field)) = field_map.get(&entry.ident.to_string()) {
            let access = field_access(field, idx);
            comparisons.push(build_cmp_check(
                field,
                &FieldAccess::StructField(&access),
                entry.order,
            ));
        }
    }
    Ok(comparisons)
}

fn build_implicit_cmp_checks(fields: &[OrdField]) -> Result<Vec<TokenStream>> {
    let sorted = sorted_field_indices(fields);

    let comparisons = sorted
        .into_iter()
        .map(|(idx, field)| {
            let access = field_access(field, idx);
            build_cmp_check(
                field,
                &FieldAccess::StructField(&access),
                field.effective_order(),
            )
        })
        .collect();

    Ok(comparisons)
}

//=============================================================================
// Enum Comparison
//=============================================================================

fn expand_enum_cmp(input: &OrdDerive, variants: &[OrdVariant]) -> Result<TokenStream> {
    if variants.is_empty() {
        return Ok(quote! { ::core::cmp::Ordering::Equal });
    }

    let enum_name = &input.ident;
    let variant_ranks = compute_variant_ranks(variants);
    let match_arms = build_enum_cmp_arms(enum_name, variants)?;
    let discriminant_fn = build_discriminant_fn(enum_name, variants, &variant_ranks);

    Ok(quote! {
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
    })
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

fn build_enum_cmp_arms(enum_name: &Ident, variants: &[OrdVariant]) -> Result<Vec<TokenStream>> {
    variants
        .iter()
        .map(|v| build_variant_cmp_arm(enum_name, v))
        .collect()
}

fn build_variant_cmp_arm(enum_name: &Ident, variant: &OrdVariant) -> Result<TokenStream> {
    let (self_pat, other_pat, bindings) = build_variant_patterns(enum_name, variant);
    let fields_vec: Vec<_> = variant.fields.iter().cloned().collect();

    let comparisons = build_binding_cmp_checks(&fields_vec, &bindings);
    let cmp_expr = chain_cmp_checks(&comparisons);

    Ok(quote! { (#self_pat, #other_pat) => { #cmp_expr } })
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
