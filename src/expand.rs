//! Code generation for the `PartialCmpDerive` macro.

use darling::{Result, ast::Fields};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::types::{FieldOrderList, NoneOrder, OrdDerive, OrdField, OrdVariant, SortOrder};

//=============================================================================
// Main Entry Point
//=============================================================================

/// Expands the derive macro into trait implementations.
pub fn expand_derive(input: &OrdDerive) -> Result<TokenStream> {
    let config = input.trait_config();
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut output = TokenStream::new();

    // Generate PartialEq if enabled
    if config.partial_eq {
        let partial_eq_impl =
            expand_partial_eq(input, name, &impl_generics, &ty_generics, where_clause)?;
        output.extend(partial_eq_impl);
    }

    // Generate Eq if enabled (it's a marker trait with no methods)
    if config.eq {
        let eq_impl = quote! {
            impl #impl_generics ::core::cmp::Eq for #name #ty_generics #where_clause {}
        };
        output.extend(eq_impl);
    }

    // Generate PartialOrd if enabled
    if config.partial_ord {
        let partial_ord_impl =
            expand_partial_ord(input, name, &impl_generics, &ty_generics, where_clause)?;
        output.extend(partial_ord_impl);
    }

    // Generate Ord if enabled
    if config.ord {
        let ord_impl = expand_ord(input, name, &impl_generics, &ty_generics, where_clause)?;
        output.extend(ord_impl);
    }

    Ok(output)
}

//=============================================================================
// PartialEq Implementation
//=============================================================================

/// Expands into a PartialEq implementation.
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

/// Expands equality check for struct types.
fn expand_struct_eq(input: &OrdDerive, fields: &Fields<OrdField>) -> Result<TokenStream> {
    let checks = build_field_equality_checks(input, fields)?;

    if checks.is_empty() {
        return Ok(quote! { true });
    }

    Ok(chain_equality_checks(&checks))
}

/// Builds equality check expressions for fields.
fn build_field_equality_checks(
    input: &OrdDerive,
    fields: &Fields<OrdField>,
) -> Result<Vec<TokenStream>> {
    // If explicit field order is specified, only those fields participate in comparison
    if let Some(ref order_list) = input.field_order {
        return build_explicit_order_equality_checks(order_list, fields);
    }

    let mut checks = Vec::new();

    for (idx, field) in fields.iter().enumerate() {
        // Skip fields marked with #[ord(skip)]
        if field.skip.is_present() {
            continue;
        }

        let field_access = if let Some(ref ident) = field.ident {
            quote! { #ident }
        } else {
            let idx = syn::Index::from(idx);
            quote! { #idx }
        };

        let check = build_single_field_equality(field, &field_access)?;
        checks.push(check);
    }

    Ok(checks)
}

/// Builds equality checks for explicitly ordered fields.
fn build_explicit_order_equality_checks(
    order_list: &FieldOrderList,
    fields: &Fields<OrdField>,
) -> Result<Vec<TokenStream>> {
    let field_map: std::collections::HashMap<String, &OrdField> = fields
        .iter()
        .filter_map(|f| f.ident.as_ref().map(|i| (i.to_string(), f)))
        .collect();

    let mut checks = Vec::new();

    for entry in &order_list.0 {
        let name = entry.ident.to_string();
        if let Some(field) = field_map.get(&name) {
            let ident = &entry.ident;
            let field_access = quote! { #ident };
            let check = build_single_field_equality(field, &field_access)?;
            checks.push(check);
        }
    }

    Ok(checks)
}

/// Builds equality check for a single field.
fn build_single_field_equality(
    field: &OrdField,
    field_access: &TokenStream,
) -> Result<TokenStream> {
    let check = if let Some(ref eq_with) = field.eq_with {
        let path = eq_with.as_ref();
        quote! { #path(&self.#field_access, &other.#field_access) }
    } else if let Some(ref compare_with) = field.compare_with {
        // Derive equality from compare_with: equal if cmp returns Equal
        let path = compare_with.as_ref();
        quote! { #path(&self.#field_access, &other.#field_access) == ::core::cmp::Ordering::Equal }
    } else if field.none_order.is_some() {
        build_option_equality(field_access)
    } else {
        quote! { self.#field_access == other.#field_access }
    };

    Ok(check)
}

/// Builds equality logic for Option fields.
fn build_option_equality(field_access: &TokenStream) -> TokenStream {
    // For equality, None ordering doesn't matter - None == None, Some(a) == Some(b) iff a == b
    quote! {
        match (&self.#field_access, &other.#field_access) {
            (::core::option::Option::None, ::core::option::Option::None) => true,
            (::core::option::Option::Some(a), ::core::option::Option::Some(b)) => a == b,
            _ => false,
        }
    }
}

/// Chains equality checks with short-circuit AND.
fn chain_equality_checks(checks: &[TokenStream]) -> TokenStream {
    if checks.is_empty() {
        return quote! { true };
    }

    if checks.len() == 1 {
        return checks[0].clone();
    }

    // Build: check1 && check2 && check3 ...
    let first = &checks[0];
    let rest = &checks[1..];

    quote! {
        #first #(&& #rest)*
    }
}

/// Expands equality check for enum types.
fn expand_enum_eq(enum_name: &Ident, variants: &[OrdVariant]) -> Result<TokenStream> {
    if variants.is_empty() {
        return Ok(quote! { true });
    }

    let match_arms = build_enum_eq_match_arms(enum_name, variants)?;

    Ok(quote! {
        match (self, other) {
            #(#match_arms,)*
            _ => false, // Different variants are never equal
        }
    })
}

/// Builds match arms for enum equality comparison.
fn build_enum_eq_match_arms(
    enum_name: &Ident,
    variants: &[OrdVariant],
) -> Result<Vec<TokenStream>> {
    let mut arms = Vec::new();

    for variant in variants {
        let (self_pattern, other_pattern, eq_check) =
            build_variant_patterns_and_eq(enum_name, variant)?;

        arms.push(quote! {
            (#self_pattern, #other_pattern) => { #eq_check }
        });
    }

    Ok(arms)
}

/// Builds patterns and equality check for a variant.
fn build_variant_patterns_and_eq(
    enum_name: &Ident,
    variant: &OrdVariant,
) -> Result<(TokenStream, TokenStream, TokenStream)> {
    let variant_name = &variant.ident;
    let fields = &variant.fields;

    // Unit variant (no fields)
    if fields.is_empty() {
        return Ok((
            quote! { #enum_name::#variant_name },
            quote! { #enum_name::#variant_name },
            quote! { true },
        ));
    }

    // Determine if this is a tuple variant or named variant
    let is_tuple = fields.iter().next().is_some_and(|f| f.ident.is_none());

    if is_tuple {
        build_tuple_variant_eq(enum_name, variant_name, fields)
    } else {
        build_named_variant_eq(enum_name, variant_name, fields)
    }
}

/// Builds equality patterns for tuple variants.
fn build_tuple_variant_eq(
    enum_name: &Ident,
    variant_name: &Ident,
    fields: &Fields<OrdField>,
) -> Result<(TokenStream, TokenStream, TokenStream)> {
    let field_count = fields.len();
    let self_bindings: Vec<Ident> = (0..field_count)
        .map(|i| format_ident!("__self_{}", i))
        .collect();
    let other_bindings: Vec<Ident> = (0..field_count)
        .map(|i| format_ident!("__other_{}", i))
        .collect();

    let self_pattern = quote! { #enum_name::#variant_name(#(#self_bindings),*) };
    let other_pattern = quote! { #enum_name::#variant_name(#(#other_bindings),*) };

    let checks = build_binding_equality_checks(fields, &self_bindings, &other_bindings)?;

    Ok((self_pattern, other_pattern, chain_equality_checks(&checks)))
}

/// Builds equality patterns for named variants.
fn build_named_variant_eq(
    enum_name: &Ident,
    variant_name: &Ident,
    fields: &Fields<OrdField>,
) -> Result<(TokenStream, TokenStream, TokenStream)> {
    let field_idents: Vec<&Ident> = fields.iter().filter_map(|f| f.ident.as_ref()).collect();

    let self_bindings: Vec<Ident> = field_idents
        .iter()
        .map(|i| format_ident!("__self_{}", i))
        .collect();
    let other_bindings: Vec<Ident> = field_idents
        .iter()
        .map(|i| format_ident!("__other_{}", i))
        .collect();

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

    let fields_vec: Vec<OrdField> = fields.iter().cloned().collect();
    let checks = build_binding_equality_checks_named(&fields_vec, &self_bindings, &other_bindings)?;

    Ok((self_pattern, other_pattern, chain_equality_checks(&checks)))
}

/// Builds equality checks using binding identifiers for tuple variants.
fn build_binding_equality_checks(
    fields: &Fields<OrdField>,
    self_bindings: &[Ident],
    other_bindings: &[Ident],
) -> Result<Vec<TokenStream>> {
    let mut checks = Vec::new();

    for (idx, field) in fields.iter().enumerate() {
        if field.skip.is_present() {
            continue;
        }

        let self_binding = &self_bindings[idx];
        let other_binding = &other_bindings[idx];

        let check = if let Some(ref eq_with) = field.eq_with {
            let path = eq_with.as_ref();
            quote! { #path(#self_binding, #other_binding) }
        } else if let Some(ref compare_with) = field.compare_with {
            let path = compare_with.as_ref();
            quote! { #path(#self_binding, #other_binding) == ::core::cmp::Ordering::Equal }
        } else if field.none_order.is_some() {
            build_binding_option_equality(self_binding, other_binding)
        } else {
            quote! { #self_binding == #other_binding }
        };

        checks.push(check);
    }

    Ok(checks)
}

/// Builds equality checks using binding identifiers for named variants.
fn build_binding_equality_checks_named(
    fields: &[OrdField],
    self_bindings: &[Ident],
    other_bindings: &[Ident],
) -> Result<Vec<TokenStream>> {
    let mut checks = Vec::new();

    for (idx, field) in fields.iter().enumerate() {
        if field.skip.is_present() {
            continue;
        }

        let self_binding = &self_bindings[idx];
        let other_binding = &other_bindings[idx];

        let check = if let Some(ref eq_with) = field.eq_with {
            let path = eq_with.as_ref();
            quote! { #path(#self_binding, #other_binding) }
        } else if let Some(ref compare_with) = field.compare_with {
            let path = compare_with.as_ref();
            quote! { #path(#self_binding, #other_binding) == ::core::cmp::Ordering::Equal }
        } else if field.none_order.is_some() {
            build_binding_option_equality(self_binding, other_binding)
        } else {
            quote! { #self_binding == #other_binding }
        };

        checks.push(check);
    }

    Ok(checks)
}

/// Builds Option equality using binding identifiers.
fn build_binding_option_equality(self_binding: &Ident, other_binding: &Ident) -> TokenStream {
    quote! {
        match (#self_binding, #other_binding) {
            (::core::option::Option::None, ::core::option::Option::None) => true,
            (::core::option::Option::Some(a), ::core::option::Option::Some(b)) => a == b,
            _ => false,
        }
    }
}

//=============================================================================
// PartialOrd Implementation
//=============================================================================

/// Expands into a PartialOrd implementation.
fn expand_partial_ord(
    input: &OrdDerive,
    name: &Ident,
    impl_generics: &syn::ImplGenerics,
    ty_generics: &syn::TypeGenerics,
    where_clause: Option<&syn::WhereClause>,
) -> Result<TokenStream> {
    let config = input.trait_config();

    // If we're also generating Ord, delegate to it
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

    // Otherwise, generate standalone PartialOrd
    let cmp_body = match &input.data {
        darling::ast::Data::Struct(fields) => expand_struct_cmp(input, fields)?,
        darling::ast::Data::Enum(variants) => expand_enum_cmp(input, variants)?,
    };

    // Apply reverse if needed
    let final_cmp = if input.reverse.is_present() {
        quote! {
            {
                let result = { #cmp_body };
                result.reverse()
            }
        }
    } else {
        cmp_body
    };

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

/// Expands into an Ord implementation.
fn expand_ord(
    input: &OrdDerive,
    name: &Ident,
    impl_generics: &syn::ImplGenerics,
    ty_generics: &syn::TypeGenerics,
    where_clause: Option<&syn::WhereClause>,
) -> Result<TokenStream> {
    let cmp_body = match &input.data {
        darling::ast::Data::Struct(fields) => expand_struct_cmp(input, fields)?,
        darling::ast::Data::Enum(variants) => expand_enum_cmp(input, variants)?,
    };

    // Apply reverse if needed
    let final_cmp = if input.reverse.is_present() {
        quote! {
            {
                let result = { #cmp_body };
                result.reverse()
            }
        }
    } else {
        cmp_body
    };

    Ok(quote! {
        impl #impl_generics ::core::cmp::Ord for #name #ty_generics #where_clause {
            #[inline]
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                #final_cmp
            }
        }
    })
}

//=============================================================================
// Struct Comparison (shared between PartialOrd and Ord)
//=============================================================================

/// Expands comparison for struct types.
fn expand_struct_cmp(input: &OrdDerive, fields: &Fields<OrdField>) -> Result<TokenStream> {
    let comparisons = build_field_comparisons(input, fields)?;

    if comparisons.is_empty() {
        return Ok(quote! { ::core::cmp::Ordering::Equal });
    }

    Ok(chain_comparisons(&comparisons))
}

/// Builds the list of field comparisons in the correct order.
fn build_field_comparisons(
    input: &OrdDerive,
    fields: &Fields<OrdField>,
) -> Result<Vec<TokenStream>> {
    if let Some(ref order_list) = input.field_order {
        // Explicit ordering via `by` attribute
        build_explicit_order_comparisons(order_list, fields)
    } else {
        // Implicit ordering via priority and declaration order
        build_implicit_order_comparisons(fields)
    }
}

/// Builds comparisons for explicitly ordered fields.
fn build_explicit_order_comparisons(
    order_list: &FieldOrderList,
    fields: &Fields<OrdField>,
) -> Result<Vec<TokenStream>> {
    let field_map: std::collections::HashMap<String, &OrdField> = fields
        .iter()
        .filter_map(|f| f.ident.as_ref().map(|i| (i.to_string(), f)))
        .collect();

    let mut comparisons = Vec::new();

    for entry in &order_list.0 {
        let name = entry.ident.to_string();
        if let Some(field) = field_map.get(&name) {
            // Find the field index
            let field_index = fields
                .iter()
                .position(|f| {
                    f.ident.as_ref().map(std::string::ToString::to_string) == Some(name.clone())
                })
                .unwrap_or(0);
            let cmp = build_single_field_comparison(field, Some(entry.order), field_index)?;
            comparisons.push(cmp);
        }
    }

    Ok(comparisons)
}

/// Builds comparisons for implicitly ordered fields.
fn build_implicit_order_comparisons(fields: &Fields<OrdField>) -> Result<Vec<TokenStream>> {
    // Collect fields with their indices for stable sorting
    let mut indexed_fields: Vec<(usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !f.skip.is_present())
        .collect();

    // Sort by priority, then by declaration order
    indexed_fields.sort_by(|(idx_a, a), (idx_b, b)| {
        a.effective_priority()
            .cmp(&b.effective_priority())
            .then(idx_a.cmp(idx_b))
    });

    let mut comparisons = Vec::new();

    for (idx, field) in indexed_fields {
        let cmp = build_single_field_comparison(field, None, idx)?;
        comparisons.push(cmp);
    }

    Ok(comparisons)
}

/// Builds a comparison expression for a single field.
fn build_single_field_comparison(
    field: &OrdField,
    order_override: Option<SortOrder>,
    field_index: usize,
) -> Result<TokenStream> {
    let order = order_override.unwrap_or_else(|| field.effective_order());

    let field_access = if let Some(ident) = &field.ident {
        quote! { #ident }
    } else {
        // Tuple struct field - use numeric index
        let idx = syn::Index::from(field_index);
        quote! { #idx }
    };

    let base_cmp = if let Some(ref compare_with) = field.compare_with {
        let path = compare_with.as_ref();
        quote! { #path(&self.#field_access, &other.#field_access) }
    } else if let Some(none_order) = field.none_order {
        build_option_comparison(&field_access, none_order)
    } else {
        quote! { ::core::cmp::Ord::cmp(&self.#field_access, &other.#field_access) }
    };

    let final_cmp = match order {
        SortOrder::Asc => base_cmp,
        SortOrder::Desc => quote! { #base_cmp.reverse() },
    };

    Ok(final_cmp)
}

/// Builds comparison logic for Option fields with custom None ordering.
fn build_option_comparison(field_access: &TokenStream, none_order: NoneOrder) -> TokenStream {
    let (none_none, none_some, some_none) = match none_order {
        NoneOrder::First => (
            quote! { ::core::cmp::Ordering::Equal },
            quote! { ::core::cmp::Ordering::Less },
            quote! { ::core::cmp::Ordering::Greater },
        ),
        NoneOrder::Last => (
            quote! { ::core::cmp::Ordering::Equal },
            quote! { ::core::cmp::Ordering::Greater },
            quote! { ::core::cmp::Ordering::Less },
        ),
    };

    quote! {
        match (&self.#field_access, &other.#field_access) {
            (::core::option::Option::None, ::core::option::Option::None) => #none_none,
            (::core::option::Option::None, ::core::option::Option::Some(_)) => #none_some,
            (::core::option::Option::Some(_), ::core::option::Option::None) => #some_none,
            (::core::option::Option::Some(a), ::core::option::Option::Some(b)) => {
                ::core::cmp::Ord::cmp(a, b)
            }
        }
    }
}

/// Chains multiple comparison expressions with early return on non-Equal.
fn chain_comparisons(comparisons: &[TokenStream]) -> TokenStream {
    if comparisons.is_empty() {
        return quote! { ::core::cmp::Ordering::Equal };
    }

    if comparisons.len() == 1 {
        return comparisons[0].clone();
    }

    // Build a chain: match cmp1 { Equal => match cmp2 { Equal => ... } }
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

//=============================================================================
// Enum Comparison (shared between PartialOrd and Ord)
//=============================================================================

/// Expands comparison for enum types.
fn expand_enum_cmp(input: &OrdDerive, variants: &[OrdVariant]) -> Result<TokenStream> {
    if variants.is_empty() {
        return Ok(quote! { ::core::cmp::Ordering::Equal });
    }

    let enum_name = &input.ident;

    // Build sorted variants by rank
    let mut indexed_variants: Vec<(usize, &OrdVariant)> = variants.iter().enumerate().collect();

    indexed_variants.sort_by_key(|(idx, v)| v.effective_rank(*idx));

    // Create discriminant values based on rank order
    let variant_ranks: std::collections::HashMap<String, usize> = indexed_variants
        .iter()
        .enumerate()
        .map(|(rank, (_, v))| (v.ident.to_string(), rank))
        .collect();

    // Build match arms for comparing variants
    let match_arms = build_enum_cmp_match_arms(enum_name, variants, &variant_ranks)?;

    Ok(quote! {
        #match_arms
    })
}

/// Builds match arms for enum comparison.
fn build_enum_cmp_match_arms(
    enum_name: &Ident,
    variants: &[OrdVariant],
    variant_ranks: &std::collections::HashMap<String, usize>,
) -> Result<TokenStream> {
    let mut arms = Vec::new();

    for variant in variants {
        // Build pattern and field bindings
        let (self_pattern, other_pattern, field_cmp) =
            build_variant_patterns_and_cmp(enum_name, variant)?;

        // Same variant comparison
        arms.push(quote! {
            (#self_pattern, #other_pattern) => { #field_cmp }
        });
    }

    // Different variants - compare by rank
    let discriminant_fn = build_discriminant_fn(enum_name, variants, variant_ranks);

    Ok(quote! {
        {
            #discriminant_fn

            let self_disc = __discriminant(self);
            let other_disc = __discriminant(other);

            if self_disc != other_disc {
                ::core::cmp::Ord::cmp(&self_disc, &other_disc)
            } else {
                match (self, other) {
                    #(#arms,)*
                    // NOTE: This arm is unreachable due to the discriminant check above,
                    // but we need it for exhaustiveness
                    _ => ::core::cmp::Ordering::Equal,
                }
            }
        }
    })
}

/// Builds a helper function that returns discriminant values based on rank.
fn build_discriminant_fn(
    enum_name: &Ident,
    variants: &[OrdVariant],
    variant_ranks: &std::collections::HashMap<String, usize>,
) -> TokenStream {
    let arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let variant_name = &v.ident;
            let rank = variant_ranks[&variant_name.to_string()];

            // Determine if this is a tuple variant or named variant
            let is_tuple = v.fields.iter().next().is_some_and(|f| f.ident.is_none());
            let field_count = v.fields.len();

            if field_count == 0 {
                // Unit variant
                quote! {
                    #enum_name::#variant_name => #rank
                }
            } else if is_tuple {
                // Tuple variant - use (..) pattern
                let underscores: Vec<TokenStream> =
                    (0..field_count).map(|_| quote! { _ }).collect();
                quote! {
                    #enum_name::#variant_name(#(#underscores),*) => #rank
                }
            } else {
                // Named variant - use { .. } pattern
                quote! {
                    #enum_name::#variant_name { .. } => #rank
                }
            }
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

/// Builds patterns and field comparison for a variant.
fn build_variant_patterns_and_cmp(
    enum_name: &Ident,
    variant: &OrdVariant,
) -> Result<(TokenStream, TokenStream, TokenStream)> {
    let variant_name = &variant.ident;
    let fields = &variant.fields;

    // Unit variant (no fields)
    if fields.is_empty() {
        return Ok((
            quote! { #enum_name::#variant_name },
            quote! { #enum_name::#variant_name },
            quote! { ::core::cmp::Ordering::Equal },
        ));
    }

    // Determine if this is a tuple variant (fields have no idents) or named variant
    let is_tuple = fields.iter().next().is_some_and(|f| f.ident.is_none());

    if is_tuple {
        // Tuple variant
        let field_count = fields.len();
        let self_bindings: Vec<Ident> = (0..field_count)
            .map(|i| format_ident!("__self_{}", i))
            .collect();
        let other_bindings: Vec<Ident> = (0..field_count)
            .map(|i| format_ident!("__other_{}", i))
            .collect();

        let self_pattern = quote! { #enum_name::#variant_name(#(#self_bindings),*) };
        let other_pattern = quote! { #enum_name::#variant_name(#(#other_bindings),*) };

        let fields_vec: Vec<OrdField> = fields.iter().cloned().collect();
        let comparisons =
            build_tuple_field_comparisons(&fields_vec, &self_bindings, &other_bindings)?;

        Ok((self_pattern, other_pattern, chain_comparisons(&comparisons)))
    } else {
        // Named variant
        let field_idents: Vec<&Ident> = fields.iter().filter_map(|f| f.ident.as_ref()).collect();

        let self_bindings: Vec<Ident> = field_idents
            .iter()
            .map(|i| format_ident!("__self_{}", i))
            .collect();
        let other_bindings: Vec<Ident> = field_idents
            .iter()
            .map(|i| format_ident!("__other_{}", i))
            .collect();

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

        let fields_vec: Vec<OrdField> = fields.iter().cloned().collect();
        let comparisons =
            build_named_field_comparisons(&fields_vec, &self_bindings, &other_bindings)?;

        Ok((self_pattern, other_pattern, chain_comparisons(&comparisons)))
    }
}

/// Builds comparisons for tuple variant fields.
fn build_tuple_field_comparisons(
    fields: &[OrdField],
    self_bindings: &[Ident],
    other_bindings: &[Ident],
) -> Result<Vec<TokenStream>> {
    let mut indexed_fields: Vec<(usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !f.skip.is_present())
        .collect();

    indexed_fields.sort_by(|(idx_a, a), (idx_b, b)| {
        a.effective_priority()
            .cmp(&b.effective_priority())
            .then(idx_a.cmp(idx_b))
    });

    let mut comparisons = Vec::new();

    for (idx, field) in indexed_fields {
        let self_binding = &self_bindings[idx];
        let other_binding = &other_bindings[idx];
        let order = field.effective_order();

        let base_cmp = if let Some(ref compare_with) = field.compare_with {
            let path = compare_with.as_ref();
            quote! { #path(#self_binding, #other_binding) }
        } else if let Some(none_order) = field.none_order {
            build_binding_option_comparison(self_binding, other_binding, none_order)
        } else {
            quote! { ::core::cmp::Ord::cmp(#self_binding, #other_binding) }
        };

        let final_cmp = match order {
            SortOrder::Asc => base_cmp,
            SortOrder::Desc => quote! { #base_cmp.reverse() },
        };

        comparisons.push(final_cmp);
    }

    Ok(comparisons)
}

/// Builds comparisons for named variant fields.
fn build_named_field_comparisons(
    fields: &[OrdField],
    self_bindings: &[Ident],
    other_bindings: &[Ident],
) -> Result<Vec<TokenStream>> {
    let mut indexed_fields: Vec<(usize, &OrdField)> = fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !f.skip.is_present())
        .collect();

    indexed_fields.sort_by(|(idx_a, a), (idx_b, b)| {
        a.effective_priority()
            .cmp(&b.effective_priority())
            .then(idx_a.cmp(idx_b))
    });

    let mut comparisons = Vec::new();

    for (idx, field) in indexed_fields {
        let self_binding = &self_bindings[idx];
        let other_binding = &other_bindings[idx];
        let order = field.effective_order();

        let base_cmp = if let Some(ref compare_with) = field.compare_with {
            let path = compare_with.as_ref();
            quote! { #path(#self_binding, #other_binding) }
        } else if let Some(none_order) = field.none_order {
            build_binding_option_comparison(self_binding, other_binding, none_order)
        } else {
            quote! { ::core::cmp::Ord::cmp(#self_binding, #other_binding) }
        };

        let final_cmp = match order {
            SortOrder::Asc => base_cmp,
            SortOrder::Desc => quote! { #base_cmp.reverse() },
        };

        comparisons.push(final_cmp);
    }

    Ok(comparisons)
}

/// Builds Option comparison using binding identifiers.
fn build_binding_option_comparison(
    self_binding: &Ident,
    other_binding: &Ident,
    none_order: NoneOrder,
) -> TokenStream {
    let (none_none, none_some, some_none) = match none_order {
        NoneOrder::First => (
            quote! { ::core::cmp::Ordering::Equal },
            quote! { ::core::cmp::Ordering::Less },
            quote! { ::core::cmp::Ordering::Greater },
        ),
        NoneOrder::Last => (
            quote! { ::core::cmp::Ordering::Equal },
            quote! { ::core::cmp::Ordering::Greater },
            quote! { ::core::cmp::Ordering::Less },
        ),
    };

    quote! {
        match (#self_binding, #other_binding) {
            (::core::option::Option::None, ::core::option::Option::None) => #none_none,
            (::core::option::Option::None, ::core::option::Option::Some(_)) => #none_some,
            (::core::option::Option::Some(_), ::core::option::Option::None) => #some_none,
            (::core::option::Option::Some(a), ::core::option::Option::Some(b)) => {
                ::core::cmp::Ord::cmp(a, b)
            }
        }
    }
}
