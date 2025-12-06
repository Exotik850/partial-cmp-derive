//! Code generation for the `PartialCmpDerive` macro.

use darling::{Result, ast::Fields};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::types::{FieldOrderList, NoneOrder, OrdDerive, OrdField, OrdVariant, SortOrder};

/// Expands the derive macro into `PartialOrd` and Ord implementations.
pub fn expand_derive(input: &OrdDerive) -> Result<TokenStream> {
    // Validate input first
    input.validate()?;

    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

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
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }

        impl #impl_generics ::core::cmp::Ord for #name #ty_generics #where_clause {
            #[inline]
            fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                #final_cmp
            }
        }
    })
}

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
            // Find the field index for the field
            let field_index = fields
                .iter()
                .position(|f| f.ident.as_ref().map(std::string::ToString::to_string) == Some(name.clone()))
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

    let field_access = if let Some(ident) = &field.ident { quote! { #ident } } else {
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
            (::core::option::Option::Some(ref a), ::core::option::Option::Some(ref b)) => {
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
    let match_arms = build_enum_match_arms(enum_name, variants, &variant_ranks)?;

    Ok(quote! {
        #match_arms
    })
}

/// Builds match arms for enum comparison.
fn build_enum_match_arms(
    enum_name: &Ident,
    variants: &[OrdVariant],
    variant_ranks: &std::collections::HashMap<String, usize>,
) -> Result<TokenStream> {
    let mut arms = Vec::new();

    for variant in variants {
        let variant_name = &variant.ident;
        let self_rank = variant_ranks[&variant_name.to_string()];

        // Build pattern and field bindings
        let (self_pattern, other_pattern, field_cmp) =
            build_variant_patterns_and_cmp(enum_name, variant)?;

        // Same variant comparison
        arms.push(quote! {
            (#self_pattern, #other_pattern) => { #field_cmp }
        });
    }

    // Different variants - compare by rank
    // We need to build a discriminant-based comparison
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

            let mut out = quote! {};
            for field in v.fields.iter() {
                match &field.ident {
                    Some(ident) => {
                        out = quote! { #out #ident: _ , };
                    }
                    None => {
                        out = quote! { #out _ , };
                    }
                }
            }
            quote! {
                #enum_name::#variant_name { #out } => #rank
            }

            // let pattern = match &v.fields {
            //     Fields::Unit => quote! { #enum_name::#variant_name },
            //     Fields::Tuple(fields) => {
            //         let underscores: Vec<_> = fields.iter().map(|_| quote! { _ }).collect();
            //         quote! { #enum_name::#variant_name(#(#underscores),*) }
            //     }
            //     Fields::Named(fields) => {
            //         let underscores: Vec<TokenStream> = fields
            //             .iter()
            //             .filter_map(|f| f.ident.as_ref())
            //             .map(|i| quote! { #i: _ })
            //             .collect();
            //         quote! { #enum_name::#variant_name { #(#underscores),* } }
            //     }
            // };
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
    let is_tuple = fields
        .iter()
        .next()
        .is_some_and(|f| f.ident.is_none());

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

        let self_renames: Vec<TokenStream> = field_idents
            .iter()
            .zip(&self_bindings)
            .map(|(orig, binding)| quote! { #orig: #binding })
            .collect();
        let other_renames: Vec<TokenStream> = field_idents
            .iter()
            .zip(&other_bindings)
            .map(|(orig, binding)| quote! { #orig: #binding })
            .collect();

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
            (::core::option::Option::Some(ref a), ::core::option::Option::Some(ref b)) => {
                ::core::cmp::Ord::cmp(a, b)
            }
        }
    }
}
