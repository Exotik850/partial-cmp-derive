/// Shared helpers and abstractions used across eq, cmp, and hash code generation.
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use crate::types::{FieldOrderList, NoneOrder, OrdField, SortOrder};

/// Represents how to access a field for comparison or hashing.
pub enum FieldAccess<'a> {
    /// Access via `self.field` and `other.field`
    StructField(&'a TokenStream),
    /// Access via bound identifiers
    Bindings {
        self_id: &'a Ident,
        other_id: &'a Ident,
    },
}

impl FieldAccess<'_> {
    /// Builds the expression to access the `self` value.
    pub fn self_expr(&self) -> TokenStream {
        match self {
            Self::StructField(access) => quote! { self.#access },
            Self::Bindings { self_id, .. } => quote! { #self_id },
        }
    }

    /// Builds the expression to access the `other` value.
    pub fn other_expr(&self) -> TokenStream {
        match self {
            Self::StructField(access) => quote! { other.#access },
            Self::Bindings { other_id, .. } => quote! { #other_id },
        }
    }

    /// Builds references to the `self` and `other` values.
    pub fn as_refs(&self) -> (TokenStream, TokenStream) {
        match self {
            Self::StructField(access) => (quote! { &self.#access }, quote! { &other.#access }),
            Self::Bindings { self_id, other_id } => (quote! { #self_id }, quote! { #other_id }),
        }
    }
}

/// Builds field access token for a field at the given index (identifier or tuple index).
pub fn field_access(field: &OrdField, index: usize) -> TokenStream {
    if let Some(ref ident) = field.ident {
        quote! { #ident }
    } else {
        let idx = syn::Index::from(index);
        quote! { #idx }
    }
}

/// Collects fields sorted by priority, filtering out skipped fields.
pub fn sorted_field_indices(fields: &[OrdField]) -> Vec<(usize, &OrdField)> {
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
pub fn generate_bindings(prefix: &str, count: usize) -> Vec<Ident> {
    (0..count)
        .map(|i| format_ident!("__{prefix}_{i}"))
        .collect()
}

/// Generates binding identifiers for named fields.
pub fn generate_named_bindings(prefix: &str, idents: &[&Ident]) -> Vec<Ident> {
    idents
        .iter()
        .map(|i| format_ident!("__{prefix}_{i}"))
        .collect()
}

/// Builds an equality check for a single field.
pub fn build_eq_check(field: &OrdField, access: &FieldAccess) -> TokenStream {
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
pub fn build_cmp_check(field: &OrdField, access: &FieldAccess, order: SortOrder) -> TokenStream {
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
pub fn build_option_eq(access: &FieldAccess) -> TokenStream {
    let (self_ref, other_ref) = access.as_refs();
    quote! {
        match (#self_ref, #other_ref) {
            (::core::option::Option::None, ::core::option::Option::None) => true,
            (::core::option::Option::Some(a), ::core::option::Option::Some(b)) => a == b,
            _ => false,
        }
    }
}

/// Builds Option comparison with custom None ordering.
pub fn build_option_cmp(access: &FieldAccess, none_order: NoneOrder) -> TokenStream {
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
pub fn chain_eq_checks(checks: impl IntoIterator<Item = TokenStream>) -> TokenStream {
    let mut iter = checks.into_iter();
    if let Some(first) = iter.next() {
        iter.fold(first, |acc, check| quote! { #acc && #check })
    } else {
        quote! { true }
    }
}

/// Chains comparisons with early return on non-Equal.
pub fn chain_cmp_checks(comparisons: impl IntoIterator<Item = TokenStream>) -> TokenStream {
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

/// Builds explicit-order equality checks for struct fields using a `by = [...]` list.
pub fn build_explicit_order_eq_checks<'a>(
    order_list: &'a FieldOrderList,
    fields: &'a [OrdField],
) -> impl Iterator<Item = TokenStream> + 'a {
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

/// Builds implicit-order equality checks for struct fields.
pub fn build_implicit_eq_checks(fields: &[OrdField]) -> impl Iterator<Item = TokenStream> + '_ {
    fields.iter().enumerate().filter_map(move |(idx, field)| {
        if field.skip.is_present() {
            None
        } else {
            let access = field_access(field, idx);
            Some(build_eq_check(field, &FieldAccess::StructField(&access)))
        }
    })
}

/// Builds explicit-order comparison checks for struct fields using a `by = [...]` list.
pub fn build_explicit_order_cmp_checks<'a>(
    order_list: &'a FieldOrderList,
    fields: &'a [OrdField],
) -> impl Iterator<Item = TokenStream> + 'a {
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

/// Builds implicit-order comparison checks for struct fields.
pub fn build_implicit_cmp_checks(fields: &[OrdField]) -> impl Iterator<Item = TokenStream> + '_ {
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
