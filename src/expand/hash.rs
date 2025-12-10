use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::expand::common::{
    FieldAccess, field_access, generate_bindings, generate_named_bindings,
};
use crate::types::{FieldOrderList, OrdDerive, OrdField, OrdVariant, TraitConfig};

/// Expands the `Hash` implementation for the given input type.
///
/// Hashing respects the same field configuration as equality and ordering:
/// - Skipped fields are not hashed
/// - `key = "path::to::fn"` extracts a hashable key
/// - `none_order` toggles consistent `Option` hashing that aligns with equality semantics
pub fn expand_hash(
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
///
/// - When a `key` is specified, hashes the derived key instead
/// - For `Option`, hashes a discriminant plus the inner value when `Some`
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

/// Builds `Option` hash with consistent handling.
///
/// We hash a small discriminant to avoid collisions between `Some(v)` and `None`,
/// and then include the inner value when `Some`.
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

/// Builds hash statements for struct fields when an explicit `by = [...]` list is provided.
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

    let hash_stmts = order_list.0.iter().filter_map(|entry| {
        field_map.get(&entry.ident).map(|&(idx, field)| {
            let access = field_access(field, idx);
            build_hash_contribution(field, &FieldAccess::StructField(&access), config)
        })
    });

    quote! { #(#hash_stmts)* }
}

/// Builds hash statements for struct fields using implicit ordering (priority, then declaration order).
fn build_implicit_hash(fields: &[OrdField], config: &TraitConfig) -> TokenStream {
    let hash_stmts = fields.iter().enumerate().filter_map(|(idx, field)| {
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
    });

    quote! { #(#hash_stmts)* }
}

/// Builds `Hash` implementation body for enums.
///
/// - Hashes the discriminant via `core::mem::discriminant(self)`
/// - Then hashes each field of the current variant (excluding skipped fields)
pub fn expand_enum_hash(
    enum_name: &Ident,
    variants: &[OrdVariant],
    config: &TraitConfig,
) -> TokenStream {
    if variants.is_empty() {
        return quote! {};
    }

    let match_arms = variants
        .iter()
        .map(|v| build_variant_hash_arm(enum_name, v, config));

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

        let hash_stmts = fields
            .iter()
            .zip(bindings.iter())
            .filter(|(f, _)| !f.skip.is_present())
            .map(|(field, binding)| {
                let access = FieldAccess::Bindings {
                    self_id: binding,
                    other_id: binding, // Not used for hashing
                };
                build_hash_contribution(field, &access, config)
            });

        quote! { #pattern => { #(#hash_stmts)* } }
    } else {
        let field_idents: Vec<_> = fields.iter().filter_map(|f| f.ident.as_ref()).collect();
        let bindings = generate_named_bindings("self", &field_idents);

        let renames = field_idents
            .iter()
            .zip(&bindings)
            .map(|(orig, binding)| quote! { #orig: #binding });

        let pattern = quote! { #enum_name::#variant_name { #(#renames),* } };

        let hash_stmts = fields
            .iter()
            .zip(bindings.iter())
            .filter(|(f, _)| !f.skip.is_present())
            .map(|(field, binding)| {
                let access = FieldAccess::Bindings {
                    self_id: binding,
                    other_id: binding, // Not used for hashing
                };
                build_hash_contribution(field, &access, config)
            });

        quote! { #pattern => { #(#hash_stmts)* } }
    }
}
