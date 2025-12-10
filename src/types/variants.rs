use darling::FromVariant;
use darling::ast::Fields;
use darling::util::SpannedValue;
use syn::Ident;

use crate::types::OrdField;

/// Variant-level attributes for enums.
#[derive(Debug, Clone, FromVariant)]
#[darling(attributes(ord))]
pub struct OrdVariant {
    /// The variant identifier.
    pub ident: Ident,

    /// The variant's fields.
    pub fields: Fields<OrdField>,

    /// Explicit rank for this variant (lower = less than).
    #[darling(default)]
    pub rank: Option<SpannedValue<i32>>,
}

impl OrdVariant {
    /// Returns the effective rank for sorting variants.
    /// If an explicit rank is set, use it; otherwise use the declaration index.
    pub fn effective_rank(&self, declaration_index: usize) -> i32 {
        self.rank.as_ref().map_or(declaration_index as i32, |r| **r)
    }
}
