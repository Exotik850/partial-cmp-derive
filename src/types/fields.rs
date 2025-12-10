use darling::util::{Flag, SpannedValue};
use darling::{Error, FromField, error::Accumulator};
use syn::{Ident, Path, Type};

use crate::types::{NoneOrder, SortOrder};

/// Field-level attributes.
#[derive(Debug, Clone, FromField)]
#[darling(attributes(ord))]
pub struct OrdField {
    /// The field identifier (None for tuple struct fields).
    pub ident: Option<Ident>,

    /// The field type (magic field populated by darling for future use).
    #[allow(dead_code)]
    pub ty: Type,

    /// Skip this field from comparison entirely.
    #[darling(default)]
    pub skip: Flag,

    /// Sort order for this field.
    pub order: Option<SortOrder>,

    /// Priority for comparison ordering (lower = compared first).
    pub priority: Option<SpannedValue<i32>>,

    /// Key extraction function path for comparison and hashing.
    /// The function should have signature `fn(&T) -> U`.
    /// The extracted key is used for `Eq`, `Ord`, and `Hash` implementations.
    pub key: Option<SpannedValue<Path>>,

    /// How to handle None values for Option fields.
    pub none_order: Option<NoneOrder>,
}

impl OrdField {
    /// Returns the effective sort order, defaulting to Asc.
    pub fn effective_order(&self) -> SortOrder {
        self.order.unwrap_or_default()
    }

    /// Returns the effective priority for sorting fields.
    pub fn effective_priority(&self) -> i32 {
        self.priority.as_ref().map_or(i32::MAX, |p| **p)
    }

    /// Validates that no conflicting attributes are used on skipped fields.
    pub fn check_skipped(&self, errors: &mut Accumulator) {
        if !self.skip.is_present() {
            return;
        }

        let conflicts = [
            ("order", self.order.is_some()),
            ("priority", self.priority.is_some()),
            ("key", self.key.is_some()),
            ("none_order", self.none_order.is_some()),
        ];

        for (name, present) in conflicts {
            if present {
                errors.push(
                    Error::custom(format!("cannot use `{name}` on skipped fields"))
                        .with_span(&self.skip.span()),
                );
            }
        }
    }
}
