use darling::{
    Error, FromDeriveInput, Result,
    ast::Data,
    util::{Flag, SpannedValue},
};
use syn::Ident;

use crate::types::{FieldOrderList, OrdField, OrdVariant, TraitConfig};

/// Top-level derive input parsed from the procedural macro attributes.
#[derive(Debug, Clone, FromDeriveInput)]
#[darling(attributes(ord), supports(struct_any, enum_any), and_then = Self::validate)]
pub struct OrdDerive {
    /// The type identifier.
    pub ident: Ident,

    /// Generic parameters.
    pub generics: syn::Generics,

    /// The data (struct fields or enum variants).
    pub data: Data<OrdVariant, OrdField>,

    /// Reverse the entire comparison result.
    #[darling(default)]
    pub reverse: Flag,

    /// Skip `PartialEq` implementation.
    #[darling(default)]
    pub skip_partial_eq: Flag,

    /// Skip `Eq` implementation.
    #[darling(default)]
    pub skip_eq: Flag,

    /// Skip `PartialOrd` implementation.
    #[darling(default)]
    pub skip_partial_ord: Flag,

    /// Skip `Ord` implementation.
    #[darling(default)]
    pub skip_ord: Flag,

    /// Skip `Hash` implementation.
    #[darling(default)]
    pub skip_hash: Flag,

    /// Explicit field ordering via `#[ord(by = [field1(asc), field2(desc)])]`.
    #[darling(default, rename = "by")]
    pub field_order: Option<SpannedValue<FieldOrderList>>,
}

impl OrdDerive {
    /// Returns the configuration for which traits to generate based on skip flags.
    pub fn trait_config(&self) -> TraitConfig {
        // Start with all traits enabled
        let mut config = TraitConfig::all();

        // Apply explicit skip flags and cascading constraints
        if self.skip_partial_eq.is_present() {
            config.partial_eq = false;
            // Without PartialEq, we can't have Eq, PartialOrd, or Ord
            config.eq = false;
            config.partial_ord = false;
            config.ord = false;
        }

        if self.skip_eq.is_present() {
            config.eq = false;
            // Without Eq, we can't have Ord or Hash
            config.ord = false;
            config.hash = false;
        }

        if self.skip_partial_ord.is_present() {
            config.partial_ord = false;
            // Without PartialOrd, we can't have Ord
            config.ord = false;
        }

        if self.skip_ord.is_present() {
            config.ord = false;
        }

        if self.skip_hash.is_present() {
            config.hash = false;
        }

        config
    }

    /// Validates the derive input and returns accumulated errors.
    fn validate(self) -> Result<Self> {
        let mut errors = Error::accumulator();

        match &self.data {
            Data::Struct(fields) => {
                self.validate_struct_fields(fields, &mut errors);
            }
            Data::Enum(variants) => {
                self.validate_enum_variants(variants, &mut errors);
            }
        }

        errors.finish_with(self)
    }

    fn validate_struct_fields(
        &self,
        fields: &darling::ast::Fields<OrdField>,
        errors: &mut darling::error::Accumulator,
    ) {
        // Check for conflicts between field_order and field-level attributes
        if let Some(ref order_list) = self.field_order {
            let ordered_names: std::collections::HashSet<_> =
                order_list.0.iter().map(|e| e.ident.to_string()).collect();

            for field in fields.iter() {
                if let Some(ref ident) = field.ident {
                    let name = ident.to_string();

                    // Check for priority on fields when explicit ordering is used
                    if field.priority.is_some() && ordered_names.contains(&name) {
                        errors.push(
                            Error::custom(
                                "cannot use `priority` on fields when `by` is specified at struct level",
                            )
                            .with_span(ident),
                        );
                    }

                    // Check for order on fields when explicit ordering is used
                    if field.order.is_some() && ordered_names.contains(&name) {
                        errors.push(
                            Error::custom(
                                "cannot use `order` on fields listed in `by`; specify order in the `by` list instead",
                            )
                            .with_span(ident),
                        );
                    }
                }
            }

            // Check that all fields in order_list exist
            let field_names: std::collections::HashSet<_> =
                fields.iter().filter_map(|f| f.ident.clone()).collect();

            for entry in &order_list.0 {
                if !field_names.contains(&entry.ident) {
                    errors.push(
                        Error::custom(format!("unknown field `{}`", entry.ident))
                            .with_span(&entry.ident),
                    );
                }
            }
        }

        // Check for conflicting skip and other attributes
        for field in fields.iter() {
            field.check_skipped(errors);
        }
    }

    fn validate_enum_variants(
        &self,
        variants: &[OrdVariant],
        errors: &mut darling::error::Accumulator,
    ) {
        // Check for duplicate ranks
        let mut ranks: std::collections::HashMap<i32, &Ident> = std::collections::HashMap::new();

        for variant in variants {
            if let Some(ref rank) = variant.rank {
                let rank_value = **rank;
                if let Some(existing) = ranks.get(&rank_value) {
                    errors.push(
                        Error::custom(format!(
                            "duplicate rank {rank_value} (also used by `{existing}`)"
                        ))
                        .with_span(&variant.ident),
                    );
                } else {
                    ranks.insert(rank_value, &variant.ident);
                }
            }
        }

        // Validate fields within each variant
        for variant in variants {
            for field in variant.fields.iter() {
                field.check_skipped(errors);
            }
        }

        // Check that field_order is not used with enums
        if self.field_order.is_some() {
            errors.push(
                Error::custom("`by` attribute is not supported on enums").with_span(&self.ident),
            );
        }
    }
}
