//! Type definitions for darling attribute parsing.

use darling::{
    Error, FromDeriveInput, FromField, FromMeta, FromVariant, Result,
    ast::{Data, Fields},
    util::{Flag, SpannedValue},
};
use syn::{Expr, Ident, Path, Type};

/// The sort order for a field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SortOrder {
    #[default]
    Asc,
    Desc,
}

impl FromMeta for SortOrder {
    fn from_string(value: &str) -> Result<Self> {
        match value.to_lowercase().as_str() {
            "asc" | "ascending" => Ok(SortOrder::Asc),
            "desc" | "descending" => Ok(SortOrder::Desc),
            other => Err(Error::unknown_value(other)),
        }
    }
}

/// Controls where `None` values sort relative to `Some` values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NoneOrder {
    /// `None` sorts before all `Some` values
    First,
    /// `None` sorts after all `Some` values (default)
    #[default]
    Last,
}

impl FromMeta for NoneOrder {
    fn from_string(value: &str) -> Result<Self> {
        match value.to_lowercase().as_str() {
            "first" => Ok(NoneOrder::First),
            "last" => Ok(NoneOrder::Last),
            other => Err(Error::unknown_value(other)),
        }
    }
}

/// A single entry in the `by = [...]` list.
#[derive(Debug, Clone)]
pub struct FieldOrderEntry {
    pub ident: Ident,
    pub order: SortOrder,
}

/// A list of field ordering entries parsed from `#[ord(by = [field1(asc), field2(desc)])]`.
#[derive(Debug, Clone, Default)]
pub struct FieldOrderList(pub Vec<FieldOrderEntry>);

impl FromMeta for FieldOrderList {
    fn from_expr(expr: &Expr) -> Result<Self> {
        // Parse array expression: [field1(asc), field2(desc)]
        let Expr::Array(array) = expr else {
            return Err(
                Error::custom("expected array syntax: [field1(asc), field2(desc)]").with_span(expr),
            );
        };

        let mut entries = Vec::new();
        let mut errors = Error::accumulator();

        for elem in &array.elems {
            if let Some(entry) = errors.handle(parse_field_order_entry(elem)) {
                entries.push(entry);
            }
        }

        errors.finish_with(FieldOrderList(entries))
    }
}

fn get_ident_from_expr(expr: &Expr) -> Result<Ident> {
    if let Expr::Path(path) = expr {
        path.path
            .get_ident()
            .cloned()
            .ok_or_else(|| Error::custom("expected simple identifier").with_span(expr))
    } else {
        Err(Error::custom("expected identifier").with_span(expr))
    }
}

/// Parses a single entry like `field(asc)` or `field(desc)`.
fn parse_field_order_entry(expr: &Expr) -> Result<FieldOrderEntry> {
    if let Expr::Path(path) = expr {
        let ident = path
            .path
            .get_ident()
            .cloned()
            .ok_or_else(|| Error::custom("expected simple identifier").with_span(expr))?;
        return Ok(FieldOrderEntry {
            ident,
            order: SortOrder::Asc,
        });
    }

    let Expr::Call(call) = expr else {
        return Err(Error::custom("expected field(asc) or field(desc) syntax").with_span(expr));
    };

    // Get the field name
    let ident = get_ident_from_expr(&call.func)?;

    // Get the order argument
    if call.args.len() != 1 {
        return Err(Error::custom("expected exactly one argument: asc or desc").with_span(expr));
    }

    let arg = &call.args[0];
    let order_ident = get_ident_from_expr(arg)?;

    let order = match order_ident.to_string().as_str() {
        "asc" => SortOrder::Asc,
        "desc" => SortOrder::Desc,
        other => {
            return Err(
                Error::custom(format!("expected `asc` or `desc`, found `{other}`")).with_span(arg),
            );
        }
    };

    Ok(FieldOrderEntry { ident, order })
}

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

    /// Custom comparison function path for Ord (returns Ordering).
    pub compare_with: Option<SpannedValue<Path>>,

    /// Custom equality function path for PartialEq (returns bool).
    /// If not specified but compare_with is, equality is derived from compare_with.
    pub eq_with: Option<SpannedValue<Path>>,

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
}

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

/// Configuration for which traits to generate.
#[derive(Debug, Clone, Default)]
pub struct TraitConfig {
    /// Generate PartialEq implementation.
    pub partial_eq: bool,
    /// Generate Eq implementation.
    pub eq: bool,
    /// Generate PartialOrd implementation.
    pub partial_ord: bool,
    /// Generate Ord implementation.
    pub ord: bool,
}

impl TraitConfig {
    /// Creates a config with all traits enabled.
    pub fn all() -> Self {
        Self {
            partial_eq: true,
            eq: true,
            partial_ord: true,
            ord: true,
        }
    }
}

/// Top-level derive input.
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

    /// Skip PartialEq implementation.
    #[darling(default)]
    pub skip_partial_eq: Flag,

    /// Skip Eq implementation.
    #[darling(default)]
    pub skip_eq: Flag,

    /// Skip PartialOrd implementation.
    #[darling(default)]
    pub skip_partial_ord: Flag,

    /// Skip Ord implementation.
    #[darling(default)]
    pub skip_ord: Flag,

    /// Explicit field ordering.
    #[darling(default, rename = "by")]
    pub field_order: Option<FieldOrderList>,
}

impl OrdDerive {
    /// Returns the configuration for which traits to generate.
    pub fn trait_config(&self) -> TraitConfig {
        // Start with all traits enabled
        let mut config = TraitConfig::all();

        // Apply explicit skip flags
        if self.skip_partial_eq.is_present() {
            config.partial_eq = false;
            // Without PartialEq, we can't have Eq, PartialOrd, or Ord
            config.eq = false;
            config.partial_ord = false;
            config.ord = false;
        }

        if self.skip_eq.is_present() {
            config.eq = false;
            // Without Eq, we can't have Ord
            config.ord = false;
        }

        if self.skip_partial_ord.is_present() {
            config.partial_ord = false;
            // Without PartialOrd, we can't have Ord
            config.ord = false;
        }

        if self.skip_ord.is_present() {
            config.ord = false;
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
        fields: &Fields<OrdField>,
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
            let field_names: std::collections::HashSet<_> = fields
                .iter()
                .filter_map(|f| f.ident.as_ref().map(std::string::ToString::to_string))
                .collect();

            for entry in &order_list.0 {
                if !field_names.contains(&entry.ident.to_string()) {
                    errors.push(
                        Error::custom(format!("unknown field `{}`", entry.ident))
                            .with_span(&entry.ident),
                    );
                }
            }
        }

        // Check for conflicting skip and other attributes
        for field in fields.iter() {
            if field.skip.is_present() {
                if field.order.is_some() {
                    errors.push(
                        Error::custom("cannot use `order` on skipped fields")
                            .with_span(&field.skip.span()),
                    );
                }
                if field.priority.is_some() {
                    errors.push(
                        Error::custom("cannot use `priority` on skipped fields")
                            .with_span(&field.skip.span()),
                    );
                }
                if field.compare_with.is_some() {
                    errors.push(
                        Error::custom("cannot use `compare_with` on skipped fields")
                            .with_span(&field.skip.span()),
                    );
                }
                if field.eq_with.is_some() {
                    errors.push(
                        Error::custom("cannot use `eq_with` on skipped fields")
                            .with_span(&field.skip.span()),
                    );
                }
                if field.none_order.is_some() {
                    errors.push(
                        Error::custom("cannot use `none_order` on skipped fields")
                            .with_span(&field.skip.span()),
                    );
                }
            }
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
                if field.skip.is_present() {
                    if field.order.is_some() {
                        errors.push(
                            Error::custom("cannot use `order` on skipped fields")
                                .with_span(&field.skip.span()),
                        );
                    }
                    if field.eq_with.is_some() {
                        errors.push(
                            Error::custom("cannot use `eq_with` on skipped fields")
                                .with_span(&field.skip.span()),
                        );
                    }
                }
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
