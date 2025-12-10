//! # partial-cmp-derive
//!
//! A procedural macro crate for deriving `PartialEq`, `Eq`, `PartialOrd`, and `Ord`
//! with fine-grained control over field comparison behavior.
//!
//! ## Features
//!
//! - **Skip fields**: Use `#[ord(skip)]` to exclude fields from all comparisons.
//!   Skipped fields are ignored in both equality and ordering checks.
//! - **Sort order**: Use `#[ord(order = "asc")]` or `#[ord(order = "desc")]` per field
//! - **Explicit ordering**: Use `#[ord(by = [field1(desc), field2(asc)])]` at struct level
//! - **Field priority**: Use `#[ord(priority = N)]` for implicit ordering (lower = first)
//! - **Custom comparators**: Use `#[ord(compare_with = "path::to::fn")]` for ordering
//! - **Custom equality**: Use `#[ord(eq_with = "path::to::fn")]` for equality checks
//! - **Reverse all**: Use `#[ord(reverse)]` at struct level to reverse entire comparison
//! - **Enum ranking**: Use `#[ord(rank = N)]` to control variant ordering
//! - **Option handling**: Use `#[ord(none_order = "first")]` or `"last"`
//! - **Trait selection**: Control which traits are generated with skip flags
//!
//! ## Trait Generation
//!
//! By default, all four comparison traits are generated: `PartialEq`, `Eq`,
//! `PartialOrd`, and `Ord`. You can opt out of specific traits:
//!
//! - `#[ord(skip_partial_eq)]` — Don't generate `PartialEq` (implies no other traits)
//! - `#[ord(skip_eq)]` — Don't generate `Eq` (implies no `Ord`)
//! - `#[ord(skip_partial_ord)]` — Don't generate `PartialOrd` (implies no `Ord`)
//! - `#[ord(skip_ord)]` — Don't generate `Ord`
//!
//! ## Example
//!
//! ```rust
//! use partial_cmp_derive::PartialCmp;
//!
//! // Generates PartialEq, Eq, PartialOrd, and Ord
//! #[derive(PartialCmp)]
//! struct Point {
//!     x: i32,
//!     y: i32,
//! }
//!
//! // Skipped fields are excluded from all comparisons
//! // Use skip_eq and skip_ord when non-skipped fields don't implement Eq/Ord
//! #[derive(Debug, PartialCmp)]
//! #[ord(skip_eq, skip_ord)]
//! struct Measurement {
//!     #[ord(skip)]
//!     raw_value: f32,  // Ignored in eq and cmp
//!     timestamp: u64,
//! }
//!
//! // Only generate PartialEq and PartialOrd (for types like f32)
//! #[derive(Debug, PartialCmp)]
//! #[ord(skip_eq, skip_ord)]
//! struct PartialOnly {
//!     #[ord(compare_with = "cmp_f32")]
//!     value: f32,
//! }
//!
//! fn cmp_f32(a: &f32, b: &f32) -> std::cmp::Ordering {
//!     a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
//! }
//! ```
//!
//! This generates consistent implementations where skipped fields are ignored
//! in both equality and ordering comparisons.

mod expand;
mod types;

use darling::FromDeriveInput;
use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

use crate::expand::expand_derive;
use crate::types::OrdDerive;

/// Derives comparison traits with customizable field behavior.
///
/// By default, this macro generates implementations for `PartialEq`, `Eq`,
/// `PartialOrd`, and `Ord`. All traits respect the same field configuration,
/// ensuring consistent behavior.
///
/// # Struct-Level Attributes
///
/// - `#[ord(reverse)]` — Reverses the final comparison result
/// - `#[ord(by = [field1(asc), field2(desc)])]` — Explicit field comparison order
/// - `#[ord(skip_partial_eq)]` — Don't generate `PartialEq` (disables all other traits too)
/// - `#[ord(skip_eq)]` — Don't generate `Eq` (also disables `Ord`)
/// - `#[ord(skip_partial_ord)]` — Don't generate `PartialOrd` (also disables `Ord`)
/// - `#[ord(skip_ord)]` — Don't generate `Ord`
///
/// # Field-Level Attributes
///
/// - `#[ord(skip)]` — Exclude this field from all comparisons (both eq and ord)
/// - `#[ord(order = "asc")]` or `#[ord(order = "desc")]` — Sort direction
/// - `#[ord(priority = N)]` — Comparison priority (lower = compared first)
/// - `#[ord(compare_with = "path::to::fn")]` — Custom comparison function for ordering
///   (signature: `fn(&T, &T) -> Ordering`)
/// - `#[ord(eq_with = "path::to::fn")]` — Custom equality function
///   (signature: `fn(&T, &T) -> bool`)
/// - `#[ord(none_order = "first")]` or `#[ord(none_order = "last")]` — Option handling
///
/// # Variant-Level Attributes (Enums)
///
/// - `#[ord(rank = N)]` — Explicit variant ranking (lower = less than)
///
/// # Custom Comparison Functions
///
/// When using `compare_with`, you can optionally provide `eq_with` for a custom
/// equality check. If `eq_with` is not provided, equality is derived from the
/// comparison function (equal when `compare_with` returns `Ordering::Equal`).
///
/// ```rust
/// use partial_cmp_derive::PartialCmp;
/// use std::cmp::Ordering;
///
/// fn cmp_abs(a: &i32, b: &i32) -> Ordering {
///     a.abs().cmp(&b.abs())
/// }
///
/// fn eq_abs(a: &i32, b: &i32) -> bool {
///     a.abs() == b.abs()
/// }
///
/// #[derive(PartialCmp)]
/// struct AbsValue {
///     #[ord(compare_with = "cmp_abs", eq_with = "eq_abs")]
///     value: i32,
/// }
/// ```
///
/// # Example
///
/// ```rust
/// use partial_cmp_derive::PartialCmp;
///
/// #[derive(Debug, PartialCmp)]
/// #[ord(by = [score(desc), name(asc)])]
/// struct Player {
///     id: u64,      // Not compared (not in `by` list)
///     name: String,
///     score: u32,
/// }
/// ```
#[proc_macro_derive(PartialCmp, attributes(ord))]
pub fn partial_cmp_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ord_derive = match OrdDerive::from_derive_input(&input) {
        Ok(v) => v,
        Err(e) => return e.write_errors().into(),
    };

    expand_derive(&ord_derive).into()
}
