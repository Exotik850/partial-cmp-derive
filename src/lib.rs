//! # partial-cmp-derive
//!
//! A procedural macro crate for deriving `PartialOrd` and `Ord` with fine-grained
//! control over field comparison behavior.
//!
//! ## Features
//!
//! - **Skip fields**: Use `#[ord(skip)]` to exclude fields from comparison.
//!   When any field is skipped, only `PartialOrd` is implemented (not `Ord`),
//!   allowing skipped fields to contain types that don't implement `Ord` or `Eq` (like `f32`).
//! - **Sort order**: Use `#[ord(order = "asc")]` or `#[ord(order = "desc")]` per field
//! - **Explicit ordering**: Use `#[ord(by = [field1(desc), field2(asc)])]` at struct level
//! - **Field priority**: Use `#[ord(priority = N)]` for implicit ordering (lower = first)
//! - **Custom comparators**: Use `#[ord(compare_with = "path::to::fn")]`
//! - **Reverse all**: Use `#[ord(reverse)]` at struct level to reverse entire comparison
//! - **Enum ranking**: Use `#[ord(rank = N)]` to control variant ordering
//! - **Option handling**: Use `#[ord(none_order = "first")]` or `"last"`
//! - **Skip Ord**: Use `#[ord(skip_ord)]` to only implement `PartialOrd` without `Ord`
//!
//! ## Example
//!
//! ```rust
//! use partial_cmp_derive::PartialCmp;
//!
//! // When no fields are skipped, both PartialOrd and Ord are implemented
//! #[derive(PartialEq, Eq, PartialCmp)]
//! struct Point {
//!     x: i32,
//!     y: i32,
//! }
//!
//! // When fields are skipped, only PartialOrd is implemented
//! // This allows using types like f32 that don't implement Ord
//! #[derive(Debug, PartialEq, PartialCmp)]
//! struct Measurement {
//!     #[ord(skip)]
//!     raw_value: f32,  // f32 doesn't implement Ord, but it's skipped
//!     timestamp: u64,
//! }
//! ```
//!
//! This generates `PartialOrd` (and `Ord` when no fields are skipped) implementations
//! that compare fields in the specified order, ignoring skipped fields entirely.

mod expand;
mod types;

use darling::FromDeriveInput;
use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

use crate::expand::expand_derive;
use crate::types::OrdDerive;

/// Derives `PartialOrd` and `Ord` with customizable field comparison behavior.
///
/// # Struct-Level Attributes
///
/// - `#[ord(reverse)]` — Reverses the final comparison result
/// - `#[ord(by = [field1(asc), field2(desc)])]` — Explicit field comparison order
/// - `#[ord(skip_ord)]` — Only implement `PartialOrd`, not `Ord`
///
/// # Field-Level Attributes
///
/// - `#[ord(skip)]` — Exclude this field from comparison. **Note:** When any field
///   is skipped, only `PartialOrd` is implemented (not `Ord`), allowing skipped
///   fields to contain types that don't implement `Ord` or `Eq` (like `f32`).
/// - `#[ord(order = "asc")]` or `#[ord(order = "desc")]` — Sort direction
/// - `#[ord(priority = N)]` — Comparison priority (lower = compared first)
/// - `#[ord(compare_with = "path::to::fn")]` — Custom comparison function
/// - `#[ord(none_order = "first")]` or `#[ord(none_order = "last")]` — Option handling
///
/// # Variant-Level Attributes (Enums)
///
/// - `#[ord(rank = N)]` — Explicit variant ranking (lower = less than)
///
/// # Requirements
///
/// - The type must also derive or implement `PartialEq` (and `Eq` if `Ord` is generated)
/// - All compared fields must implement `Ord` (or provide `compare_with`)
/// - Skipped fields have no trait requirements
///
/// # Example
///
/// ```rust
/// use partial_cmp_derive::PartialCmp;
///
/// #[derive(PartialEq, Eq, PartialCmp)]
/// #[ord(by = [score(desc), name(asc)])]
/// struct Player {
///     id: u64,  // Not compared (not in `by` list)
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

    match expand_derive(&ord_derive) {
        Ok(tokens) => tokens.into(),
        Err(e) => e.write_errors().into(),
    }
}
