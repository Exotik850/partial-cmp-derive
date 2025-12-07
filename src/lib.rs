//! # partial-cmp-derive
//!
//! A procedural macro crate for deriving `PartialOrd` and `Ord` with fine-grained
//! control over field comparison behavior.
//!
//! ## Features
//!
//! - **Skip fields**: Use `#[ord(skip)]` to exclude fields from comparison
//! - **Sort order**: Use `#[ord(order = "asc")]` or `#[ord(order = "desc")]` per field
//! - **Explicit ordering**: Use `#[ord(by = [field1(desc), field2(asc)])]` at struct level
//! - **Field priority**: Use `#[ord(priority = N)]` for implicit ordering (lower = first)
//! - **Custom comparators**: Use `#[ord(compare_with = "path::to::fn")]`
//! - **Reverse all**: Use `#[ord(reverse)]` at struct level to reverse entire comparison
//! - **Enum ranking**: Use `#[ord(rank = N)]` to control variant ordering
//! - **Option handling**: Use `#[ord(none_order = "first")]` or `"last"`
//!
//! ## Example
//!
//! ```rust
//! use partial_cmp_derive::PartialCmpDerive;
//!
//!  #[derive(PartialEq, Eq, PartialCmpDerive)]
//! struct Player {
//!      #[ord(skip)]
//!     id: u64,
//!     #[ord(order = "asc")]
//!     name: String,
//!      #[ord(order = "desc")]
//!     score: u32,
//! }
//!  ```
//!
//! This generates `PartialOrd` and `Ord` implementations that compare `name`
//! ascending, then `score` descending, while ignoring `id` entirely.

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
///
/// # Field-Level Attributes
///
/// - `#[ord(skip)]` — Exclude this field from comparison
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
/// - The type must also derive or implement `PartialEq` and `Eq`
/// - All compared fields must implement `Ord` (or provide `compare_with`)
///
/// # Example
///
/// ```rust
/// use partial_cmp_derive::PartialCmpDerive;
///
/// #[derive(PartialEq, Eq, PartialCmpDerive)]
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
