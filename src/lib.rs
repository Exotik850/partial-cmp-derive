//! # partial-cmp-derive
//!
//! A procedural macro crate for deriving `PartialEq`, `Eq`, `PartialOrd`, `Ord`, and `Hash`
//! with fine-grained control over field comparison and hashing behavior.
//!
//! ## Features
//!
//! - **Skip fields**: Use `#[ord(skip)]` to exclude fields from all comparisons and hashing.
//!   Skipped fields are ignored in equality, ordering, and hash computations.
//! - **Sort order**: Use `#[ord(order = "asc")]` or `#[ord(order = "desc")]` per field
//! - **Explicit ordering**: Use `#[ord(by = [field1(desc), field2(asc)])]` at struct level
//! - **Field priority**: Use `#[ord(priority = N)]` for implicit ordering (lower = first)
//! - **Key extraction**: Use `#[ord(key = "path::to::fn")]` to extract a comparable key
//! - **Reverse all**: Use `#[ord(reverse)]` at struct level to reverse entire comparison
//! - **Enum ranking**: Use `#[ord(rank = N)]` to control variant ordering
//! - **Option handling**: Use `#[ord(none_order = "first")]` or `"last"`
//! - **Trait selection**: Control which traits are generated with skip flags
//!
//! ## Trait Generation
//!
//! By default, all five traits are generated: `PartialEq`, `Eq`, `PartialOrd`, `Ord`, and `Hash`.
//! The `Hash` implementation is consistent with `Eq`, ensuring the invariant
//! `a == b -> hash(a) == hash(b)` holds. You can opt out of specific traits:
//!
//! - `#[ord(skip_partial_eq)]` — Don't generate `PartialEq` (implies no other traits)
//! - `#[ord(skip_eq)]` — Don't generate `Eq` (also disables `Ord`)
//! - `#[ord(skip_partial_ord)]` — Don't generate `PartialOrd` (also disables `Ord`)
//! - `#[ord(skip_ord)]` — Don't generate `Ord`
//! - `#[ord(skip_hash)]` — Don't generate `Hash`
//!
//! ## Example
//!
//! ```rust
//! use partial_cmp_derive::PartialCmp;
//!
//! // Generates PartialEq, Eq, PartialOrd, Ord, and Hash
//! #[derive(PartialCmp)]
//! struct Point {
//!     x: i32,
//!     y: i32,
//! }
//!
//! // Skipped fields are excluded from all comparisons and hashing
//! // Use skip_eq, skip_ord, and skip_hash when non-skipped fields don't implement those traits
//! #[derive(Debug, PartialCmp)]
//! #[ord(skip_eq, skip_ord, skip_hash)]
//! struct Measurement {
//!     #[ord(skip)]
//!     raw_value: f32,  // Ignored in eq, cmp, and hash
//!     timestamp: u64,
//! }
//!
//! // Compare by absolute value using a key function
//! #[derive(Debug, PartialCmp)]
//! struct AbsValue {
//!     #[ord(key = "abs_key")]
//!     value: i32,
//! }
//!
//! fn abs_key(v: &i32) -> i32 {
//!     v.abs()
//! }
//! ```
//!
//! This generates consistent implementations where skipped fields are ignored
//! in equality, ordering, and hash computations.
//!
//! ## Key Extraction
//!
//! The `key` attribute allows you to specify a function that extracts a comparable
//! value from a field. This single function is used for `Eq`, `Ord`, and `Hash`,
//! ensuring consistency across all three traits automatically:
//!
//! ```rust
//! use partial_cmp_derive::PartialCmp;
//!
//! fn abs_key(v: &i32) -> i32 {
//!     v.abs()
//! }
//!
//! #[derive(Debug, PartialCmp)]
//! struct AbsValue {
//!     #[ord(key = "abs_key")]
//!     value: i32,
//! }
//!
//! let a = AbsValue { value: -5 };
//! let b = AbsValue { value: 5 };
//!
//! // Both are equal because abs(-5) == abs(5)
//! assert_eq!(a, b);
//! // And their hashes are equal too (Hash/Eq invariant maintained)
//! ```
//!
//! The key function signature should be `fn(&T) -> U` where `U: Ord + Hash`.

mod expand;
mod types;

use darling::FromDeriveInput;
use proc_macro::TokenStream;

use syn::{DeriveInput, parse_macro_input};

use expand::cmp::expand_derive;
use types::OrdDerive;

/// Derives comparison and hash traits with customizable field behavior.
///
/// By default, this macro generates implementations for `PartialEq`, `Eq`,
/// `PartialOrd`, `Ord`, and `Hash`. All traits respect the same field configuration,
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
/// - `#[ord(skip_hash)]` — Don't generate `Hash`
///
/// # Field-Level Attributes
///
/// - `#[ord(skip)]` — Exclude this field from all comparisons and hashing
/// - `#[ord(order = "asc")]` or `#[ord(order = "desc")]` — Sort direction
/// - `#[ord(priority = N)]` — Comparison priority (lower = compared first)
/// - `#[ord(key = "path::to::fn")]` — Key extraction function (signature: `fn(&T) -> U`)
/// - `#[ord(none_order = "first")]` or `#[ord(none_order = "last")]` — Option handling
///
/// # Variant-Level Attributes (Enums)
///
/// - `#[ord(rank = N)]` — Explicit variant ranking (lower = less than)
///
/// # Key Extraction
///
/// The `key` attribute specifies a function that extracts a comparable/hashable value
/// from a field. The extracted key is used consistently for `Eq`, `Ord`, and `Hash`,
/// ensuring the invariant `a == b -> hash(a) == hash(b)` is maintained.
///
/// ```rust
/// use partial_cmp_derive::PartialCmp;
///
/// fn abs_key(v: &i32) -> i32 {
///     v.abs()
/// }
///
/// #[derive(Debug, PartialCmp)]
/// struct AbsValue {
///     #[ord(key = "abs_key")]
///     value: i32,
/// }
///
/// let a = AbsValue { value: -5 };
/// let b = AbsValue { value: 5 };
///
/// assert_eq!(a, b);  // Equal because abs(-5) == abs(5)
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
