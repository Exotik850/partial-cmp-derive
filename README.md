# partial-cmp-derive

A procedural macro for deriving `PartialEq`, `Eq`, `PartialOrd`, and `Ord` with fine-grained control over field comparison behavior.

## Features

- **Consistent trait generation**: All four comparison traits are generated with consistent behavior
- **Skip fields**: Exclude fields from all comparisons (both equality and ordering)
- **Custom comparators**: Use custom functions for ordering and/or equality
- **Sort order control**: Ascending or descending per field
- **Field priority**: Control comparison order independent of declaration order
- **Explicit field ordering**: Specify exactly which fields to compare and in what order
- **Option handling**: Control whether `None` sorts first or last
- **Enum ranking**: Control variant ordering independent of declaration order
- **Trait selection**: Opt out of specific traits as needed

## Installation

```toml
[dependencies]
partial-cmp-derive = "0.2"
```

## Quick Start

```rust
use partial_cmp_derive::PartialCmp;

// Generates PartialEq, Eq, PartialOrd, and Ord
#[derive(Debug, PartialCmp)]
struct Player {
    #[ord(skip)]           // Ignored in all comparisons
    id: u64,
    #[ord(order = "desc")] // Higher scores come first
    score: u32,
    name: String,          // Compared in ascending order (default)
}

let alice = Player { id: 1, score: 100, name: "Alice".into() };
let bob = Player { id: 2, score: 100, name: "Bob".into() };

// id is ignored, score compared first (desc), then name (asc)
assert!(alice < bob);  // Same score, Alice < Bob alphabetically

// Equality also ignores id
let alice2 = Player { id: 999, score: 100, name: "Alice".into() };
assert_eq!(alice, alice2);  // Same score and name, different id - equal!
```

## Attributes

### Struct-Level Attributes

| Attribute                                  | Description                                          |
| ------------------------------------------ | ---------------------------------------------------- |
| `#[ord(reverse)]`                          | Reverse the final comparison result                  |
| `#[ord(by = [field1(asc), field2(desc)])]` | Explicit field comparison order                      |
| `#[ord(skip_partial_eq)]`                  | Don't generate `PartialEq` (implies no other traits) |
| `#[ord(skip_eq)]`                          | Don't generate `Eq` (also disables `Ord`)            |
| `#[ord(skip_partial_ord)]`                 | Don't generate `PartialOrd` (also disables `Ord`)    |
| `#[ord(skip_ord)]`                         | Don't generate `Ord`                                 |

### Field-Level Attributes

| Attribute                               | Description                                         |
| --------------------------------------- | --------------------------------------------------- |
| `#[ord(skip)]`                          | Exclude from all comparisons                        |
| `#[ord(order = "asc"\|"desc")]`         | Sort direction (default: asc)                       |
| `#[ord(priority = N)]`                  | Comparison priority (lower = compared first)        |
| `#[ord(compare_with = "path::to::fn")]` | Custom comparison function `fn(&T, &T) -> Ordering` |
| `#[ord(eq_with = "path::to::fn")]`      | Custom equality function `fn(&T, &T) -> bool`       |
| `#[ord(none_order = "first"\|"last")]`  | Where `None` sorts for Option fields                |

### Enum Variant Attributes

| Attribute          | Description                         |
| ------------------ | ----------------------------------- |
| `#[ord(rank = N)]` | Variant ranking (lower = less than) |

## Examples

### Skipping Fields

Fields marked with `#[ord(skip)]` are excluded from both equality and ordering comparisons:

```rust
use partial_cmp_derive::PartialCmp;

#[derive(Debug, PartialCmp)]
struct Record {
    #[ord(skip)]
    internal_id: u64,  // Ignored in eq and cmp
    value: i32,
}

let a = Record { internal_id: 1, value: 10 };
let b = Record { internal_id: 2, value: 10 };

assert_eq!(a, b);  // Equal because only value is compared
```

### Explicit Field Ordering

Use `by` to specify exactly which fields participate in comparison:

```rust
use partial_cmp_derive::PartialCmp;

#[derive(Debug, PartialCmp)]
#[ord(by = [priority(desc), created_at(asc)])]
struct Task {
    id: u64,           // Not compared
    name: String,      // Not compared
    priority: u8,
    created_at: u64,
}
```

### Custom Comparison Functions

```rust
use partial_cmp_derive::PartialCmp;
use std::cmp::Ordering;

fn cmp_abs(a: &i32, b: &i32) -> Ordering {
    a.abs().cmp(&b.abs())
}

fn eq_abs(a: &i32, b: &i32) -> bool {
    a.abs() == b.abs()
}

#[derive(Debug, PartialCmp)]
struct AbsValue {
    #[ord(compare_with = "cmp_abs", eq_with = "eq_abs")]
    value: i32,
}

let a = AbsValue { value: -5 };
let b = AbsValue { value: 5 };

assert_eq!(a, b);  // Equal because |-5| == |5|
```

### Option Handling

```rust
use partial_cmp_derive::PartialCmp;

#[derive(Debug, PartialCmp)]
struct MaybeValue {
    #[ord(none_order = "first")]
    value: Option<i32>,
}

let none = MaybeValue { value: None };
let some = MaybeValue { value: Some(1) };

assert!(none < some);  // None comes first
```

### Enum Ranking

```rust
use partial_cmp_derive::PartialCmp;

#[derive(Debug, PartialCmp)]
enum Priority {
    #[ord(rank = 0)]
    High,
    #[ord(rank = 1)]
    Medium,
    #[ord(rank = 2)]
    Low,
}

assert!(Priority::High < Priority::Medium);
assert!(Priority::Medium < Priority::Low);
```

### Working with Non-Ord Types

For types like `f32` that don't implement `Ord`, use `skip_ord` and `skip_eq`:

```rust
use partial_cmp_derive::PartialCmp;
use std::cmp::Ordering;

fn cmp_f32(a: &f32, b: &f32) -> Ordering {
    a.partial_cmp(b).unwrap_or(Ordering::Equal)
}

#[derive(Debug, PartialCmp)]
#[ord(skip_eq, skip_ord)]
struct FloatWrapper {
    #[ord(compare_with = "cmp_f32")]
    value: f32,
}
```

## Trait Dependencies

The trait generation respects the following dependencies:

- `Ord` requires `Eq` and `PartialOrd`
- `Eq` and `PartialOrd` require `PartialEq`

When you skip a trait, dependent traits are automatically skipped:

| Skip Flag           | Traits Generated                       |
| ------------------- | -------------------------------------- |
| (none)              | `PartialEq`, `Eq`, `PartialOrd`, `Ord` |
| `skip_ord`          | `PartialEq`, `Eq`, `PartialOrd`        |
| `skip_eq`           | `PartialEq`, `PartialOrd`              |
| `skip_partial_ord`  | `PartialEq`, `Eq`                      |
| `skip_eq, skip_ord` | `PartialEq`, `PartialOrd`              |
| `skip_partial_eq`   | (none)                                 |

## License

MIT
