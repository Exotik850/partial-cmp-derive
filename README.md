# partial-cmp-derive

A procedural macro for deriving `PartialEq`, `Eq`, `PartialOrd`, `Ord`, and `Hash` with fine-grained control over field comparison and hashing behavior.

## Features

- **Consistent trait generation**: All five traits are generated with consistent behavior
- **Hash/Eq consistency**: The `Hash` implementation respects the same field configuration as `Eq`, ensuring `a == b -> hash(a) == hash(b)`
- **Skip fields**: Exclude fields from all comparisons and hashing
- **Key extraction**: Use a single function to extract comparable keys for `Eq`, `Ord`, and `Hash`
- **Sort order control**: Ascending or descending per field
- **Field priority**: Control comparison order independent of declaration order
- **Explicit field ordering**: Specify exactly which fields to compare and in what order
- **Option handling**: Control whether `None` sorts first or last
- **Enum ranking**: Control variant ordering independent of declaration order
- **Trait selection**: Opt out of specific traits as needed

## Installation

```toml
[dependencies]
partial-cmp-derive = "0.3"
```

## Quick Start

```rust
use partial_cmp_derive::PartialCmp;

// Generates PartialEq, Eq, PartialOrd, Ord, and Hash
#[derive(Debug, PartialCmp)]
struct Player {
    #[ord(skip)]           // Ignored in all comparisons and hashing
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
| `#[ord(skip_eq)]`                          | Don't generate `Eq` (also disables `Ord` and `Hash`)            |
| `#[ord(skip_partial_ord)]`                 | Don't generate `PartialOrd` (also disables `Ord`)    |
| `#[ord(skip_ord)]`                         | Don't generate `Ord`                                 |
| `#[ord(skip_hash)]`                        | Don't generate `Hash`                                |

### Field-Level Attributes

| Attribute                              | Description                                              |
| -------------------------------------- | -------------------------------------------------------- |
| `#[ord(skip)]`                         | Exclude from all comparisons and hashing                 |
| `#[ord(order = "asc"\|"desc")]`        | Sort direction (default: asc)                            |
| `#[ord(priority = N)]`                 | Comparison priority (lower = compared first)             |
| `#[ord(key = "path::to::fn")]`         | Key extraction function `fn(&T) -> U`                    |
| `#[ord(none_order = "first"\|"last")]` | Where `None` sorts for Option fields                     |

### Enum Variant Attributes

| Attribute          | Description                         |
| ------------------ | ----------------------------------- |
| `#[ord(rank = N)]` | Variant ranking (lower = less than) |

## Examples

### Skipping Fields

Fields marked with `#[ord(skip)]` are excluded from equality, ordering, and hash computations:

```rust
use partial_cmp_derive::PartialCmp;

#[derive(Debug, PartialCmp)]
struct Record {
    #[ord(skip)]
    internal_id: u64,  // Ignored in eq, cmp, and hash
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
    id: u64,           // Not compared or hashed
    name: String,      // Not compared or hashed
    priority: u8,
    created_at: u64,
}
```

### Key Extraction

The `key` attribute allows you to specify a function that extracts a comparable value from a field. This single function is used for `Eq`, `Ord`, and `Hash`, ensuring consistency automatically:

```rust
use partial_cmp_derive::PartialCmp;

fn abs_key(v: &i32) -> i32 {
    v.abs()
}

#[derive(Debug, PartialCmp)]
struct AbsValue {
    #[ord(key = "abs_key")]
    value: i32,
}

let a = AbsValue { value: -5 };
let b = AbsValue { value: 5 };

assert_eq!(a, b);  // Equal because abs(-5) == abs(5)
// And their hashes are also equal, maintaining the Hash/Eq invariant
```

The key function signature should be `fn(&T) -> U` where `U: Ord + Hash`.

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

For types like `f32` that don't implement `Ord` or `Hash`, you can use a key function to convert them, or skip the relevant traits:

```rust
use partial_cmp_derive::PartialCmp;

// Convert f32 to ordered bits for comparison
fn f32_key(v: &f32) -> i32 {
    let bits = v.to_bits() as i32;
    if bits < 0 { !bits } else { bits }
}

#[derive(Debug, PartialCmp)]
struct FloatWrapper {
    #[ord(key = "f32_key")]
    value: f32,
}
```

Or skip traits that aren't needed:

```rust
use partial_cmp_derive::PartialCmp;

#[derive(Debug, PartialCmp)]
#[ord(skip_eq, skip_ord, skip_hash)]  // Only generate PartialEq and PartialOrd
struct PartialFloat {
    value: f32,  // Uses default PartialEq/PartialOrd
}
```

## Trait Dependencies

The trait generation respects the following dependencies:

- `Ord` requires `Eq` and `PartialOrd`
- `Eq` and `PartialOrd` require `PartialEq`
- `Hash` is independent but should be consistent with `Eq`

When you skip a trait, dependent traits are automatically skipped:

| Skip Flag           | Traits Generated                                |
| ------------------- | ----------------------------------------------- |
| (none)              | `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`  |
| `skip_ord`          | `PartialEq`, `Eq`, `PartialOrd`, `Hash`         |
| `skip_eq`           | `PartialEq`, `PartialOrd`, `Hash`               |
| `skip_partial_ord`  | `PartialEq`, `Eq`, `Hash`                       |
| `skip_hash`         | `PartialEq`, `Eq`, `PartialOrd`, `Ord`          |
| `skip_eq, skip_ord` | `PartialEq`, `PartialOrd`, `Hash`               |
| `skip_partial_eq`   | (none)                                          |

## License

MIT
