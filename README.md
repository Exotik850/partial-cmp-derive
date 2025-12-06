# partial-cmp-derive

A procedural macro for deriving `PartialOrd` and `Ord` with fine-grained control over field comparison.

## Installation

```toml
[dependencies]
partial-cmp-derive = "0.1"
```

## Usage

```rust
use partial_cmp_derive::PartialCmpDerive;

#[derive(PartialEq, Eq, PartialCmpDerive)]
struct Player {
    #[ord(skip)]
    id: u64,
    #[ord(order = "asc")]
    name: String,
    #[ord(order = "desc")]
    score: u32,
}
```

## Attributes

### Struct-Level

- `#[ord(reverse)]` — Reverse the final comparison result
- `#[ord(by = [field1(asc), field2(desc)])]` — Explicit field comparison order

### Field-Level

- `#[ord(skip)]` — Exclude field from comparison
- `#[ord(order = "asc"|"desc")]` — Sort direction
- `#[ord(priority = N)]` — Comparison priority (lower = first)
- `#[ord(compare_with = "path::to::fn")]` — Custom comparator
- `#[ord(none_order = "first"|"last")]` — Option handling

### Enum Variants

- `#[ord(rank = N)]` — Variant ranking (lower = less than)

## License

MIT
