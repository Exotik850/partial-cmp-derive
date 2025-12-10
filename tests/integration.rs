#![allow(dead_code)]
//! Integration tests for PartialCmp

use partial_cmp_derive::PartialCmp;
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// Helper function to compute the hash of a value.
fn compute_hash<T: Hash>(value: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

//=============================================================================
// Basic Struct Tests
//=============================================================================

#[test]
fn test_basic_struct_default_ordering() {
    #[derive(Debug, PartialCmp)]
    struct Point {
        x: i32,
        y: i32,
    }

    let a = Point { x: 1, y: 2 };
    let b = Point { x: 1, y: 3 };
    let c = Point { x: 2, y: 1 };

    // Test PartialEq
    assert_eq!(a, Point { x: 1, y: 2 });
    assert_ne!(a, b);

    // Test PartialOrd/Ord
    assert!(a < b); // Same x, compare y
    assert!(a < c); // x differs, x wins
    assert!(b < c);
}

#[test]
fn test_tuple_struct() {
    #[derive(Debug, PartialCmp)]
    struct Pair(#[ord(order = "desc")] u32, u32);

    let a = Pair(10, 1);
    let b = Pair(5, 1);
    let c = Pair(10, 2);

    // Test PartialEq
    assert_eq!(a, Pair(10, 1));
    assert_ne!(a, b);

    // Test ordering
    assert!(a < b); // First field desc: 10 > 5, so a < b
    assert!(a < c); // First equal, second asc: 1 < 2
}

#[test]
fn test_unit_struct() {
    #[derive(Debug, PartialCmp)]
    struct Unit;

    let a = Unit;
    let b = Unit;

    assert_eq!(a, b);
    assert_eq!(a.cmp(&b), Ordering::Equal);
}

//=============================================================================
// Skip Field Tests
//=============================================================================

#[test]
fn test_skip_field() {
    #[derive(Debug, PartialCmp)]
    struct Player {
        #[ord(skip)]
        id: u64,
        score: u32,
    }

    let a = Player { id: 1, score: 100 };
    let b = Player {
        id: 999,
        score: 100,
    };
    let c = Player { id: 1, score: 200 };

    // id is ignored in both eq and cmp
    assert_eq!(a, b); // Same score, different id - equal!
    assert!(a < c);
    assert_eq!(a.cmp(&b), Ordering::Equal);
}

#[test]
fn test_skip_field_tuple_struct() {
    #[derive(Debug, PartialCmp)]
    struct FloatPair(#[ord(skip)] f32, u32);

    let a = FloatPair(1.5, 10);
    let b = FloatPair(2.5, 20);
    let c = FloatPair(3.5, 10);

    // First field (f32) is skipped, only second field matters
    assert!(a < b);
    assert_eq!(a, c); // Same second field, different first - equal!
    assert_eq!(a.cmp(&c), Ordering::Equal);
}

#[test]
fn test_skip_all_fields() {
    #[derive(Debug, PartialCmp)]
    struct AllSkipped {
        #[ord(skip)]
        a: i32,
        #[ord(skip)]
        b: String,
    }

    let x = AllSkipped {
        a: 1,
        b: "hello".into(),
    };
    let y = AllSkipped {
        a: 999,
        b: "world".into(),
    };

    // All fields skipped, everything is equal
    assert_eq!(x, y);
    assert_eq!(x.cmp(&y), Ordering::Equal);
}

//=============================================================================
// Sort Order Tests
//=============================================================================

#[test]
fn test_descending_order() {
    #[derive(Debug, PartialCmp)]
    struct HighScore {
        #[ord(order = "desc")]
        score: u32,
        name: String,
    }

    let a = HighScore {
        score: 100,
        name: "Alice".into(),
    };
    let b = HighScore {
        score: 200,
        name: "Bob".into(),
    };

    // Higher score should come first (be "less")
    assert!(b < a);
    assert_ne!(a, b);
}

#[test]
fn test_explicit_field_order() {
    #[derive(Debug, PartialCmp)]
    #[ord(by = [priority(desc), created_at])]
    struct Task {
        id: u64,      // Not compared
        name: String, // Not compared
        priority: u8,
        created_at: u64,
    }

    let a = Task {
        id: 1,
        name: "A".into(),
        priority: 1,
        created_at: 100,
    };
    let b = Task {
        id: 2,
        name: "B".into(),
        priority: 2,
        created_at: 50,
    };
    let c = Task {
        id: 3,
        name: "C".into(),
        priority: 2,
        created_at: 100,
    };

    // b has higher priority (desc), so it comes first
    assert!(b < a);
    assert!(b < c); // Same priority, earlier created_at

    // Equality only considers fields in `by` list
    let d = Task {
        id: 999,
        name: "Different".into(),
        priority: 1,
        created_at: 100,
    };
    assert_eq!(a, d); // Same priority and created_at
}

#[test]
fn test_priority_attribute() {
    #[derive(Debug, PartialCmp)]
    struct Item {
        #[ord(priority = 1)]
        secondary: u32,
        #[ord(priority = 0)]
        primary: u32,
    }

    let a = Item {
        primary: 1,
        secondary: 100,
    };
    let b = Item {
        primary: 2,
        secondary: 1,
    };

    // primary is compared first despite declaration order
    assert!(a < b);
}

#[test]
fn test_reverse_all() {
    #[derive(Debug, PartialCmp)]
    #[ord(reverse)]
    struct Score(u32);

    let a = Score(100);
    let b = Score(200);

    // Normally a < b, but with reverse, a > b
    assert!(a > b);

    // Equality is not affected by reverse
    assert_eq!(a, Score(100));
}

//=============================================================================
// Key Extraction Tests
//=============================================================================

fn abs_key(v: &i32) -> i32 {
    v.abs()
}

#[test]
fn test_key_extraction() {
    #[derive(Debug, PartialCmp)]
    struct AbsValue {
        #[ord(key = "abs_key")]
        value: i32,
    }

    let a = AbsValue { value: -5 };
    let b = AbsValue { value: 5 };
    let c = AbsValue { value: 10 };

    // -5 and 5 have the same absolute value
    assert_eq!(a, b); // Equality derived from key
    assert!(a < c);
    assert!(b < c);
}

fn str_len(s: &String) -> usize {
    s.len()
}

#[test]
fn test_key_with_references() {
    #[derive(Debug, PartialCmp)]
    struct LenCompare {
        #[ord(key = "str_len")]
        text: String,
    }

    let a = LenCompare { text: "hi".into() };
    let b = LenCompare {
        text: "hello".into(),
    };
    let c = LenCompare { text: "ab".into() };

    assert!(a < b); // 2 < 5
    assert_eq!(a, c); // Both length 2
}

//=============================================================================
// Option Handling Tests
//=============================================================================

#[test]
fn test_none_order_first() {
    #[derive(Debug, PartialCmp)]
    struct MaybeValue {
        #[ord(none_order = "first")]
        value: Option<i32>,
    }

    let none = MaybeValue { value: None };
    let some1 = MaybeValue { value: Some(1) };
    let some2 = MaybeValue { value: Some(2) };

    // None comes first
    assert!(none < some1);
    assert!(none < some2);
    assert!(some1 < some2);

    // Equality
    assert_eq!(none, MaybeValue { value: None });
    assert_eq!(some1, MaybeValue { value: Some(1) });
    assert_ne!(none, some1);
}

#[test]
fn test_none_order_last() {
    #[derive(Debug, PartialCmp)]
    struct MaybeValue {
        #[ord(none_order = "last")]
        value: Option<i32>,
    }

    let none = MaybeValue { value: None };
    let some1 = MaybeValue { value: Some(1) };
    let some2 = MaybeValue { value: Some(2) };

    // None comes last
    assert!(some1 < none);
    assert!(some2 < none);
    assert!(some1 < some2);
}

//=============================================================================
// Enum Tests
//=============================================================================

#[test]
fn test_enum_basic() {
    #[derive(Debug, PartialCmp)]
    enum Status {
        Pending,
        InProgress,
        Completed,
    }

    assert!(Status::Pending < Status::InProgress);
    assert!(Status::InProgress < Status::Completed);

    assert_eq!(Status::Pending, Status::Pending);
    assert_ne!(Status::Pending, Status::Completed);
}

#[test]
fn test_enum_custom_rank() {
    #[derive(Debug, PartialCmp)]
    enum Priority {
        #[ord(rank = 2)]
        Low,
        #[ord(rank = 1)]
        Medium,
        #[ord(rank = 0)]
        High,
    }

    // High has lowest rank, so it comes first
    assert!(Priority::High < Priority::Medium);
    assert!(Priority::Medium < Priority::Low);
}

#[test]
fn test_enum_with_fields() {
    #[derive(Debug, PartialCmp)]
    enum Event {
        Click { x: i32, y: i32 },
        KeyPress { code: u32 },
    }

    let a = Event::Click { x: 0, y: 0 };
    let b = Event::Click { x: 1, y: 0 };
    let c = Event::KeyPress { code: 65 };

    assert!(a < b); // Same variant, compare fields
    assert!(a < c); // Click (rank 0) < KeyPress (rank 1)

    // Equality
    assert_eq!(a, Event::Click { x: 0, y: 0 });
    assert_ne!(a, b);
    assert_ne!(a, c);
}

#[test]
fn test_enum_tuple_variant() {
    #[derive(Debug, PartialCmp)]
    enum Value {
        Int(i32),
        Pair(i32, i32),
    }

    let a = Value::Int(5);
    let b = Value::Int(10);
    let c = Value::Pair(1, 2);
    let d = Value::Pair(1, 3);

    assert!(a < b);
    assert!(a < c); // Different variants
    assert!(c < d); // Same variant, compare fields

    assert_eq!(a, Value::Int(5));
    assert_eq!(c, Value::Pair(1, 2));
}

#[test]
fn test_enum_with_skip() {
    #[derive(Debug, PartialCmp)]
    enum Tagged {
        Value {
            #[ord(skip)]
            tag: String,
            data: i32,
        },
    }

    let a = Tagged::Value {
        tag: "first".into(),
        data: 10,
    };
    let b = Tagged::Value {
        tag: "second".into(),
        data: 10,
    };
    let c = Tagged::Value {
        tag: "third".into(),
        data: 20,
    };

    // tag is skipped
    assert_eq!(a, b); // Same data, different tag - equal!
    assert!(a < c);
}

//=============================================================================
// Trait Skip Tests
//=============================================================================

#[test]
fn test_skip_ord() {
    #[derive(Debug, PartialCmp)]
    #[ord(skip_ord)]
    struct Score {
        value: u32,
    }

    let a = Score { value: 10 };
    let b = Score { value: 20 };

    // PartialEq, Eq, PartialOrd are implemented
    assert_eq!(a, Score { value: 10 });
    assert!(a < b);
    assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));

    // Note: Ord is not implemented, so a.cmp(&b) would not compile
    // assert_eq!(a.cmp(&b), Ordering::Less);
}

#[test]
fn test_skip_eq_and_ord() {
    #[derive(Debug, PartialCmp)]
    #[ord(skip_eq, skip_ord)]
    struct PartialScore {
        value: u32,
    }

    let a = PartialScore { value: 10 };
    let b = PartialScore { value: 20 };

    // Only PartialEq and PartialOrd are implemented
    assert_eq!(a, PartialScore { value: 10 });
    assert!(a < b);
    assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));
}

#[test]
fn test_skip_partial_ord_and_ord() {
    #[derive(Debug, PartialCmp)]
    #[ord(skip_partial_ord)]
    struct EqOnly {
        value: u32,
    }

    let a = EqOnly { value: 10 };
    let b = EqOnly { value: 10 };
    let c = EqOnly { value: 20 };

    // Only PartialEq and Eq are implemented
    assert_eq!(a, b);
    assert_ne!(a, c);

    // Note: partial_cmp and cmp would not compile
}

#[test]
fn test_skip_key_combination() {
    fn abs_key(v: &i32) -> i32 {
        v.abs()
    }

    #[derive(Debug, PartialCmp)]
    struct AbsSkip {
        #[ord(skip)]
        id: u64,
        #[ord(key = "abs_key")]
        value: i32,
    }

    let a = AbsSkip { id: 1, value: -5 };
    let b = AbsSkip { id: 2, value: 5 };
    let c = AbsSkip { id: 3, value: 10 };

    // a and b are equal (same abs value), id is skipped
    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));

    // a and c are not equal
    assert_ne!(a, c);
}

//=============================================================================
// Complex Scenarios
//=============================================================================

#[test]
fn test_multiple_attributes() {
    #[derive(Debug, PartialCmp)]
    struct Complex {
        #[ord(skip)]
        id: u64,
        #[ord(priority = 0, order = "desc")]
        priority: u8,
        #[ord(priority = 1)]
        name: String,
    }

    let a = Complex {
        id: 1,
        priority: 10,
        name: "Alice".into(),
    };
    let b = Complex {
        id: 2,
        priority: 10,
        name: "Bob".into(),
    };
    let c = Complex {
        id: 3,
        priority: 5,
        name: "Alice".into(),
    };

    // id is skipped, priority compared first (desc), then name
    assert!(a < b); // Same priority, Alice < Bob
    assert!(a < c); // priority 10 > 5, so a comes before c (desc)
}

#[test]
fn test_generic_struct() {
    #[derive(Debug, PartialCmp)]
    struct Wrapper<T: Eq + Ord + Hash> {
        value: T,
    }

    let a = Wrapper { value: 5i32 };
    let b = Wrapper { value: 10i32 };

    assert!(a < b);
    assert_eq!(a, Wrapper { value: 5 });
}

#[test]
fn test_nested_structs() {
    #[derive(Debug, PartialCmp)]
    struct Inner {
        x: i32,
    }

    #[derive(Debug, PartialCmp)]
    struct Outer {
        inner: Inner,
        y: i32,
    }

    let a = Outer {
        inner: Inner { x: 1 },
        y: 2,
    };
    let b = Outer {
        inner: Inner { x: 1 },
        y: 3,
    };
    let c = Outer {
        inner: Inner { x: 2 },
        y: 1,
    };

    assert!(a < b); // Same inner, y differs
    assert!(a < c); // inner.x differs
    assert_eq!(
        a,
        Outer {
            inner: Inner { x: 1 },
            y: 2
        }
    );
}

#[test]
fn test_consistency_between_eq_and_ord() {
    // This is the key test: eq and ord must be consistent
    // when fields are skipped

    #[derive(Debug, PartialCmp)]
    struct Player {
        #[ord(skip)]
        id: u64,
        score: u32,
    }

    let a = Player { id: 1, score: 100 };
    let b = Player { id: 2, score: 100 };

    // If cmp returns Equal, eq must return true
    assert_eq!(a.cmp(&b), Ordering::Equal);
    assert_eq!(a, b);

    // And vice versa
    assert!(a == b);
    assert!(a.cmp(&b) == Ordering::Equal);

    // Also verify hash consistency
    assert_eq!(compute_hash(&a), compute_hash(&b));
}

#[test]
fn test_empty_struct() {
    #[derive(Debug, PartialCmp)]
    struct Empty {}

    let a = Empty {};
    let b = Empty {};

    assert_eq!(a, b);
    assert_eq!(a.cmp(&b), Ordering::Equal);
    assert_eq!(compute_hash(&a), compute_hash(&b));
}

#[test]
fn test_single_field() {
    #[derive(Debug, PartialCmp)]
    struct Single {
        value: i32,
    }

    let a = Single { value: 5 };
    let b = Single { value: 10 };

    assert!(a < b);
    assert_eq!(a, Single { value: 5 });
    assert_eq!(compute_hash(&a), compute_hash(&Single { value: 5 }));
}

//=============================================================================
// Hash Implementation Tests
//=============================================================================

#[test]
fn test_hash_basic_struct() {
    #[derive(Debug, PartialCmp)]
    struct Point {
        x: i32,
        y: i32,
    }

    let a = Point { x: 1, y: 2 };
    let b = Point { x: 1, y: 2 };
    let c = Point { x: 1, y: 3 };

    // Equal values must have equal hashes
    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));

    // Different values may have different hashes (not guaranteed, but likely)
    assert_ne!(a, c);
}

#[test]
fn test_hash_with_skip() {
    #[derive(Debug, PartialCmp)]
    struct Player {
        #[ord(skip)]
        id: u64,
        score: u32,
    }

    let a = Player { id: 1, score: 100 };
    let b = Player {
        id: 999,
        score: 100,
    };
    let c = Player { id: 1, score: 200 };

    // a and b are equal (id is skipped), so hashes must be equal
    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));

    // a and c are not equal
    assert_ne!(a, c);
}

#[test]
fn test_hash_explicit_field_order() {
    #[derive(Debug, PartialCmp)]
    #[ord(by = [priority, created_at])]
    struct Task {
        id: u64,      // Not compared or hashed
        name: String, // Not compared or hashed
        priority: u8,
        created_at: u64,
    }

    let a = Task {
        id: 1,
        name: "A".into(),
        priority: 1,
        created_at: 100,
    };
    let b = Task {
        id: 999,
        name: "Different".into(),
        priority: 1,
        created_at: 100,
    };

    // Equal because only priority and created_at are compared
    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));
}

#[test]
fn test_hash_with_key() {
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
    let c = AbsValue { value: 10 };

    // -5 and 5 have the same absolute value, so they're equal
    assert_eq!(a, b);
    // And their hashes must be equal (key is used consistently for Eq and Hash)
    assert_eq!(compute_hash(&a), compute_hash(&b));

    // 10 is different
    assert_ne!(a, c);
}

#[test]
fn test_hash_enum() {
    #[derive(Debug, PartialCmp)]
    enum Status {
        Pending,
        InProgress,
        Completed,
    }

    let a = Status::Pending;
    let b = Status::Pending;
    let c = Status::Completed;

    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));
    assert_ne!(a, c);
}

#[test]
fn test_hash_enum_with_fields() {
    #[derive(Debug, PartialCmp)]
    enum Event {
        Click { x: i32, y: i32 },
        KeyPress { code: u32 },
    }

    let a = Event::Click { x: 10, y: 20 };
    let b = Event::Click { x: 10, y: 20 };
    let c = Event::Click { x: 10, y: 30 };
    let d = Event::KeyPress { code: 65 };

    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));

    assert_ne!(a, c);
    assert_ne!(a, d);
}

#[test]
fn test_hash_enum_with_skip() {
    #[derive(Debug, PartialCmp)]
    enum Tagged {
        Value {
            #[ord(skip)]
            tag: String,
            data: i32,
        },
    }

    let a = Tagged::Value {
        tag: "first".into(),
        data: 10,
    };
    let b = Tagged::Value {
        tag: "second".into(),
        data: 10,
    };

    // Equal because tag is skipped
    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));
}

#[test]
fn test_hash_tuple_struct() {
    #[derive(Debug, PartialCmp)]
    struct Pair(u32, u32);

    let a = Pair(1, 2);
    let b = Pair(1, 2);
    let c = Pair(1, 3);

    assert_eq!(a, b);
    assert_eq!(compute_hash(&a), compute_hash(&b));
    assert_ne!(a, c);
}

#[test]
fn test_hash_none_order() {
    #[derive(Debug, PartialCmp)]
    struct MaybeValue {
        #[ord(none_order = "first")]
        value: Option<i32>,
    }

    let none1 = MaybeValue { value: None };
    let none2 = MaybeValue { value: None };
    let some1 = MaybeValue { value: Some(5) };
    let some2 = MaybeValue { value: Some(5) };

    assert_eq!(none1, none2);
    assert_eq!(compute_hash(&none1), compute_hash(&none2));

    assert_eq!(some1, some2);
    assert_eq!(compute_hash(&some1), compute_hash(&some2));

    assert_ne!(none1, some1);
}

#[test]
fn test_skip_hash() {
    // This test verifies that skip_hash prevents Hash from being generated
    // We can't easily test that Hash is NOT implemented, but we can test
    // that the struct works without Hash bounds

    #[derive(Debug, PartialCmp)]
    #[ord(skip_hash)]
    struct NoHash {
        value: i32,
    }

    let a = NoHash { value: 5 };
    let b = NoHash { value: 5 };

    // PartialEq and Eq still work
    assert_eq!(a, b);

    // Note: compute_hash(&a) would fail to compile because Hash is not implemented
    // That's the expected behavior when skip_hash is used
}

#[test]
fn test_hash_all_fields_skipped() {
    #[derive(Debug, PartialCmp)]
    struct AllSkipped {
        #[ord(skip)]
        a: i32,
        #[ord(skip)]
        b: String,
    }

    let x = AllSkipped {
        a: 1,
        b: "hello".into(),
    };
    let y = AllSkipped {
        a: 999,
        b: "world".into(),
    };

    // All fields skipped, so everything is equal
    assert_eq!(x, y);
    // And hashes must be equal
    assert_eq!(compute_hash(&x), compute_hash(&y));
}

#[test]
fn test_no_ord_no_hash() {
    #[derive(Debug, PartialCmp)]
    #[ord(skip_eq)]
    struct EqOnly {
        value: i32,
    }

    let a = EqOnly { value: 5 };
    let b = EqOnly { value: 5 };
    let c = EqOnly { value: 10 };

    // PartialEq and PartialOrd work
    assert_eq!(a, b);
    assert!(a < c);
    assert_ne!(a, c);

    // Note: a.cmp(&b) and compute_hash(&a) would not compile
    // That's the expected behavior when skip_ord and skip_hash are used
    // assert_eq!(compute_hash(&a), compute_hash(&b));
}
