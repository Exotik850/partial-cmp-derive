//! Integration tests for PartialCmpDerive

use partial_cmp_derive::PartialCmpDerive;

#[test]
fn test_basic_struct_default_ordering() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
    struct Point {
        x: i32,
        y: i32,
    }

    let a = Point { x: 1, y: 2 };
    let b = Point { x: 1, y: 3 };
    let c = Point { x: 2, y: 1 };

    assert!(a < b); // Same x, compare y
    assert!(a < c); // x differs, x wins
    assert!(b < c);
}

#[test]
fn test_skip_field() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
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

    // id is ignored, only score matters
    assert_eq!(a.cmp(&b), std::cmp::Ordering::Equal);
    assert!(a < c);
}

#[test]
fn test_descending_order() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
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
}

#[test]
fn test_explicit_field_order() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
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
}

#[test]
fn test_priority_attribute() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
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
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
    #[ord(reverse)]
    struct Score(u32);

    let a = Score(100);
    let b = Score(200);

    // Normally a < b, but with reverse, a > b
    assert!(a > b);
}

#[test]
fn test_enum_basic() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
    enum Status {
        Pending,
        InProgress,
        Completed,
    }

    assert!(Status::Pending < Status::InProgress);
    assert!(Status::InProgress < Status::Completed);
}

#[test]
fn test_enum_custom_rank() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
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
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
    enum Event {
        Click { x: i32, y: i32 },
        KeyPress { code: u32 },
    }

    let a = Event::Click { x: 0, y: 0 };
    let b = Event::Click { x: 1, y: 0 };
    let c = Event::KeyPress { code: 65 };

    assert!(a < b); // Same variant, compare fields
    assert!(a < c); // Click (rank 0) < KeyPress (rank 1)
}

#[test]
fn test_tuple_struct() {
    #[derive(Debug, PartialEq, Eq, PartialCmpDerive)]
    struct Pair(#[ord(order = "desc")] u32, u32);

    let a = Pair(10, 1);
    let b = Pair(5, 1);
    let c = Pair(10, 2);

    assert!(a < b); // First field desc: 10 > 5, so a < b
    assert!(a < c); // First equal, second asc: 1 < 2
}
