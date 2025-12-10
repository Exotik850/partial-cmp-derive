use darling::{Error, FromMeta};

/// The sort order for a field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SortOrder {
    #[default]
    Asc,
    Desc,
}

impl FromMeta for SortOrder {
    fn from_string(value: &str) -> darling::Result<Self> {
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
    fn from_string(value: &str) -> darling::Result<Self> {
        match value.to_lowercase().as_str() {
            "first" => Ok(NoneOrder::First),
            "last" => Ok(NoneOrder::Last),
            other => Err(Error::unknown_value(other)),
        }
    }
}
