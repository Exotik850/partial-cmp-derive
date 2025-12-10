/// Configuration for which traits to generate in the derive macro.
///
/// This struct determines whether implementations for `PartialEq`, `Eq`,
/// `PartialOrd`, `Ord`, and `Hash` should be generated. It is typically
/// constructed via the parsed derive input and used by the code generation
/// routines to decide which impl blocks to emit.
#[derive(Debug, Clone, Default)]
pub struct TraitConfig {
    /// Generate `PartialEq` implementation.
    pub partial_eq: bool,
    /// Generate `Eq` implementation.
    pub eq: bool,
    /// Generate `PartialOrd` implementation.
    pub partial_ord: bool,
    /// Generate `Ord` implementation.
    pub ord: bool,
    /// Generate `Hash` implementation.
    pub hash: bool,
}

impl TraitConfig {
    /// Creates a configuration with all traits enabled.
    ///
    /// Returns a `TraitConfig` with:
    /// - `partial_eq = true`
    /// - `eq = true`
    /// - `partial_ord = true`
    /// - `ord = true`
    /// - `hash = true`
    pub fn all() -> Self {
        Self {
            partial_eq: true,
            eq: true,
            partial_ord: true,
            ord: true,
            hash: true,
        }
    }
}
