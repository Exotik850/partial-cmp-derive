use darling::{Error, FromMeta, Result};
use syn::{Expr, Ident};

/// A single entry in the `by = [...]` list.
#[derive(Debug, Clone)]
pub struct FieldOrderEntry {
    pub ident: Ident,
    pub order: crate::types::SortOrder,
}

/// A list of field ordering entries parsed from `#[ord(by = [field1(asc), field2(desc)])]`.
#[derive(Debug, Clone, Default)]
pub struct FieldOrderList(pub Vec<FieldOrderEntry>);

impl FromMeta for FieldOrderList {
    fn from_expr(expr: &Expr) -> Result<Self> {
        // Parse array expression: [field1(asc), field2(desc)]
        let Expr::Array(array) = expr else {
            return Err(
                Error::custom("expected array syntax: [field1(asc), field2(desc)]").with_span(expr),
            );
        };

        let mut entries = Vec::new();
        let mut errors = Error::accumulator();

        for elem in &array.elems {
            if let Some(entry) = errors.handle(parse_field_order_entry(elem)) {
                entries.push(entry);
            }
        }

        errors.finish_with(FieldOrderList(entries))
    }
}

fn get_ident_from_expr(expr: &Expr) -> Result<Ident> {
    if let Expr::Path(path) = expr {
        path.path
            .get_ident()
            .cloned()
            .ok_or_else(|| Error::custom("expected simple identifier").with_span(expr))
    } else {
        Err(Error::custom("expected identifier").with_span(expr))
    }
}

/// Parses a single entry like `field(asc)` or `field(desc)`, or bare `field` (defaults to asc).
fn parse_field_order_entry(expr: &Expr) -> Result<FieldOrderEntry> {
    // Bare identifier: `field` (defaults to asc)
    if let Expr::Path(path) = expr {
        let ident = path
            .path
            .get_ident()
            .cloned()
            .ok_or_else(|| Error::custom("expected simple identifier").with_span(expr))?;
        return Ok(FieldOrderEntry {
            ident,
            order: crate::types::SortOrder::Asc,
        });
    }

    // Call syntax: `field(asc | desc)`
    let Expr::Call(call) = expr else {
        return Err(Error::custom("expected field(asc) or field(desc) syntax").with_span(expr));
    };

    // Get the field name
    let ident = get_ident_from_expr(&call.func)?;

    // Get the order argument
    if call.args.len() != 1 {
        return Err(Error::custom("expected exactly one argument: asc or desc").with_span(expr));
    }

    let arg = &call.args[0];
    let order_ident = get_ident_from_expr(arg)?;

    let order = match order_ident.to_string().as_str() {
        "asc" => crate::types::SortOrder::Asc,
        "desc" => crate::types::SortOrder::Desc,
        other => {
            return Err(
                Error::custom(format!("expected `asc` or `desc`, found `{other}`")).with_span(arg),
            );
        }
    };

    Ok(FieldOrderEntry { ident, order })
}
