#![allow(unused)]

use std::time::Duration;

use configory::docgen::{DocType, Docgen, Field, Leaf, Table};

/// Root table documentation.
#[derive(Docgen)]
struct Config {
    /// Text-based configuration option.
    field: String,
    no_doc: f64,
    /// Doc of a field with a leaf type.
    #[docgen(default = "\"blub\"")]
    inline_child: CustomLeaf,
    /// Doc of a field with its own children.
    nested_child: CustomTable,
    /// This enum derives Docgen.
    inline_enum: LeafEnum,
    /// Doesn't matter, it's ignored.
    #[docgen(skip)]
    ignored: u8,
    /// This field has a documentation heading.
    ///
    /// But it also has additional documentation paragraphs.
    multiline: u8,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            inline_child: CustomLeaf,
            field: "demo".into(),
            no_doc: 19.71,
            nested_child: Default::default(),
            inline_enum: Default::default(),
            multiline: Default::default(),
            ignored: Default::default(),
        }
    }
}

struct CustomLeaf;

impl Docgen for CustomLeaf {
    fn doc_type() -> DocType {
        DocType::Leaf(Leaf::new("CustomLeaf"))
    }

    fn format(&self) -> String {
        String::new()
    }
}

#[derive(Docgen)]
struct CustomTable {
    /// Nested field with docs.
    nested_field: u8,
    /// Field doc type override.
    #[docgen(doc_type = "milliseconds", default = "300")]
    duration: Duration,
}

impl Default for CustomTable {
    fn default() -> Self {
        Self { nested_field: 3, duration: Duration::from_millis(300) }
    }
}

#[derive(Docgen, Debug, Default)]
#[docgen(doc_type = "LeafEnum")]
enum LeafEnum {
    #[default]
    X,
}

#[test]
fn derive_docgen() {
    let mut table = Table::new("Root table documentation.");
    table.push(Field {
        doc: "Text-based configuration option.".into(),
        doc_type: DocType::Leaf(Leaf::new("text")),
        default: "\"demo\"".into(),
        ident: "field".into(),
    });
    table.push(Field {
        doc_type: DocType::Leaf(Leaf::new("float")),
        default: "19.71".into(),
        ident: "no_doc".into(),
        doc: "".into(),
    });
    table.push(Field {
        doc_type: DocType::Leaf(Leaf::new("CustomLeaf")),
        doc: "Doc of a field with a leaf type.".into(),
        ident: "inline_child".into(),
        default: "\"blub\"".into(),
    });
    let mut nested_table = Table::new("");
    nested_table.push(Field {
        doc_type: DocType::Leaf(Leaf::new("integer")),
        doc: "Nested field with docs.".into(),
        ident: "nested_field".into(),
        default: "3".into(),
    });
    nested_table.push(Field {
        doc_type: DocType::Leaf(Leaf::new("milliseconds")),
        doc: "Field doc type override.".into(),
        ident: "duration".into(),
        default: "300".into(),
    });
    table.push(Field {
        doc: "Doc of a field with its own children.".into(),
        doc_type: DocType::Table(nested_table),
        ident: "nested_child".into(),
        default: "".into(),
    });
    table.push(Field {
        doc_type: DocType::Leaf(Leaf::new("LeafEnum")),
        doc: "This enum derives Docgen.".into(),
        ident: "inline_enum".into(),
        default: "X".into(),
    });
    table.push(Field {
        doc_type: DocType::Leaf(Leaf::new("integer")),
        doc: "This field has a documentation heading.\n\nBut it also has additional documentation \
              paragraphs."
            .into(),
        ident: "multiline".into(),
        default: "0".into(),
    });
    let expected = DocType::Table(table);

    let doc_type = Config::doc_type();
    assert_eq!(doc_type, expected);
}
