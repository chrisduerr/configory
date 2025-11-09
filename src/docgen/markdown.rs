//! Rustdoc conversion to markdown.
//!
//! This module allows converting structs which implement [`Docgen`] into
//! markdown strings, for automatically documenting configurations files based
//! on Rustdoc attributes.
//!
//! # Example
//!
//! ```rust
//! use configory::docgen::Docgen;
//! use configory::docgen::markdown::Markdown;
//!
//! #[derive(Docgen, Default)]
//! struct Demo {
//!     /// Shows up in markdown.
//!     field: u8,
//! }
//!
//! // Convert our struct to a markdown `String`.
//! let mut formatter = Markdown::new();
//! formatter.set_heading_size(3);
//! let markdown = formatter.format::<Demo>();
//!
//! assert!(markdown.contains("Shows up in markdown"));
//! ```

use std::mem;

use crate::docgen::{DocType, Docgen, Table};

/// Markdown documentation formatter.
///
/// This struct allows taking a struct which implements [`Docgen`] and
/// transforming it into a markdown documentation.
///
/// # Example
///
/// ```rust
/// use configory::docgen::Docgen;
/// use configory::docgen::markdown::Markdown;
///
/// #[derive(Docgen, Default)]
/// struct Demo {
///     /// Shows up in markdown.
///     field: u8,
/// }
///
/// let markdown = Markdown::new().format::<Demo>();
/// assert!(markdown.contains("Shows up in markdown"));
/// ```
#[derive(Copy, Clone)]
pub struct Markdown {
    /// Number of `#` used for table headings.
    heading_size: usize,
}

impl Default for Markdown {
    fn default() -> Self {
        Self { heading_size: 1 }
    }
}

impl Markdown {
    /// Create a new markdown formatter.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the number of `#` prepended to each table name.
    ///
    /// The default is `1`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::docgen::markdown::Markdown;
    ///
    /// let mut markdown = Markdown::new();
    /// markdown.set_heading_size(1);
    /// ```
    pub fn set_heading_size(&mut self, size: u8) {
        self.heading_size = size as usize;
    }

    /// Get a type's documentation as markdown.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::docgen::Docgen;
    /// use configory::docgen::markdown::Markdown;
    ///
    /// #[derive(Docgen, Default)]
    /// struct Demo {
    ///     field: u8,
    /// }
    ///
    /// let markdown = Markdown::new().format::<Demo>();
    /// assert!(markdown.contains("field"));
    /// ```
    pub fn format<D: Docgen>(&self) -> String {
        let mut markdown = String::new();
        match D::doc_type() {
            DocType::Table(table) => {
                // Add root table documentation as header.
                if !table.doc.is_empty() {
                    markdown.push_str(&table.doc);
                    markdown.push_str("\n\n");
                }

                // Recurse into table fields.
                self.push_table_fields(&mut markdown, Vec::new(), &table);
            },
            DocType::Leaf(_) => panic!("config root must be a table"),
        }
        markdown
    }

    fn push_table_fields<'a>(
        &self,
        markdown: &mut String,
        parents: Vec<&'a str>,
        table: &'a Table,
    ) {
        // Push non-table fields as table columns.
        let mut has_leafs = false;
        for field in &table.fields {
            let leaf = match &field.doc_type {
                DocType::Leaf(leaf) => leaf,
                DocType::Table(_) => continue,
            };

            // Push field table header before the first field.
            if !mem::replace(&mut has_leafs, true) {
                markdown.push_str("|Name|Description|Type|Default|\n");
                markdown.push_str("|-|-|-|-|\n");
            }

            // Strip trailing `.` from doc unless it's multi-sentence.
            let doc = match field.doc.find('.') {
                index @ Some(_) if index == field.doc.rfind('.') => {
                    field.doc.strip_suffix('.').unwrap()
                },
                _ => &field.doc,
            };

            // Ensure newlines don't break tables, while allowing paragraphs to reflow.
            let mut escaped_doc = String::with_capacity(doc.len());
            let mut newline_count = 0;
            for c in doc.chars() {
                if c == '\n' {
                    newline_count += 1;
                } else {
                    match newline_count {
                        0 => (),
                        1 => escaped_doc.push(' '),
                        _ => escaped_doc.push_str("<br><br>"),
                    }
                    newline_count = 0;
                    escaped_doc.push(c);
                }
            }

            markdown.push('|');
            markdown.push_str(&field.ident);
            markdown.push('|');
            markdown.push_str(&escaped_doc);
            markdown.push('|');
            markdown.push_str(&leaf.type_name);
            markdown.push_str("|`");
            markdown.push_str(&field.default);
            markdown.push_str("`|\n");
        }

        // Push table fields as separate headings.
        for (i, field) in table.fields.iter().enumerate() {
            let table = match &field.doc_type {
                DocType::Table(table) => table,
                DocType::Leaf(_) => continue,
            };

            // Push separator if table has both leaf fields and subtables.
            if has_leafs || i > 0 {
                markdown.push('\n');
            }

            // Push table heading.
            let mut parents = parents.clone();
            parents.push(&field.ident);
            markdown.push_str(&"#".repeat(self.heading_size));
            markdown.push(' ');
            markdown.push_str(&parents.join("."));
            markdown.push_str("\n\n");

            // Push table documentation.
            if !field.doc.is_empty() {
                markdown.push_str(&field.doc);
                markdown.push_str("\n\n");
            }

            // Recurse into sub-fields.
            self.push_table_fields(markdown, parents, table);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate as configory;
    use crate::docgen::Leaf;

    #[allow(unused)]
    #[derive(Docgen)]
    struct Config {
        /// Text-based configuration option.
        field: String,
        no_doc: f64,
        /// Doc of a field with a leaf type.
        #[docgen(default = "19")]
        inline_child: CustomLeaf,
        /// Doc of a field with its own children.
        #[docgen(default = "")]
        nested_child: CustomTable,
        /// Second table.
        #[docgen(default = "")]
        second_table: CustomTable2,
    }

    impl Default for Config {
        fn default() -> Self {
            Self {
                field: "option1".into(),
                no_doc: 39.51,
                inline_child: Default::default(),
                nested_child: Default::default(),
                second_table: Default::default(),
            }
        }
    }

    #[derive(Default)]
    struct CustomLeaf;

    impl Docgen for CustomLeaf {
        fn doc_type() -> DocType {
            DocType::Leaf(Leaf::new("integer"))
        }

        fn format(&self) -> String {
            String::new()
        }
    }

    #[allow(unused)]
    #[derive(Docgen, Default)]
    struct CustomTable {
        /// Nested field with docs.
        nested_field: u8,
        /// Some custom table.
        #[docgen(default = "")]
        deep_nested: CustomTable2,
    }

    #[allow(unused)]
    #[derive(Docgen, Default)]
    struct CustomTable2 {
        /// Deep nested field with docs.
        deep_nested_field: u8,
    }

    #[test]
    fn format_markdown() {
        let expected = "\
|Name|Description|Type|Default|
|-|-|-|-|
|field|Text-based configuration option|text|`\"option1\"`|
|no_doc||float|`39.51`|
|inline_child|Doc of a field with a leaf type|integer|`19`|

# nested_child

Doc of a field with its own children.

|Name|Description|Type|Default|
|-|-|-|-|
|nested_field|Nested field with docs|integer|`0`|

# nested_child.deep_nested

Some custom table.

|Name|Description|Type|Default|
|-|-|-|-|
|deep_nested_field|Deep nested field with docs|integer|`0`|

# second_table

Second table.

|Name|Description|Type|Default|
|-|-|-|-|
|deep_nested_field|Deep nested field with docs|integer|`0`|
";

        let markdown = Markdown::new().format::<Config>();

        assert_eq(markdown, expected);
    }

    #[test]
    fn table_without_doc() {
        #[allow(unused)]
        #[derive(Docgen)]
        struct Test {
            #[docgen(default = "")]
            nested_child: CustomTable,
        }

        let expected = "\
# nested_child

|Name|Description|Type|Default|
|-|-|-|-|
|nested_field|Nested field with docs|integer|`0`|

# nested_child.deep_nested

Some custom table.

|Name|Description|Type|Default|
|-|-|-|-|
|deep_nested_field|Deep nested field with docs|integer|`0`|
";

        let markdown = Markdown::new().format::<Test>();

        assert_eq(markdown, expected);
    }

    #[test]
    fn multi_sentence_preserves_fullstop() {
        #[allow(unused)]
        #[derive(Docgen, Default)]
        struct Test {
            /// This is one sentence. This is another.
            test: u8,
        }

        let expected = "\
|Name|Description|Type|Default|
|-|-|-|-|
|test|This is one sentence. This is another.|integer|`0`|
";

        let markdown = Markdown::new().format::<Test>();

        assert_eq(markdown, expected);
    }

    #[test]
    fn two_toplevel_tables() {
        #[allow(unused)]
        #[derive(Docgen, Default)]
        struct Test {
            /// First table.
            first_table: CustomTable2,
            /// Second table.
            second_table: CustomTable2,
        }

        let expected = "\
# first_table

First table.

|Name|Description|Type|Default|
|-|-|-|-|
|deep_nested_field|Deep nested field with docs|integer|`0`|

# second_table

Second table.

|Name|Description|Type|Default|
|-|-|-|-|
|deep_nested_field|Deep nested field with docs|integer|`0`|
";

        let markdown = Markdown::new().format::<Test>();

        assert_eq(markdown, expected);
    }

    #[test]
    fn root_table_docs() {
        /// This is the root table documentation.
        ///
        /// I could put funny stuff here, like cat pictures or format
        /// documentation.
        #[allow(unused)]
        #[derive(Docgen, Default)]
        struct Test {
            /// Some random field.
            field: u8,
        }

        let expected = "\
This is the root table documentation.

I could put funny stuff here, like cat pictures or format
documentation.

|Name|Description|Type|Default|
|-|-|-|-|
|field|Some random field|integer|`0`|
";

        let markdown = Markdown::new().format::<Test>();

        assert_eq(markdown, expected);
    }

    #[test]
    fn multiline_field_doc() {
        #[allow(unused)]
        #[derive(Docgen, Default)]
        struct Test {
            /// Some random field.
            ///
            /// With loads of doc.
            /// And a multi-line paragraph.
            ///
            ///
            ///
            /// Third paragraph.
            field: u8,
        }

        #[rustfmt::skip]
        let expected = "\
|Name|Description|Type|Default|
|-|-|-|-|
|field|Some random field.<br><br>With loads of doc. And a multi-line paragraph.<br><br>Third paragraph.|integer|`0`|
";

        let markdown = Markdown::new().format::<Test>();

        assert_eq(markdown, expected);
    }

    #[test]
    fn option() {
        #[allow(unused)]
        #[derive(Docgen, Default)]
        struct Test {
            /// Some optional field.
            field: Option<u8>,
        }

        let expected = "\
|Name|Description|Type|Default|
|-|-|-|-|
|field|Some optional field|integer|`null`|
";

        let markdown = Markdown::new().format::<Test>();

        assert_eq(markdown, expected);
    }

    #[test]
    fn boxed() {
        #[allow(unused)]
        #[derive(Docgen, Default)]
        struct Test {
            /// Some boxed field.
            field: Box<u8>,
        }

        let expected = "\
|Name|Description|Type|Default|
|-|-|-|-|
|field|Some boxed field|integer|`0`|
";

        let markdown = Markdown::new().format::<Test>();

        assert_eq(markdown, expected);
    }

    fn assert_eq(markdown: String, expected: &'static str) {
        if markdown != expected {
            #[rustfmt::skip]
            panic!(
                "{0:->20}\n      EXPECTED\n{0:->20}\n\x1b[32m{expected}\x1b[0m{0:->20}\n\n\
                 {0:->20}\n      RECEIVED\n{0:->20}\n\x1b[31m{markdown}\x1b[0m{0:->20}",
                "-",
            );
        }
    }
}
