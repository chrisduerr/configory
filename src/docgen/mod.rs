//! Rustdoc config documentation generation.
//!
//! The [`Docgen`] crate allows automatically parsing a struct's fields and doc
//! attributes to format into documentation using formatters like [`Markdown`].
//!
//! [`Markdown`]: crate::docgen::markdown::Markdown
//!
//! # Example
//!
//! ```rust
//! use std::time::Duration;
//!
//! use configory::docgen::Docgen;
//! use configory::docgen::markdown::Markdown;
//!
//! /// This shows up at the top of the documentation.
//! #[derive(Docgen, Default)]
//! struct Demo {
//!     /// Fields show up as markdown tables.
//!     field: u8,
//!     /// Nested structs get their own table heading.
//!     nested: Nested,
//! }
//!
//! /// The struct doc for child structs is not used for markdown formatting.
//! #[derive(Docgen)]
//! struct Nested {
//!     /// Custom types can be worked around with attributes.
//!     #[docgen(doc_type = "integer (milliseconds)", default = "300")]
//!     custom: Duration,
//! }
//!
//! impl Default for Nested {
//!     fn default() -> Self {
//!         Self { custom: Duration::from_millis(300) }
//!     }
//! }
//!
//! // Convert our struct to a markdown `String`.
//! let mut formatter = Markdown::new();
//! formatter.set_heading_size(3);
//! let markdown = formatter.format::<Demo>();
//!
//! let expected = "\
//! This shows up at the top of the documentation.
//!
//! |Name|Description|Type|Default|
//! |-|-|-|-|
//! |field|Fields show up as markdown tables|integer|`0`|
//!
//! #### nested
//!
//! Nested structs get their own table heading.
//!
//! |Name|Description|Type|Default|
//! |-|-|-|-|
//! |custom|Custom types can be worked around with attributes|integer (milliseconds)|`300`|
//! ";
//!
//! assert_eq!(markdown, expected);
//! ```

use std::borrow::Cow;
use std::fmt::Debug;
use std::path::{Path, PathBuf};

/// This can be used to automatically derive [`Docgen`] for a type.
///
/// # Structs
///
/// Deriving [`Docgen`] for structs allows formatting it using configuration
/// formatters like [`Markdown`].
///
/// The derive macro makes use of [`Default`] and [`Debug`] impls to document
/// the default value for an option. See [`attributes`] for working around types
/// which do not implement these traits.
///
/// [`attributes`]: ./derive.Docgen.html#struct-field-attributes
///
/// ## Example
///
/// ```rust
/// use configory::docgen::Docgen;
///
/// #[derive(Docgen, Default, Debug)]
/// struct Demo {
///     /// This is a field.
///     field: u8,
/// }
/// ```
///
/// ## Struct Attributes
///
/// For some types the derive macro does not quite work out of the box, while a
/// custom implementation would be too verbose. The `doc_type` attribute can be
/// used as a shorthand for [`DocType::Leaf`] implementations with [`Debug`]
/// support:
///
/// ```rust
/// use configory::docgen::Docgen;
///
/// #[derive(Docgen, Debug)]
/// #[docgen(doc_type = "text")]
/// struct Demo(String);
///
/// #[derive(Docgen, Debug)]
/// #[docgen(doc_type = "\"A\" | \"B\"")]
/// enum Options {
///     A,
///     B,
/// }
/// ```
///
/// ## Struct Field Attributes
///
/// Various attributes are supported to work around foreign structs that are not
/// natively supported:
///
///  - `doc_type = string` &mdash; This is equivalent to a
///    `DocType::Leaf(Leaf::new(string))`
///  - `default = string` &mdash; The `string` will be used as default value for
///    this field
///  - `skip` &mdash; Skipped fields will not show up in the documentation
///
/// ```rust
/// use configory::docgen::Docgen;
///
/// #[derive(Docgen, Default, Debug)]
/// struct Demo {
///     /// This field is incorrectly documented as a string.
///     #[docgen(doc_type = "text", default = "\"invalid\"")]
///     field: u8,
///     /// This field will never show up.
///     #[docgen(skip)]
///     secret_password: u8,
/// }
/// ```
///
/// [`Markdown`]: crate::docgen::markdown::Markdown
pub use configory_derive::Docgen;

pub mod markdown;

/// Documentation representation for a type.
///
/// # Example
///
/// The trait can either be implemented manually:
///
/// ```rust
/// use configory::docgen::{DocType, Docgen, Leaf};
///
/// struct CustomString(String);
///
/// impl Docgen for CustomString {
///     fn doc_type() -> DocType {
///         DocType::Leaf(Leaf::new("text"))
///     }
///
///     fn format(&self) -> String {
///         format!("{:?}", self.0)
///     }
/// }
/// ```
///
/// Or it can be derived:
///
/// ```rust
/// use configory::docgen::{DocType, Docgen, Leaf};
///
/// #[derive(Docgen, Debug)]
/// #[docgen(doc_type = "text")]
/// struct MyString;
/// ```
pub trait Docgen {
    /// Documentation representation of this type.
    fn doc_type() -> DocType;

    /// Format a value to the expected config value.
    ///
    /// This is used to document the default value.
    fn format(&self) -> String;
}

/// Kind of documentation type.
///
/// This represents nodes in the syntax tree structure. [`Self::Table`] is a
/// node with children (like a struct), while [`Self::Leaf`] has none (like a
/// primitive field).
#[derive(PartialEq, Eq, Debug)]
pub enum DocType {
    /// Table with multiple fields, each their own [`DocType`].
    Table(Table),
    /// Terminating type with direct representation, like [`bool`].
    Leaf(Leaf),
}

/// Container field type.
///
/// A table contains multiple fields, each their own [`DocType`]. Its
/// documentation is represented as a collection of its children.
#[derive(PartialEq, Eq, Debug)]
pub struct Table {
    /// Table's documentation.
    pub doc: Cow<'static, str>,
    /// Fields in the table.
    pub fields: Vec<Field>,
}

impl Table {
    /// Create a new table field type.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::docgen::Table;
    ///
    /// let _table = Table::new("This table has documentation attached.");
    /// ```
    pub fn new<S>(doc: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self { doc: doc.into(), fields: Default::default() }
    }

    /// Add a field to the table.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::docgen::{DocType, Field, Leaf, Table};
    ///
    /// let mut table = Table::new("");
    /// table.push(Field {
    ///     doc_type: DocType::Leaf(Leaf::new("integer")),
    ///     ident: "integer_field".into(),
    ///     doc: "This is an integer.".into(),
    ///     default: "300".into(),
    /// });
    /// ```
    pub fn push(&mut self, field: Field) {
        self.fields.push(field);
    }
}

/// Terminating field type, like [`bool`].
///
/// A leaf's type description has a direct representation which can be presented
/// to the user without having to reference another type.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Leaf {
    type_name: Cow<'static, str>,
}

impl Leaf {
    /// Create a new leaf field type.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::docgen::Leaf;
    ///
    /// let _leaf = Leaf::new("integer");
    /// ```
    pub fn new<S>(type_name: S) -> Self
    where
        S: Into<Cow<'static, str>>,
    {
        Self { type_name: type_name.into() }
    }
}

/// Field on a [`Table`].
#[derive(PartialEq, Eq, Debug)]
pub struct Field {
    /// Default value.
    pub default: Cow<'static, str>,
    /// Field name.
    pub ident: Cow<'static, str>,
    /// Field's documentation.
    pub doc: Cow<'static, str>,
    /// Field type.
    pub doc_type: DocType,
}

// Assign `DocType::Leaf` to common types.
macro_rules! impl_simple_type {
    ($($ty:ty),* , $doc:literal $(,)*) => {
        $(
            impl Docgen for $ty {
                fn doc_type() -> DocType {
                    DocType::Leaf(Leaf::new($doc))
                }

                fn format(&self) -> String {
                    format!("{:?}", self)
                }
            }
        )*
    };
}
impl_simple_type!(usize, u8, u16, u32, u64, u128, "integer");
impl_simple_type!(isize, i8, i16, i32, i64, i128, "integer");
impl_simple_type!(PathBuf, Path, "path");
impl_simple_type!(String, &str, "text");
impl_simple_type!(f32, f64, "float");
impl_simple_type!(char, "character");
impl_simple_type!(bool, "boolean");

impl<T: Docgen + Debug> Docgen for Option<T> {
    fn doc_type() -> DocType {
        T::doc_type()
    }

    fn format(&self) -> String {
        match self {
            Some(value) => format!("{:?}", value),
            None => "null".into(),
        }
    }
}
