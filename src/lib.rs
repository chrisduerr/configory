//! Batteries included application config management.
//!
//! Configory is a batteries included configuration management library which
//! handles all the gory details of supporting configuration files while also
//! supporting IPC access and overrides.
//!
//! # Example
//!
//! To get a configuration which is backed by a file that is automatically
//! reloaded on change, you just need to create a config with [`Manager::new`]:
//!
//! ```rust
//! use configory::Manager;
//!
//! let manager = Manager::new("configory", ()).unwrap();
//! manager.set(&["option"], 3);
//!
//! assert_eq!(manager.get::<_, i32>(&["option"]), Ok(Some(3)));
//! ```
//!
//! This will also automatically spawn an IPC server which allows configuration
//! file access and modification through a socket file. If you want to disable
//! this, you can use [`Manager::with_options`].
//!
//! The event handler passed to [`Manager::new`] or [`Manager::with_options`]
//! can be used to handle configuration changes, custom IPC messages, and
//! errors.
//!
//! ```rust
//! use std::sync::Arc;
//! use std::sync::atomic::{AtomicU8, Ordering};
//!
//! use configory::{Config, EventHandler, Manager};
//!
//! /// Event handler with a configuration change counter.
//! struct MyEventHandler {
//!     changes: Arc<AtomicU8>,
//! }
//!
//! impl EventHandler<()> for MyEventHandler {
//!     fn ipc_changed(&self, _config: &Config) {
//!         self.changes.fetch_add(1, Ordering::Relaxed);
//!     }
//! }
//!
//! // Register our event handler, which increments a counter on change.
//! let changes = Arc::new(AtomicU8::new(0));
//! let event_handler = MyEventHandler { changes: changes.clone() };
//! let manager = Manager::new("configory", event_handler).unwrap();
//!
//! // Update the config through direct access and IPC.
//! manager.set(&["integer"], 3);
//! manager.ipc().unwrap().set(&["text"], "demo");
//!
//! // Verify the config is correct and was changed once through IPC.
//! assert_eq!(manager.get::<_, String>(&["text"]), Ok(Some("demo".into())));
//! assert_eq!(manager.get::<_, i32>(&["integer"]), Ok(Some(3)));
//! assert_eq!(changes.load(Ordering::Relaxed), 1);
//! ```
//!
//! # IPC client
//!
//! The client side of the IPC interface is constructed from the socket path,
//! which can be acquired using [`Manager::ipc`] and [`Ipc::socket_path`].
//!
//! ```rust
//! use configory::Manager;
//! use configory::ipc::Ipc;
//!
//! // This would typically happen in a separate process.
//! let manager = Manager::new("configory", ()).unwrap();
//! let socket_path = manager.ipc().unwrap().socket_path();
//!
//! // Set and retrieve a configuration value through the socket.
//! let ipc = Ipc::client(socket_path);
//! ipc.set(&["option"], 3).unwrap();
//! let value = ipc.get::<_, i32>(&["option"]).unwrap();
//! assert_eq!(value, Some(3));
//! ```
//!
//! # Struct Deserialization
//!
//! If you prefer accessing configuration values through a struct rather than
//! using the dynamic syntax of [`Config::get`], you can deserialize the
//! toplevel value into your struct:
//!
//! ```rust
//! use configory::Manager;
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Default, PartialEq, Debug)]
//! #[serde(default)]
//! struct MyConfig {
//!     field: String,
//! }
//!
//! let manager = Manager::new("configory", ()).unwrap();
//!
//! // Without configuration file, the default will be empty.
//! let my_config = manager.get::<&str, MyConfig>(&[]);
//! assert_eq!(my_config, Ok(None));
//!
//! // Once changed wit the path syntax, the field will be uptaded.
//! manager.set(&["field"], "demo");
//! let my_config = manager.get::<&str, MyConfig>(&[]).unwrap().unwrap();
//! assert_eq!(my_config.field, String::from("demo"));
//! ```

#![deny(missing_docs)]

use std::fs;
use std::io::{self, ErrorKind as IoErrorKind};
use std::ops::Deref;
use std::path::Path;
use std::sync::{Arc, RwLock};

use serde::de::{Deserialize, DeserializeOwned};
use toml::{Table, Value};

use crate::ipc::{Ipc, Message};
use crate::monitor::Watcher;

pub mod ipc;
mod monitor;
mod thread;

/// Configuration manager.
///
/// # Example
///
/// ```rust
/// use configory::Manager;
///
/// let manager = Manager::new("configory", ()).unwrap();
/// manager.set(&["option"], 3);
///
/// assert_eq!(manager.get::<_, i32>(&["option"]), Ok(Some(3)));
/// ```
pub struct Manager {
    config: Config,
    ipc: Option<Ipc>,

    _watcher: Option<Watcher>,
}

impl Manager {
    /// Initialize the configuration manager.
    ///
    /// This will spawn multiple background threads and create socket files.
    /// See [`Self::with_options`] to control the enabled features.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Manager;
    ///
    /// let manager = Manager::new("configory", ()).unwrap();
    /// #
    /// # manager.set(&["option"], 3);
    /// # assert_eq!(manager.get::<_, i32>(&["option"]), Ok(Some(3)));
    /// ```
    pub fn new<S: AsRef<str>, E, D>(namespace: S, event_handler: E) -> Result<Self, Error>
    where
        E: EventHandler<D>,
        D: DeserializeOwned,
    {
        let options = Options::new(namespace.as_ref()).notify(true).ipc(true);
        Self::with_options(&options, event_handler)
    }

    /// Initialize the configuration manager with some features disabled.
    ///
    /// See [`Self::new`] if you want all the features available.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::{Manager, Options};
    ///
    /// // Create a config without IPC.
    /// let options = Options::new("configory").notify(true);
    /// let manager = Manager::with_options(&options, ()).unwrap();
    /// #
    /// # manager.set(&["option"], 3);
    /// # assert_eq!(manager.get::<_, i32>(&["option"]), Ok(Some(3)));
    /// ```
    pub fn with_options<E, D>(options: &Options, event_handler: E) -> Result<Self, Error>
    where
        E: EventHandler<D>,
        D: DeserializeOwned,
    {
        // Parse initial configuration file.
        let path = dirs::config_dir()
            .map(|dir| dir.join(options.namespace).join(format!("{}.toml", options.namespace)));
        let config = match path.as_ref().map(|path| load_config(path)) {
            Some(Ok(config_file)) => Config::from_config(config_file),
            Some(Err(err)) => {
                let config = Config::new();
                event_handler.file_error(&config, err);
                config
            },
            None => Config::new(),
        };

        let event_handler = Arc::new(event_handler);

        // Spawn IPC thread.
        let ipc = if options.ipc {
            Some(Ipc::listen(config.clone(), options.namespace, event_handler.clone())?)
        } else {
            None
        };

        // Spawn file monitor.
        let _watcher = match &path {
            Some(path) if options.notify => {
                Watcher::new(config.clone(), event_handler, path.into())?
            },
            _ => None,
        };

        Ok(Self { _watcher, config, ipc })
    }

    /// Get an IPC handle.
    ///
    /// Will be [`None`] only if IPC was disabled using [`Self::with_options`].
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::{Manager, Options};
    ///
    /// let manager = Manager::new("configory", ()).unwrap();
    /// assert!(manager.ipc().is_some());
    ///
    /// let options = Options::new("configory");
    /// let manager = Manager::with_options(&options, ()).unwrap();
    /// assert!(manager.ipc().is_none());
    /// ```
    pub fn ipc(&self) -> Option<&Ipc> {
        self.ipc.as_ref()
    }
}

impl Deref for Manager {
    type Target = Config;

    fn deref(&self) -> &Self::Target {
        &self.config
    }
}

/// Configuration value storage.
#[derive(Clone)]
pub struct Config {
    pub(crate) values: Arc<RwLock<Values>>,
}

impl Config {
    /// Create a new empty store.
    fn new() -> Self {
        Self::from_config(Value::Table(Table::new()))
    }

    /// Create a new store from a parsed configuration file.
    fn from_config(value: Value) -> Self {
        Self { values: Arc::new(RwLock::new(Values::new(value))) }
    }

    /// Get the current value of a config option.
    ///
    /// This will return `Ok(None)` if the requested option does not exist, and
    /// an error if it does not match the requested type. This will never error
    /// if `T` is [`toml::Value`].
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Manager;
    ///
    /// let manager = Manager::new("configory", ()).unwrap();
    /// manager.set(&["option"], 3);
    ///
    /// let existing_value = manager.get::<_, i32>(&["option"]);
    /// let missing_value = manager.get::<_, i32>(&["missing"]);
    ///
    /// assert_eq!(existing_value, Ok(Some(3)));
    /// assert_eq!(missing_value, Ok(None));
    /// ```
    pub fn get<'de, S, T>(&self, path: &[S]) -> Result<Option<T>, toml::de::Error>
    where
        S: AsRef<str>,
        T: Deserialize<'de>,
    {
        let value = self.values.read().unwrap().get(path).cloned();
        match value {
            Some(value) => Ok(Some(value.try_into()?)),
            None => Ok(None),
        }
    }

    /// Set a runtime override for a config option.
    ///
    /// See [`Self::reset`] for clearing runtime overrides.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Manager;
    ///
    /// let manager = Manager::new("configory", ()).unwrap();
    ///
    /// manager.set(&["option"], 3);
    /// #
    /// # assert_eq!(manager.get::<_, i32>(&["option"]), Ok(Some(3)));
    /// ```
    pub fn set<S, V>(&self, path: &[S], value: V)
    where
        S: AsRef<str>,
        V: Into<Value>,
    {
        let value = value.into();
        self.values.write().unwrap().set(path, value);
    }

    /// Clear the runtime override for a config option.
    ///
    /// See [`Self::set`] for setting runtime overrides.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Manager;
    ///
    /// let manager = Manager::new("configory", ()).unwrap();
    /// # manager.set(&["option"], 3);
    ///
    /// assert_eq!(manager.get::<_, i32>(&["option"]), Ok(Some(3)));
    /// manager.reset(&["option"]);
    /// assert_eq!(manager.get::<_, i32>(&["option"]), Ok(None));
    /// ```
    pub fn reset<S>(&self, path: &[S])
    where
        S: AsRef<str>,
    {
        self.values.write().unwrap().reset(path)
    }
}

/// Configuration value store.
#[derive(Debug)]
struct Values {
    /// Deserialized configuration file values.
    file: Value,
    /// Current runtime overrides, like IPC config.
    runtime: Value,
    /// File values merged with runtime overrides.
    merged: Value,
}

impl Values {
    /// Create a new config value store.
    fn new(file: Value) -> Self {
        let merged = file.clone();
        Self { merged, file, runtime: Value::Table(Table::new()) }
    }

    /// Update configuration file values.
    fn set_file(&mut self, file: Value) {
        self.file = file;

        // Apply config overrides to the file.
        self.merged = self.file.clone();
        toml_merge(&mut self.merged, self.runtime.clone());
    }

    /// Get the current value of a config option.
    fn get<S>(&self, path: &[S]) -> Option<&toml::Value>
    where
        S: AsRef<str>,
    {
        toml_get(&self.merged, path)
    }

    /// Override the runtime portion of a configuration value.
    fn set<S, V>(&mut self, path: &[S], value: V)
    where
        S: AsRef<str>,
        V: Into<Value>,
    {
        let value = value.into();
        toml_insert(&mut self.merged, path, value.clone());
        toml_insert(&mut self.runtime, path, value);
    }

    /// Clear the runtime portion of a configuration value.
    fn reset<S>(&mut self, path: &[S])
    where
        S: AsRef<str>,
    {
        // Reset merged value to configuration file if available.
        match toml_get(&self.file, path) {
            Some(value) => toml_insert(&mut self.merged, path, value.clone()),
            None => toml_remove(&mut self.merged, path),
        }
        toml_remove(&mut self.runtime, path);
    }
}

/// Event handler for configuration changes.
///
/// ```rust
/// use std::sync::Arc;
/// use std::sync::atomic::{AtomicU8, Ordering};
///
/// use configory::{Config, EventHandler, Manager};
///
/// /// Event handler with a configuration change counter.
/// struct MyEventHandler {
///     changes: Arc<AtomicU8>,
/// }
///
/// impl EventHandler<()> for MyEventHandler {
///     fn ipc_changed(&self, _config: &Config) {
///         self.changes.fetch_add(1, Ordering::Relaxed);
///     }
/// }
///
/// // Register our event handler, which increments a counter on change.
/// let changes = Arc::new(AtomicU8::new(0));
/// let event_handler = MyEventHandler { changes: changes.clone() };
/// let manager = Manager::new("configory", event_handler).unwrap();
///
/// // Update the config through direct access and IPC.
/// manager.set(&["integer"], 3);
/// manager.ipc().unwrap().set(&["text"], "demo");
///
/// // Verify the config is correct and was changed once through IPC.
/// assert_eq!(manager.get::<_, String>(&["text"]), Ok(Some("demo".into())));
/// assert_eq!(manager.get::<_, i32>(&["integer"]), Ok(Some(3)));
/// assert_eq!(changes.load(Ordering::Relaxed), 1);
/// ```
pub trait EventHandler<D>: Send + Sync + 'static {
    /// Handle configuration file changes.
    fn file_changed(&self, _config: &Config) {}

    /// Handle configuration file syntax errors.
    fn file_error(&self, _config: &Config, _err: Error) {}

    /// Handle configuration changes through IPC.
    fn ipc_changed(&self, _config: &Config) {}

    /// Handle user-defined ipc messages.
    fn ipc_message(&self, _config: &Config, _message: Message<D>) {}
}

/// Dummy handler when update notifications are undesired.
impl EventHandler<()> for () {}

/// Configuration monitor options.
///
/// See [`Manager::with_options`].
///
/// # Example
///
/// ```rust
/// use configory::Options;
///
/// // Disable all features.
/// let options = Options::new("configory");
///
/// // Enable all features.
/// let options = Options::new("configory").notify(true).ipc(true);
/// ```
pub struct Options<'a> {
    namespace: &'a str,
    notify: bool,
    ipc: bool,
}

impl<'a> Options<'a> {
    /// Create a new set of options with all features disabled.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Options;
    ///
    /// let options = Options::new("configory");
    /// ```
    pub fn new(namespace: &'a str) -> Self {
        Self { namespace, notify: false, ipc: false }
    }

    /// Enable or disable the file monitor.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Options;
    ///
    /// let options = Options::new("configory").notify(true);
    /// ```
    pub fn notify(mut self, enabled: bool) -> Self {
        self.notify = enabled;
        self
    }

    /// Enable or disable IPC.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Options;
    ///
    /// // Enable all features.
    /// let options = Options::new("configory").ipc(true);
    /// ```
    pub fn ipc(mut self, enabled: bool) -> Self {
        self.ipc = enabled;
        self
    }
}

/// Resolve a toml path key's value recursively.
fn toml_get<'a, S>(value: &'a Value, path: &[S]) -> Option<&'a Value>
where
    S: AsRef<str>,
{
    match (value, path.first()) {
        (Value::Table(table), Some(segment)) => {
            let next_value = table.get(segment.as_ref())?;
            toml_get(next_value, &path[1..])
        },
        (Value::Table(table), None) if table.is_empty() => None,
        (value, _) => Some(value),
    }
}

/// Insert a toml value, creating new tables if necessary.
fn toml_insert<S>(value: &mut Value, path: &[S], inserting: Value)
where
    S: AsRef<str>,
{
    match (value, path.first()) {
        (Value::Table(table), Some(segment)) => {
            let next_value =
                table.entry(segment.as_ref()).or_insert_with(|| Value::Table(Table::new()));
            toml_insert(next_value, &path[1..], inserting)
        },
        (value, Some(segment)) => {
            *value = Value::Table(Table::new());
            let table = value.as_table_mut().unwrap();
            let next_value =
                table.entry(segment.as_ref()).or_insert_with(|| Value::Table(Table::new()));
            toml_insert(next_value, path, inserting)
        },
        (value, None) => *value = inserting,
    }
}

/// Remove a toml value.
fn toml_remove<S>(value: &mut Value, path: &[S])
where
    S: AsRef<str>,
{
    // If the root is removed, just replace it with a new table.
    if path.is_empty() {
        *value = Value::Table(Table::new());
        return;
    }

    // Values can only be removed from tables, so ignore everything else.
    let table = match value {
        Value::Table(table) => table,
        _ => return,
    };

    // Remove value if it's in the current table.
    if path.len() == 1 {
        table.remove(path[0].as_ref());
    }

    // Recurse into the table, ignoring invalid paths.
    if let Some(next_value) = table.get_mut(path[0].as_ref()) {
        toml_remove(next_value, &path[1..]);
    }
}

/// Merge two toml values together.
fn toml_merge(base: &mut Value, new: Value) {
    match (base, new) {
        (Value::Table(base_table), Value::Table(new_table)) => {
            for (key, new_value) in new_table.into_iter() {
                match base_table.get_mut(&key) {
                    Some(base_value) => toml_merge(base_value, new_value),
                    None => _ = base_table.insert(key, new_value),
                }
            }
        },
        (Value::String(base_string), Value::String(new_string)) => *base_string = new_string,
        (Value::Integer(base_int), Value::Integer(new_int)) => *base_int = new_int,
        (Value::Float(base_float), Value::Float(new_float)) => *base_float = new_float,
        (Value::Boolean(base_bool), Value::Boolean(new_bool)) => *base_bool = new_bool,
        (Value::Datetime(base_date), Value::Datetime(new_date)) => *base_date = new_date,
        (Value::Array(base_array), Value::Array(new_array)) => base_array.extend(new_array),
        // On type mismatch, we just use the override.
        (base, new) => *base = new,
    }
}

/// Configuration errors.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// Configuration file deserialization failed.
    #[error("{0}")]
    ConfigDeserialize(#[from] toml::de::Error),
    /// IPC message deserialization failed.
    #[error("{0}")]
    IpcDeserialize(#[from] serde_json::Error),
    /// File monitor creation failed.
    #[error("{0}")]
    Notify(#[from] notify::Error),
    /// IO error.
    #[error("{0}")]
    Io(#[from] io::Error),
}

/// Deserialize a configuration file.
pub(crate) fn load_config(path: &Path) -> Result<Value, Error> {
    // Get file content and strip UTF-8 BOM.
    let mut config_text = match fs::read_to_string(path) {
        Ok(config_text) => config_text,
        Err(err) if err.kind() == IoErrorKind::NotFound => {
            return Ok(Value::Table(Table::new()));
        },
        Err(err) => return Err(err.into()),
    };
    if config_text.starts_with('\u{FEFF}') {
        config_text = config_text.split_off(3);
    }

    // Deserialize configuration file.
    let config: Value = toml::from_str(&config_text)?;

    Ok(config)
}

#[cfg(test)]
mod tests {
    use std::env;

    use serde::Deserialize;
    use tempfile::tempdir;

    use super::*;

    #[test]
    fn toml_get_simple() {
        // Create test tree.
        let mut table = Table::new();
        table.insert("exists".into(), Value::Integer(3));
        let value = Value::Table(table);

        // Run tests against both implementations.
        assert_eq!(toml_get(&value, &["exists"]), Some(&Value::Integer(3)));
        assert_eq!(toml_get(&value, &["missing"]), None);
    }

    #[test]
    fn toml_get_table() {
        // Create test tree.
        let mut nested_table = Table::new();
        nested_table.insert("nested".into(), Value::Integer(3));
        let mut table = Table::new();
        table.insert("exists".into(), Value::Table(Table::new()));
        table.insert("exists2".into(), Value::Table(nested_table));
        let value = Value::Table(table);

        // Run tests against both implementations.
        assert_eq!(toml_get(&value, &["exists"]), None);
        assert!(toml_get(&value, &["exists2"]).is_some());
        assert_eq!(toml_get(&value, &["exists2", "nested"]), Some(&Value::Integer(3)));
    }

    #[test]
    fn toml_insert_simple() {
        // Create test tree.
        let mut value = Value::Table(Table::new());

        // Insert the new value.
        toml_insert(&mut value, &["exists"], Value::Integer(3));

        // Verify new tree structure.
        assert_eq!(toml_get(&value, &["exists"]), Some(&Value::Integer(3)));
    }

    #[test]
    fn toml_insert_replace() {
        // Create test tree.
        let mut table = Table::new();
        table.insert("exists".into(), Value::Integer(0));
        let mut value = Value::Table(table);
        assert_eq!(toml_get(&value, &["exists"]), Some(&Value::Integer(0)));

        // Insert the new value.
        toml_insert(&mut value, &["exists"], Value::Integer(3));

        // Verify new tree structure.
        assert_eq!(toml_get(&value, &["exists"]), Some(&Value::Integer(3)));
    }

    #[test]
    fn toml_insert_nested_replace() {
        // Create test tree.
        let mut nested_table = Table::new();
        nested_table.insert("nested".into(), Value::Integer(0));
        let mut table = Table::new();
        table.insert("exists".into(), Value::Table(nested_table));
        let mut value = Value::Table(table);
        assert_eq!(toml_get(&value, &["exists", "nested"]), Some(&Value::Integer(0)));

        // Insert the new value.
        toml_insert(&mut value, &["exists", "nested"], Value::Integer(3));

        // Verify new tree structure.
        assert_eq!(toml_get(&value, &["exists", "nested"]), Some(&Value::Integer(3)));
    }

    #[test]
    fn toml_insert_replace_table() {
        // Create test tree.
        let mut nested_table = Table::new();
        nested_table.insert("nested".into(), Value::Integer(0));
        nested_table.insert("nested2".into(), Value::Integer(1));
        let mut table = Table::new();
        table.insert("exists".into(), Value::Table(nested_table));
        let mut value = Value::Table(table);
        assert_eq!(toml_get(&value, &["exists", "nested"]), Some(&Value::Integer(0)));
        assert_eq!(toml_get(&value, &["exists", "nested2"]), Some(&Value::Integer(1)));

        // Insert the new value.
        let mut new_nested_table = Table::new();
        new_nested_table.insert("nested".into(), Value::Integer(3));
        toml_insert(&mut value, &["exists"], Value::Table(new_nested_table));

        // Verify new tree structure.
        assert_eq!(toml_get(&value, &["exists", "nested"]), Some(&Value::Integer(3)));
        assert_eq!(toml_get(&value, &["exists", "nested2"]), None);
    }

    #[test]
    fn toml_insert_deep_new() {
        // Create test tree.
        let mut value = Value::Table(Table::new());

        // Insert the new value.
        toml_insert(&mut value, &["exists", "nested", "deep"], Value::Integer(3));

        // Verify new tree structure.
        assert_eq!(toml_get(&value, &["exists", "nested", "deep"]), Some(&Value::Integer(3)));
    }

    #[test]
    fn toml_remove_all() {
        let mut table = Table::new();
        table.insert("aoeu".into(), Value::Integer(3));
        let mut value = Value::Table(table);

        toml_remove::<&str>(&mut value, &[]);

        assert_eq!(value, Value::Table(Table::new()));
    }

    #[test]
    fn toml_remove_simple() {
        let mut table = Table::new();
        table.insert("aoeu".into(), Value::Integer(3));
        table.insert("bbb".into(), Value::Integer(9));
        let mut value = Value::Table(table);

        toml_remove(&mut value, &["bbb"]);

        let mut expected_table = Table::new();
        expected_table.insert("aoeu".into(), Value::Integer(3));
        let expected = Value::Table(expected_table);

        assert_eq!(value, expected);
    }

    #[test]
    fn toml_merge_tables() {
        let mut table_a = Table::new();
        table_a.insert("yyy".into(), Value::Integer(1));
        table_a.insert("aoeu".into(), Value::Integer(3));
        let mut a = Value::Table(table_a);

        let mut table_b = Table::new();
        table_b.insert("aoeu".into(), Value::Integer(0));
        table_b.insert("xxx".into(), Value::Integer(9));
        let b = Value::Table(table_b);

        toml_merge(&mut a, b);

        let mut expected_table = Table::new();
        expected_table.insert("yyy".into(), Value::Integer(1));
        expected_table.insert("aoeu".into(), Value::Integer(0));
        expected_table.insert("xxx".into(), Value::Integer(9));
        let expected = Value::Table(expected_table);

        assert_eq!(a, expected);
    }

    #[test]
    fn toml_merge_array() {
        let mut table_a = Table::new();
        table_a.insert("a".into(), Value::Array(vec![Value::Integer(3)]));
        let mut a = Value::Table(table_a);

        let mut table_b = Table::new();
        table_b.insert("a".into(), Value::Array(vec![Value::Integer(9)]));
        let b = Value::Table(table_b);

        toml_merge(&mut a, b);

        let mut expected_table = Table::new();
        expected_table.insert("a".into(), Value::Array(vec![Value::Integer(3), Value::Integer(9)]));
        let expected = Value::Table(expected_table);

        assert_eq!(a, expected);
    }

    #[test]
    fn toml_merge_mismatched_types() {
        let mut table_a = Table::new();
        table_a.insert("a".into(), Value::Integer(0));
        let mut a = Value::Table(table_a);

        let mut table_b = Table::new();
        table_b.insert("a".into(), Value::String("test".into()));
        let b = Value::Table(table_b);

        toml_merge(&mut a, b);

        let mut expected_table = Table::new();
        expected_table.insert("a".into(), Value::String("test".into()));
        let expected = Value::Table(expected_table);

        assert_eq!(a, expected);
    }

    #[test]
    fn config_get_merged() {
        let test_id = "configory_config_get_merged";

        #[derive(Deserialize, PartialEq, Debug)]
        struct Test {
            integer: i32,
            text: String,
        }

        // Create a temporary config with an initial value present.
        let tempdir = tempdir().unwrap();
        let fake_home = tempdir.path().join(test_id);
        unsafe { env::set_var("XDG_CONFIG_HOME", &*fake_home.to_string_lossy()) };
        let config_path = fake_home.join(test_id).join(format!("{test_id}.toml"));
        fs::create_dir_all(config_path.parent().unwrap()).unwrap();
        fs::write(&config_path, "integer = 13").unwrap();

        // Load config and add a runtime option.
        let manager = Manager::new(test_id, ()).unwrap();
        manager.set(&["text"], "test");

        // Ensure runtime and file values are merged in the root table.
        let root = manager.get::<&str, Test>(&[]);
        assert_eq!(root, Ok(Some(Test { integer: 13, text: "test".into() })));
    }
}
