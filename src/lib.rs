//! Batteries included application config management.
//!
//! This crate aims to provide a simple configuration interface while making the
//! implementation of live config file reload and IPC updates trivial.
//!
//! # Example
//!
//! To get a configuration which is backed by a file that is automatically
//! reloaded on change, you just need to create a config with [`Config::new`]:
//!
//! ```rust
//! use configory::Config;
//!
//! let config = Config::<()>::new("configory").unwrap();
//! config.set(&["option"], 3);
//!
//! assert_eq!(config.get::<_, i32>(&["option"]), Some(3));
//! ```
//!
//! This will also automatically spawn an IPC server which allows configuration
//! file access and modification through a socket file. If you want to disable
//! this, you can use [`Config::with_options`].
//!
//! You can subscribe to the [`Config::update_rx`] channel to receive
//! notifications about configuration changes and errors:
//!
//! ```rust,no_run
//! use configory::{Config, Event};
//!
//! let config = Config::<()>::new("configory").unwrap();
//!
//! while let Ok(event) = config.update_rx().recv() {
//!     match event {
//!         // To update your application when a config value changes, you need to handle
//!         // both `Event::FileChanged` and `Event::IpcChanged`.
//!         Event::FileChanged | Event::IpcChanged => (),
//!         // File errors will be dispatched when the user's configuration file is invalid.
//!         Event::FileError(_err) => (),
//!         // This will only be called if you send custom IPC messages.
//!         Event::Ipc(_msg) => (),
//!         // This will only be called if you send your own events through the config channel.
//!         Event::User(_data) => (),
//!     }
//! }
//! ```
//!
//! ## IPC client
//!
//! The client side of the IPC interface is constructed from the socket path,
//! which can be acquired using [`Config::ipc`] and [`Ipc::socket_path`].
//!
//! ```rust
//! use configory::Config;
//! use configory::ipc::Ipc;
//!
//! // This would typically happen in a separate process.
//! let config = Config::<()>::new("configory").unwrap();
//! let socket_path = config.ipc().unwrap().socket_path();
//!
//! // Set and retrieve a configuration value through the socket.
//! let ipc = Ipc::client(socket_path);
//! ipc.set(&["option"], 3).unwrap();
//! let value = ipc.get::<_, i32>(&["option"]).unwrap();
//! assert_eq!(value, Some(3));
//! ```
//!
//! ## Struct Deserialization
//!
//! If you prefer accessing configuration values through a struct rather than
//! using the dynamic syntax of [`Config::get`], you can deserialize the
//! toplevel value into your struct:
//!
//! ```rust
//! use configory::Config;
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Default, PartialEq, Debug)]
//! #[serde(default)]
//! struct MyConfig {
//!     field: String,
//! }
//!
//! let config = Config::<()>::new("configory").unwrap();
//!
//! // Without configuration file, the default will be empty.
//! let my_config = config.get::<&str, MyConfig>(&[]);
//! assert_eq!(my_config, None);
//!
//! // Once changed wit the path syntax, the field will be uptaded.
//! config.set(&["field"], "demo");
//! let my_config = config.get::<&str, MyConfig>(&[]).unwrap();
//! assert_eq!(my_config.field, String::from("demo"));
//! ```

#![deny(missing_docs)]

use std::fs;
use std::io::{self, ErrorKind as IoErrorKind};
use std::path::Path;
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, RwLock};

use serde::de::{Deserialize, DeserializeOwned};
use toml::{Table, Value};

use crate::ipc::{Ipc, Message};

pub mod ipc;
mod monitor;
mod thread;

/// Configuration file manager.
///
/// # Example
///
/// ```rust
/// use configory::Config;
///
/// let config = Config::<()>::new("configory").unwrap();
/// config.set(&["option"], 3);
///
/// assert_eq!(config.get::<_, i32>(&["option"]), Some(3));
/// ```
pub struct Config<D> {
    update_tx: Sender<Event<D>>,
    update_rx: Receiver<Event<D>>,
    values: Arc<RwLock<Values>>,
    ipc: Option<Ipc>,
}

impl<D> Config<D>
where
    D: DeserializeOwned + Send + 'static,
{
    /// Initialize the configuration file manager.
    ///
    /// This will spawn multiple background threads and create socket files.
    /// See [`Self::with_options`] to select different features.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    /// #
    /// # config.set(&["option"], 3);
    /// # assert_eq!(config.get::<_, i32>(&["option"]), Some(3));
    /// ```
    pub fn new<S: AsRef<str>>(namespace: S) -> Result<Self, Error> {
        let options = Options::new(namespace.as_ref()).notify(true).ipc(true);
        Self::with_options(&options)
    }

    /// Initialize the config monitor with some features disabled.
    ///
    /// See [`Self::new`] if you want all the features available.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::{Config, Options};
    ///
    /// // Create a config without IPC.
    /// let options = Options::new("configory").notify(true);
    /// let config = Config::<()>::with_options(&options).unwrap();
    /// #
    /// # config.set(&["option"], 3);
    /// # assert_eq!(config.get::<_, i32>(&["option"]), Some(3));
    /// ```
    pub fn with_options(options: &Options) -> Result<Self, Error> {
        let (update_tx, update_rx) = mpsc::channel();

        // Parse initial configuration file.
        let path = dirs::config_dir()
            .map(|dir| dir.join(options.namespace).join(format!("{}.toml", options.namespace)));
        let values = match path.as_ref().map(|path| load_config(path)) {
            Some(Ok(config_file)) => Values::from_config(config_file),
            Some(Err(err)) => {
                let _ = update_tx.send(Event::FileError(err));
                Values::default()
            },
            None => Values::default(),
        };
        let values = Arc::new(RwLock::new(values));

        // Spawn IPC thread.
        let ipc = if options.ipc {
            Some(Ipc::listen(values.clone(), update_tx.clone(), options.namespace)?)
        } else {
            None
        };

        // Spawn file monitor.
        if let Some(path) = &path
            && options.notify
        {
            monitor::watch(values.clone(), update_tx.clone(), path.into())?;
        }

        Ok(Self { update_tx, update_rx, values, ipc })
    }

    /// Get the current value of a config option.
    ///
    /// This will return [`None`] if the requested does not exist, or does not
    /// match the requested type.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    /// config.set(&["option"], 3);
    ///
    /// let existing_value = config.get::<_, i32>(&["option"]);
    /// let missing_value = config.get::<_, i32>(&["missing"]);
    ///
    /// assert_eq!(existing_value, Some(3));
    /// assert_eq!(missing_value, None);
    /// ```
    pub fn get<'de, S, T>(&self, path: &[S]) -> Option<T>
    where
        S: AsRef<str>,
        T: Deserialize<'de>,
    {
        self.values.read().unwrap().get(path)
    }

    /// Override a configuration value.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    ///
    /// config.set(&["option"], 3);
    /// #
    /// # assert_eq!(config.get::<_, i32>(&["option"]), Some(3));
    /// ```
    pub fn set<S, V>(&self, path: &[S], value: V)
    where
        S: AsRef<str>,
        V: Into<Value>,
    {
        self.values.write().unwrap().set(path, value)
    }

    /// Clear the runtime portion of a configuration value.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    /// # config.set(&["option"], 3);
    ///
    /// assert_eq!(config.get::<_, i32>(&["option"]), Some(3));
    /// config.reset(&["option"]);
    /// assert_eq!(config.get::<_, i32>(&["option"]), None);
    /// ```
    pub fn reset<S>(&self, path: &[S])
    where
        S: AsRef<str>,
    {
        self.values.write().unwrap().reset(path)
    }

    /// Get the receiving end for the update channel.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    ///
    /// let rx = config.update_rx();
    /// ```
    pub fn update_rx(&self) -> &Receiver<Event<D>> {
        &self.update_rx
    }

    /// Get a copy of the sending end for the update channel.
    ///
    /// Sending configuration update events through this channel will **not**
    /// trigger a configuration file reload.
    ///
    /// The purpose of this function is to reuse the existing channel to extend
    /// it with additional functionality. You'll have to handle processing
    /// these events yourself using [`Self::update_rx`].
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    ///
    /// let tx = config.update_tx();
    /// ```
    pub fn update_tx(&self) -> &Sender<Event<D>> {
        &self.update_tx
    }

    /// Get an IPC handle.
    ///
    /// Will be [`None`] only if IPC was disabled using [`Self::with_options`].
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::{Config, Options};
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    /// assert!(config.ipc().is_some());
    ///
    /// let options = Options::new("configory");
    /// let config = Config::<()>::with_options(&options).unwrap();
    /// assert!(config.ipc().is_none());
    /// ```
    pub fn ipc(&self) -> Option<&Ipc> {
        self.ipc.as_ref()
    }
}

/// Config monitor options.
///
/// See [`Config::with_options`].
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

/// Configuration value store.
struct Values {
    /// Deserialized configuration file values.
    file: Value,
    /// Current runtime overrides, like IPC config.
    runtime: Value,
    /// File values merged with runtime overrides.
    merged: Value,
}

impl Values {
    /// Create a new empty config store.
    fn new() -> Self {
        Self::from_config(Value::Table(Table::new()))
    }

    /// Create a new store from a parsed configuration file.
    fn from_config(file: Value) -> Self {
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
    fn get<'de, S, T>(&self, path: &[S]) -> Option<T>
    where
        S: AsRef<str>,
        T: Deserialize<'de>,
    {
        toml_get(&self.merged, path)?.clone().try_into().ok()
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

impl Default for Values {
    fn default() -> Self {
        Self::new()
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

/// Configuration events.
pub enum Event<D> {
    /// Configuration file change.
    FileChanged,
    /// Configuration file contains errors.
    FileError(Error),
    /// Runtime configuration changed through IPC.
    IpcChanged,
    /// User-defined IPC message received.
    Ipc(Message<D>),
    /// Custom message sent directly through IPC.
    ///
    /// See [`Config::update_tx`].
    User(D),
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
        let config = Config::<()>::new(test_id).unwrap();
        config.set(&["text"], "test");

        // Ensure runtime and file values are merged in the root table.
        let root = config.get::<&str, Test>(&[]).unwrap();
        assert_eq!(root, Test { integer: 13, text: "test".into() });
    }
}
