//! IPC config changes.

use std::io::{self, Read, Write};
use std::net::Shutdown;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::mpsc::Sender;
use std::sync::{Arc, RwLock};
use std::{env, fs, process};

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use toml::Value;

use crate::{Error, Event, Values, thread};

/// Socket file component to help with uniqueness.
const SOCKET_ID: &str = "configory-ipc";

/// Socket IPC.
///
/// The IPC socket is automatically constructed by [`Config::new`] and can be
/// accessed with [`Config::ipc`]. To connect from the other socket end you can
/// connect to [`Ipc::socket_path`] with [`Ipc::client`].
///
/// [`Config::new`]: crate::Config::new
/// [`Config::ipc`]: crate::Config::ipc
///
/// # Example
///
/// ```rust
/// use configory::Config;
/// use configory::ipc::Ipc;
///
/// // Spawn the IPC socket and get its path.
/// let config = Config::<()>::new("configory").unwrap();
/// let socket_path = config.ipc().unwrap().socket_path();
///
/// // Connect to the socket and change `option` to `3`.
/// // This would typically be done from a separate process.
/// let ipc = Ipc::client(socket_path);
/// ipc.set(&["option"], 3).unwrap();
///
/// // Access the new config value directly.
/// let value = config.get::<_, i32>(&["option"]);
/// assert_eq!(value, Ok(Some(3)));
/// ```
pub struct Ipc {
    path: PathBuf,
    server: bool,
}

impl Drop for Ipc {
    fn drop(&mut self) {
        if self.server {
            let _ = fs::remove_file(&self.path);
        }
    }
}

impl Ipc {
    /// Create a client for sending IPC requests.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use configory::Config;
    /// use configory::ipc::Ipc;
    ///
    /// # let config = Config::<()>::new("configory").unwrap();
    /// # config.set(&["option"], 3);
    /// #
    /// # let socket_path = config.ipc().unwrap().socket_path();
    /// let ipc = Ipc::client(socket_path);
    /// #
    /// # assert_eq!(ipc.get::<_, i32>(&["option"]).unwrap(), Some(3));
    /// ```
    pub fn client<P>(path: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self { path: path.into(), server: false }
    }

    /// Get all IPC sockets available for this namespace.
    ///
    /// This can be used in programs where users cannot easily retrieve the
    /// socket path, allowing the IPC client to talk to just retrieve data
    /// from one random socket or broadcast updates to all of them.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    /// use configory::ipc::Ipc;
    ///
    /// // Start multiple IPC servers with two separate namespaces.
    /// let config_a = Config::<()>::new("configory-all").unwrap();
    /// let config_b = Config::<()>::new("configory-all").unwrap();
    /// let other = Config::<()>::new("other").unwrap();
    ///
    /// // Get all available IPC sockets for the `configory-all` namespace.
    /// let ipcs = Ipc::all("configory-all");
    /// assert_eq!(ipcs.len(), 2);
    /// ```
    pub fn all(namespace: &str) -> Vec<Self> {
        let mut ipcs = Vec::new();

        // Socket prefix used by all socket files for this namespace.
        let socket_prefix = format!("{namespace}-{SOCKET_ID}-");

        // Get all socket files in the runtime directory.
        let socket_dir = dirs::runtime_dir().unwrap_or_else(env::temp_dir);
        for entry in fs::read_dir(&socket_dir).into_iter().flatten().flatten() {
            if entry.file_name().to_str().is_some_and(|s| s.starts_with(&socket_prefix)) {
                ipcs.push(Ipc::client(entry.path()));
            }
        }

        ipcs
    }

    /// Create and listen on an IPC socket.
    pub(crate) fn listen<D>(
        values: Arc<RwLock<Values>>,
        update_tx: Sender<Event<D>>,
        namespace: &str,
    ) -> Result<Self, Error>
    where
        D: DeserializeOwned + Send + 'static,
    {
        #[cfg(feature = "log")]
        log::info!("Starting config IPC");

        // Ensure we're not trying to use the same socket name multiple times.
        static PROCESS_SOCKET_COUNT: AtomicU32 = AtomicU32::new(0);
        let socket_index = PROCESS_SOCKET_COUNT.fetch_add(1, Ordering::Relaxed);

        // Create unique target socket path.
        let socket_name = format!("{namespace}-{SOCKET_ID}-{}-{}", process::id(), socket_index);
        let socket_dir = dirs::runtime_dir().unwrap_or_else(env::temp_dir);
        let path = socket_dir.join(format!("{socket_name}.sock"));

        // Try to delete the socket if it exists already.
        let _ = fs::remove_file(&path);

        // Create unix socket listener.
        let listener = UnixListener::bind(&path)?;

        // Listen for socket messages in a separate thread.
        thread::spawn_named("ipc listener", move || {
            let mut buffer = String::new();
            for stream in listener.incoming().filter_map(Result::ok) {
                let _ = Self::handle_message(&values, &update_tx, stream, &mut buffer);
            }
        });

        Ok(Self { path, server: true })
    }

    /// Get the IPC socket path.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    ///
    /// let config = Config::<()>::new("configory").unwrap();
    /// let socket_path = config.ipc().unwrap().socket_path();
    /// ```
    pub fn socket_path(&self) -> &Path {
        &self.path
    }

    /// Handle a new socket message.
    ///
    /// Returns `true` if the socket should be shut down.
    fn handle_message<D>(
        values: &Arc<RwLock<Values>>,
        update_tx: &Sender<Event<D>>,
        mut stream: UnixStream,
        buffer: &mut String,
    ) -> Result<(), Box<dyn std::error::Error>>
    where
        D: DeserializeOwned,
    {
        // Read the message content to our buffer.
        buffer.clear();
        stream.read_to_string(buffer)?;

        // Attempt to deserialize the message.
        let message: IpcMessage<D> = serde_json::from_str(buffer)?;

        // Process IPC event.
        match message {
            // Override runtime config option.
            IpcMessage::SetConfig(path, value) => {
                values.write().unwrap().set(&path, value);
                let _ = update_tx.send(Event::IpcChanged);
            },
            // Clear the runtime portion of a configuration value.
            IpcMessage::ResetConfig(path) => {
                values.write().unwrap().reset(&path);
                let _ = update_tx.send(Event::IpcChanged);
            },
            // Get current config and write it to the socket.
            IpcMessage::GetConfig(path) => {
                let value = values.read().unwrap().get(&path).cloned();
                write_reply(&mut stream, IpcReply::<()>::GetConfig(value))?;
            },
            // Notify user about new custom socket message.
            IpcMessage::User(data) => {
                let message = Message { stream, data };
                let _ = update_tx.send(Event::Ipc(message));
            },
        }

        Ok(())
    }

    /// Get a config option through IPC.
    ///
    /// ```rust
    /// use configory::Config;
    /// use configory::ipc::Ipc;
    ///
    /// # let config = Config::<()>::new("configory").unwrap();
    /// # config.set(&["option"], 3);
    /// #
    /// # let socket_path = config.ipc().unwrap().socket_path();
    /// #
    /// let ipc = Ipc::client(socket_path);
    ///
    /// // Valid option returns `Some(T)`.
    /// let valid = ipc.get::<_, i32>(&["option"]).unwrap();
    /// assert_eq!(valid, Some(3));
    ///
    /// // Incorrect type returns `Err`.
    /// let invalid_type = ipc.get::<_, String>(&["option"]);
    /// assert!(invalid_type.is_err());
    ///
    /// // Missing value returns `None`.
    /// let missing = ipc.get::<_, String>(&["missing"]).unwrap();
    /// assert_eq!(missing, None);
    /// ```
    pub fn get<'de, S, T>(&self, path: &[S]) -> Result<Option<T>, Error>
    where
        S: AsRef<str>,
        T: Deserialize<'de>,
    {
        let path = path.iter().map(|s| s.as_ref().into()).collect::<Vec<_>>();
        let message = IpcMessage::<()>::GetConfig(path);
        match Self::send_message_internal::<_, ()>(&self.path, &message)? {
            Some(IpcReply::GetConfig(Some(value))) => Ok(value.try_into()?),
            _ => Ok(None),
        }
    }

    /// Set a config option through IPC.
    ///
    /// ```rust
    /// use configory::Config;
    /// use configory::ipc::Ipc;
    ///
    /// # let config = Config::<()>::new("configory").unwrap();
    /// #
    /// # let socket_path = config.ipc().unwrap().socket_path();
    /// #
    /// let ipc = Ipc::client(socket_path);
    ///
    /// // Setting an option overrides its runtime value.
    /// ipc.set(&["option"], 3).unwrap();
    /// let value = ipc.get::<_, i32>(&["option"]).unwrap();
    /// assert_eq!(value, Some(3));
    /// ```
    pub fn set<S, V>(&self, path: &[S], value: V) -> Result<(), Error>
    where
        S: AsRef<str>,
        V: Into<Value>,
    {
        let path = path.iter().map(|s| s.as_ref().into()).collect::<Vec<_>>();
        let message = IpcMessage::<()>::SetConfig(path, value.into());
        let _ = Self::send_message_internal::<_, ()>(&self.path, &message)?;
        Ok(())
    }

    /// Clear the runtime portion of a configuration value.
    ///
    /// # Example
    ///
    /// ```rust
    /// use configory::Config;
    /// use configory::ipc::Ipc;
    ///
    /// # let config = Config::<()>::new("configory").unwrap();
    /// #
    /// # let socket_path = config.ipc().unwrap().socket_path();
    /// #
    /// let ipc = Ipc::client(socket_path);
    /// # ipc.set(&["option"], 3).unwrap();
    ///
    /// let value = ipc.get::<_, i32>(&["option"]).unwrap();
    /// assert_eq!(value, Some(3));
    ///
    /// ipc.reset(&["option"]).unwrap();
    /// let value = ipc.get::<_, i32>(&["option"]).unwrap();
    /// assert_eq!(value, None);
    /// ```
    pub fn reset<S>(&self, path: &[S]) -> Result<(), Error>
    where
        S: AsRef<str>,
    {
        let path = path.iter().map(|s| s.as_ref().into()).collect::<Vec<_>>();
        let message = IpcMessage::<()>::ResetConfig(path);
        let _ = Self::send_message_internal::<_, ()>(&self.path, &message)?;
        Ok(())
    }

    /// Send a custom message over the IPC socket.
    ///
    /// This can be used to avoid spinning up your own IPC socket for messages
    /// unrelated to configuration.
    ///
    /// ```rust
    /// use std::thread;
    ///
    /// use configory::ipc::{Ipc, Message};
    /// use configory::{Config, Event};
    ///
    /// # let config = Config::<String>::new("configory").unwrap();
    /// #
    /// # let socket_path = config.ipc().unwrap().socket_path();
    /// #
    /// let ipc = Ipc::client(socket_path);
    ///
    /// // Reply to the next IPC message on a separate thread.
    /// thread::spawn(move || {
    ///     if let Ok(Event::Ipc(mut message)) = config.update_rx().recv() {
    ///         let reply = format!("{} was received", message.data());
    ///         message.reply(&reply).unwrap();
    ///     }
    /// });
    ///
    /// // Send our message and check that the reply matches what we expect.
    /// let reply = ipc.send_message::<_, String>(&String::from("my message")).unwrap();
    /// assert_eq!(reply, Some("my message was received".into()));
    /// ```
    pub fn send_message<D, R>(&self, data: &D) -> Result<Option<R>, Error>
    where
        D: Serialize,
        R: DeserializeOwned,
    {
        match Self::send_message_internal(&self.path, &IpcMessage::User(data))? {
            Some(IpcReply::User(data)) => Ok(Some(data)),
            _ => Ok(None),
        }
    }

    /// Send a message over the IPC socket.
    ///
    /// This differs from [Self::send_message] by allowing the transmission of
    /// internal events.
    fn send_message_internal<D, R>(
        path: &Path,
        message: &IpcMessage<D>,
    ) -> Result<Option<IpcReply<R>>, Error>
    where
        D: Serialize,
        R: DeserializeOwned,
    {
        // Ensure socket exists.
        if !path.exists() {
            let msg = format!("invalid socket path: {}", path.display());
            return Err(io::Error::new(io::ErrorKind::NotFound, msg).into());
        }

        // Write message to the socket.
        let mut stream = UnixStream::connect(path)?;
        let json = serde_json::to_string(message)?;
        stream.write_all(json.as_bytes())?;
        stream.flush()?;

        // Shutdown write end, to allow reading.
        stream.shutdown(Shutdown::Write)?;

        Self::listen_for_reply(&mut stream)
    }

    /// Await and process IPC replies.
    fn listen_for_reply<R>(stream: &mut UnixStream) -> Result<Option<IpcReply<R>>, Error>
    where
        R: DeserializeOwned,
    {
        // Read the reply to our buffer.
        let mut buffer = String::new();
        stream.read_to_string(&mut buffer)?;

        // Handle messages with no reply.
        if buffer.is_empty() {
            return Ok(None);
        }

        // Attempt to deserialize the message.
        let reply: IpcReply<R> = serde_json::from_str(&buffer)?;

        Ok(Some(reply))
    }
}

/// IPC messages.
#[derive(Serialize, Deserialize)]
enum IpcMessage<D> {
    SetConfig(Vec<String>, toml::Value),
    ResetConfig(Vec<String>),
    GetConfig(Vec<String>),
    User(D),
}

/// IPC replies.
#[derive(Serialize, Deserialize)]
enum IpcReply<R> {
    GetConfig(Option<toml::Value>),
    User(R),
}

/// IPC user message.
#[derive(Debug)]
pub struct Message<D> {
    stream: UnixStream,
    data: D,
}

impl<D> Message<D> {
    /// Get the message's content.
    pub fn data(&self) -> &D {
        &self.data
    }

    /// Get the message's content.
    pub fn into_data(self) -> D {
        self.data
    }

    /// Send an IPC reply.
    ///
    /// ```rust
    /// # use std::thread;
    /// #
    /// use configory::ipc::{Ipc, Message};
    /// use configory::{Config, Event};
    ///
    /// # let config = Config::<String>::new("configory").unwrap();
    /// #
    /// # let socket_path = config.ipc().unwrap().socket_path().to_path_buf();
    /// # thread::spawn(move || {
    /// #   let ipc = Ipc::client(socket_path);
    /// #   let reply = ipc.send_message::<_, String>(&String::from("my message")).unwrap();
    /// # });
    /// #
    /// // Reply to the first IPC message.
    /// if let Ok(Event::Ipc(mut message)) = config.update_rx().recv() {
    ///     let reply = format!("{} was received", message.data());
    ///     message.reply(&reply).unwrap();
    /// }
    /// ```
    pub fn reply<R>(&mut self, reply: &R) -> Result<(), ReplyError>
    where
        R: Serialize,
    {
        write_reply(&mut self.stream, IpcReply::User(reply))
    }
}

/// IPC reply transmission error.
#[derive(thiserror::Error, Debug)]
pub enum ReplyError {
    /// Deserialization failure.
    #[error("{0}")]
    Format(#[from] serde_json::Error),
    /// Socket IO error.
    #[error("{0}")]
    Io(#[from] std::io::Error),
}

/// Write an IPC reply to a socket stream.
fn write_reply<R>(stream: &mut UnixStream, reply: IpcReply<R>) -> Result<(), ReplyError>
where
    R: Serialize,
{
    let json = serde_json::to_string(&reply)?;
    stream.write_all(json.as_bytes())?;
    stream.flush()?;
    Ok(())
}
