# Configory

Configory is a batteries included configuration management library which
handles all the gory details of supporting configuration files while also
supporting IPC access and overrides.

# Example

To get a configuration which is backed by a file that is automatically
reloaded on change, you just need to create a config with `Manager::new`:

```rust
use configory::Manager;

let manager = Manager::new("configory", ()).unwrap();
manager.set(&["option"], 3);

assert_eq!(manager.get::<_, i32>(&["option"]), Ok(Some(3)));
```

This will also automatically spawn an IPC server which allows configuration
file access and modification through a socket file. If you want to disable
this, you can use `Manager::with_options`.

The event handler passed to `Manager::new` or `Manager::with_options`
can be used to handle configuration changes, custom IPC messages, and
errors.

```rust
use std::sync::Arc;
use std::sync::atomic::{AtomicU8, Ordering};

use configory::{Config, EventHandler, Manager};

/// Event handler with a configuration change counter.
struct MyEventHandler {
    changes: Arc<AtomicU8>,
}

impl EventHandler<()> for MyEventHandler {
    fn ipc_changed(&self, _config: &Config) {
        self.changes.fetch_add(1, Ordering::Relaxed);
    }
}

// Register our event handler, which increments a counter on change.
let changes = Arc::new(AtomicU8::new(0));
let event_handler = MyEventHandler { changes: changes.clone() };
let manager = Manager::new("configory", event_handler).unwrap();

// Update the config through direct access and IPC.
manager.set(&["integer"], 3);
manager.ipc().unwrap().set(&["text"], "demo");

// Verify the config is correct and was changed once through IPC.
assert_eq!(manager.get::<_, String>(&["text"]), Ok(Some("demo".into())));
assert_eq!(manager.get::<_, i32>(&["integer"]), Ok(Some(3)));
assert_eq!(changes.load(Ordering::Relaxed), 1);
```

# IPC client

The client side of the IPC interface is constructed from the socket path,
which can be acquired using `Manager::ipc` and `Ipc::socket_path`.

```rust
use configory::Manager;
use configory::ipc::Ipc;

// This would typically happen in a separate process.
let manager = Manager::new("configory", ()).unwrap();
let socket_path = manager.ipc().unwrap().socket_path();

// Set and retrieve a configuration value through the socket.
let ipc = Ipc::client(socket_path);
ipc.set(&["option"], 3).unwrap();
let value = ipc.get::<_, i32>(&["option"]).unwrap();
assert_eq!(value, Some(3));
```

# Struct Deserialization

If you prefer accessing configuration values through a struct rather than
using the dynamic syntax of `Config::get`, you can deserialize the
toplevel value into your struct:

```rust
use configory::Manager;
use serde::Deserialize;

#[derive(Deserialize, Default, PartialEq, Debug)]
#[serde(default)]
struct MyConfig {
    field: String,
}

let manager = Manager::new("configory", ()).unwrap();

// Without configuration file, the default will be empty.
let my_config = manager.get::<&str, MyConfig>(&[]);
assert_eq!(my_config, Ok(None));

// Once changed wit the path syntax, the field will be uptaded.
manager.set(&["field"], "demo");
let my_config = manager.get::<&str, MyConfig>(&[]).unwrap().unwrap();
assert_eq!(my_config.field, String::from("demo"));
```
