# Configory

Configory is a batteries included configuration management library which handles
all the gory details of supporting configuration files while also supporting IPC
access and overrides.

## Example

To get a configuration which is backed by a file that is automatically
reloaded on change, you just need to create a config with `Config::new`:

```rust
use configory::Config;

let config = Config::<()>::new("configory").unwrap();
config.set(&["option"], 3);

assert_eq!(config.get::<_, i32>(&["option"]), Some(3));
```

This will also automatically spawn an IPC server which allows configuration
file access and modification through a socket file. If you want to disable
this, you can use `Config::with_options`.

You can subscribe to the `Config::update_rx` channel to receive
notifications about configuration changes and errors:

```rust,no_run
use configory::{Config, Event};

let config = Config::<()>::new("configory").unwrap();

while let Ok(event) = config.update_rx().recv() {
    match event {
        // To update your application when a config value changes, you need to handle
        // both `Event::FileChanged` and `Event::IpcChanged`.
        Event::FileChanged | Event::IpcChanged => (),
        // File errors will be dispatched when the user's configuration file is invalid.
        Event::FileError(_err) => (),
        // This will only be called if you send custom IPC messages.
        Event::Ipc(_msg) => (),
        // This will only be called if you send your own events through the config channel.
        Event::User(_data) => (),
    }
}
```

## IPC client

The client side of the IPC interface is constructed from the socket path,
which can be acquired using `Config::ipc` and `Ipc::socket_path`.

```rust
use configory::Config;
use configory::ipc::Ipc;

// This would typically happen in a separate process.
let config = Config::<()>::new("configory").unwrap();
let socket_path = config.ipc().unwrap().socket_path();

// Set and retrieve a configuration value through the socket.
let ipc = Ipc::client(socket_path);
ipc.set(&["option"], 3).unwrap();
let value = ipc.get::<_, i32>(&["option"]).unwrap();
assert_eq!(value, Some(3));
```

## Struct Deserialization

If you prefer accessing configuration values through a struct rather than
using the dynamic syntax of `Config::get`, you can deserialize the
toplevel value into your struct:

```rust
use configory::Config;
use serde::Deserialize;

#[derive(Deserialize, Default, PartialEq, Debug)]
#[serde(default)]
struct MyConfig {
    field: String,
}

let config = Config::<()>::new("configory").unwrap();

// Without configuration file, the default will be empty.
let my_config = config.get::<&str, MyConfig>(&[]);
assert_eq!(my_config, None);

// Once changed wit the path syntax, the field will be uptaded.
config.set(&["field"], "demo");
let my_config = config.get::<&str, MyConfig>(&[]).unwrap();
assert_eq!(my_config.field, String::from("demo"));
```
