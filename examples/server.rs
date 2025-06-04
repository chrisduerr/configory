//! Example configuration user.
//!
//! This represents an application using `configory`, which in this case just
//! logs the config whenever it is changed.

use std::{env, fs};

use configory::{Config, Event};

fn main() {
    // We set a custom config home to demonstrate initialization from a
    // configuration file. Traditionally this would be in
    // `~/.config/configory-example/configory-example.toml` or equivalent on
    // non-Linux platforms.
    let fake_home = env::temp_dir().join("configory-example-home");
    unsafe { env::set_var("XDG_CONFIG_HOME", &*fake_home.to_string_lossy()) };

    // Write our example config.
    let config_path = fake_home.join("configory-example").join("configory-example.toml");
    fs::create_dir_all(config_path.parent().unwrap()).unwrap();
    fs::write(&config_path, "integer = 13").unwrap();

    // Create a new config.
    //
    // This will automatically source
    // `~/.config/configory-example/configory-example.toml` or equivalent on
    // other platforms.
    let config = Config::<()>::new("configory-example").unwrap();

    // Setup signal handler for ctrl+c.
    let ipc_sender = config.update_tx().clone();
    ctrlc::set_handler(move || {
        let _ = ipc_sender.send(Event::User(()));
    })
    .unwrap();

    // Print initial configuration state.
    print_config(&config);

    // Print config whenever a change event is received.
    while let Ok(event) = config.update_rx().recv() {
        match event {
            Event::FileChanged | Event::IpcChanged => print_config(&config),
            Event::FileError(_) | Event::Ipc(_) => unreachable!(),
            Event::User(()) => break,
        }
    }
}

/// Print the current configuration values.
fn print_config(config: &Config<()>) {
    println!("CURRENT VALUES:");
    println!("  integer: {:?}", config.get::<_, i32>(&["integer"]));
    println!("  text: {:?}", config.get::<_, String>(&["text"]));
}
