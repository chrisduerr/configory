//! Example configuration user.
//!
//! This represents an application using `configory`, which in this case just
//! logs the config whenever it is changed.

use std::{env, fs};

use configory::{Config, EventHandler, Manager};
use serde::Deserialize;
use signal_hook::consts::signal::{SIGINT, SIGQUIT, SIGTERM};
use signal_hook::iterator::Signals;
use tempfile::tempdir;

#[derive(Deserialize, Default, Debug)]
#[serde(default)]
struct MyConfig {
    integer: i32,
    text: String,
}

fn main() {
    // We set a custom config home to demonstrate initialization from a
    // configuration file. Traditionally this would be in
    // `~/.config/configory-example/configory-example.toml` or equivalent on
    // non-Linux platforms.
    let tempdir = tempdir().unwrap();
    let fake_home = tempdir.path().join("configory-example-home");
    unsafe { env::set_var("XDG_CONFIG_HOME", &*fake_home.to_string_lossy()) };

    // Write our example config.
    let config_path = fake_home.join("configory-example").join("configory-example.toml");
    fs::create_dir_all(config_path.parent().unwrap()).unwrap();
    fs::write(&config_path, "integer = 13").unwrap();

    // Create a new config.
    //
    // This will automatically source the temporary config we just created.
    let manager = Manager::new("configory-example", ConfigEventHandler).unwrap();

    // Print initial configuration state.
    print_config(&manager);

    // Wait for shutdown.
    let mut signals = Signals::new([SIGINT, SIGTERM, SIGQUIT]).unwrap();
    signals.wait();
}

/// Asynchronous configuraton file change handler.
struct ConfigEventHandler;
impl EventHandler for ConfigEventHandler {
    type MessageData = ();

    fn file_changed(&self, config: &Config) {
        print_config(config);
    }

    fn ipc_changed(&self, config: &Config) {
        print_config(config);
    }
}

/// Print the current configuration values.
fn print_config(config: &Config) {
    let my_config = config.get::<&str, MyConfig>(&[]).unwrap().unwrap_or_default();
    println!("{my_config:#?}");
}
