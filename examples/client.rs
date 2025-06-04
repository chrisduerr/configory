//! Example IPC client.
//!
//! This represents the client side of a configuration IPC socket, allowing
//! users to modify the configuration from a separate process.

use std::env;
use std::str::FromStr;

use configory::ipc::Ipc;

fn main() {
    // Get the first socket for this namespace.
    let ipcs = Ipc::all("configory-example");
    let ipc = ipcs.first().expect("socket not found, please run the server example first");

    // Allow change and retrieval of `integer`/`text` options through IPC.
    let mut args = env::args();
    let _ = args.next();
    match (args.next().as_deref(), args.next().as_deref(), args.next().as_deref()) {
        (Some("get"), Some("integer"), None) => {
            let value = ipc.get::<_, i32>(&["integer"]).unwrap();
            println!("{value:?}");
        },
        (Some("set"), Some("integer"), Some(value)) => {
            let value = i32::from_str(value).unwrap();
            ipc.set(&["integer"], Some(value)).unwrap();
        },
        (Some("get"), Some("text"), None) => {
            let value = ipc.get::<_, String>(&["text"]).unwrap();
            println!("{value:?}");
        },
        (Some("set"), Some("text"), Some(value)) => {
            ipc.set(&["text"], Some(value)).unwrap();
        },
        _ => println!("USAGE: client <get|set> <integer|text> [VALUE]"),
    }
}
