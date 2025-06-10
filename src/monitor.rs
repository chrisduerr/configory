//! File change monitor.

use std::path::PathBuf;
use std::sync::Arc;
use std::sync::mpsc::{self, RecvTimeoutError};
use std::time::{Duration, Instant};

use notify::{EventKind, RecommendedWatcher, RecursiveMode, Watcher as _};

use crate::{Config, Error, EventHandler, thread};

/// Manual polling interval for platforms without file event support.
const FALLBACK_POLLING_TIMEOUT: Duration = Duration::from_secs(1);

/// Config debounce interval, to avoid triggers without significant change.
const DEBOUNCE_DELAY: Duration = Duration::from_millis(100);

pub(crate) struct Watcher {
    _watcher: RecommendedWatcher,
}

impl Watcher {
    /// Watch a path for updates.
    ///
    /// This will automatically spawn two background threads to monitor for
    /// changes.
    pub(crate) fn new<E, D>(
        config: Config,
        event_handler: Arc<E>,
        path: PathBuf,
    ) -> Result<Option<Self>, Error>
    where
        E: EventHandler<D>,
    {
        // Ensure path is not a special file.
        if path.metadata().is_ok_and(|metadata| !metadata.file_type().is_file()) {
            #[cfg(feature = "log")]
            log::warn!("Cannot monitor special files: {path:?}");

            return Ok(None);
        }

        #[cfg(feature = "log")]
        log::info!("Starting config monitor for {path:?}");

        // Get all paths than require monitoring.
        let mut paths = Vec::with_capacity(2);
        if let Ok(canonical) = path.canonicalize() {
            // Watch original path if path is a symlink.
            if path.symlink_metadata().is_ok_and(|meta| meta.file_type().is_symlink()) {
                paths.push(path.clone());
            }

            // Always watch canonicalized path.
            paths.push(canonical);
        }

        // Create notify file monitor.
        let (tx, rx) = mpsc::channel();
        let notify_config = notify::Config::default().with_poll_interval(FALLBACK_POLLING_TIMEOUT);
        let mut watcher = RecommendedWatcher::new(tx, notify_config)?;

        // Get monitored file's parent directories.
        let parents = paths.iter().map(|path| {
            let mut path = path.clone();
            path.pop();
            path
        });
        let mut parents = parents.collect::<Vec<_>>();
        parents.sort_unstable();
        parents.dedup();

        // Watch all configuration file's parent directories.
        for parent in &parents {
            watcher.watch(parent, RecursiveMode::NonRecursive)?;
        }

        // Watch config files in a background thread.
        thread::spawn_named("config monitor", move || {
            let mut debounce_deadline: Option<Instant> = None;
            loop {
                // Get next notify event, or none if the debounce timeout was reached.
                let event = match debounce_deadline.as_ref() {
                    Some(debounce_deadline) => {
                        let timeout = debounce_deadline.saturating_duration_since(Instant::now());
                        rx.recv_timeout(timeout)
                    },
                    None => rx.recv().map_err(Into::into),
                };

                match event {
                    Ok(Ok(event)) => match event.kind {
                        EventKind::Any
                        | EventKind::Create(_)
                        | EventKind::Modify(_)
                        | EventKind::Remove(_)
                        | EventKind::Other
                            if debounce_deadline.is_none() =>
                        {
                            debounce_deadline = Some(Instant::now() + DEBOUNCE_DELAY)
                        },
                        _ => (),
                    },
                    // Dispatch config events once debounce timeout is reached.
                    Err(RecvTimeoutError::Timeout) => {
                        // Reload config and update our value store.
                        match crate::load_config(&path) {
                            Ok(value) => {
                                config.values.write().unwrap().set_file(value);

                                event_handler.file_changed(&config);
                            },
                            Err(err) => event_handler.file_error(&config, err),
                        }

                        debounce_deadline = None;
                    },
                    Ok(Err(err)) => return Err(err),
                    _ => break,
                }
            }

            #[cfg(feature = "log")]
            log::info!("Shutting down config monitor");

            Ok(())
        });

        Ok(Some(Self { _watcher: watcher }))
    }
}
