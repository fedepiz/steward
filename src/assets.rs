use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    sync::mpsc::{Receiver, Sender},
    thread,
};

use macroquad::prelude as mq;

#[derive(Clone, Copy)]
pub(crate) enum AssetKind {
    Texture,
    Font,
}

pub(crate) struct Event {
    pub(crate) kind: AssetKind,
    pub(crate) name: String,
    pub(crate) bytes: Vec<u8>,
}

#[derive(Default)]
pub(crate) struct Assets {
    textures: HashMap<String, mq::Texture2D>,
    fonts: HashMap<String, mq::Font>,
    rx: Option<Receiver<Event>>,
}

impl Assets {
    /// Starts a background loader thread and returns a ready Assets handle.
    pub(crate) fn start_loading(asset_root: impl AsRef<Path>) -> Self {
        Self {
            rx: Some(start_asset_loader(asset_root)),
            ..Default::default()
        }
    }

    pub(crate) fn get_texture(&self, name: &str) -> Option<&mq::Texture2D> {
        self.textures.get(name)
    }

    pub(crate) fn get_font(&self, name: &str) -> Option<&mq::Font> {
        self.fonts.get(name)
    }

    /// Applies a single event by decoding bytes on the main thread.
    pub(crate) fn apply_event(&mut self, event: Event) {
        match event.kind {
            AssetKind::Texture => {
                let texture =
                    mq::Texture2D::from_file_with_format(&event.bytes, Some(mq::ImageFormat::Png));
                self.textures.insert(event.name, texture);
            }
            AssetKind::Font => {
                if let Ok(font) = mq::load_ttf_font_from_bytes(&event.bytes) {
                    self.fonts.insert(event.name, font);
                }
            }
        }
    }

    /// Drains pending loader events; stops polling once the channel closes.
    pub(crate) fn tick(&mut self) {
        let _span = tracing::info_span!("Asset tick");
        let rx = match self.rx.take() {
            Some(rx) => rx,
            None => {
                return;
            }
        };

        let mut disconnected = false;
        loop {
            match rx.try_recv() {
                Ok(event) => {
                    self.apply_event(event);
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {
                    break;
                }
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    disconnected = true;
                    break;
                }
            }
        }

        if !disconnected {
            self.rx = Some(rx);
        }
    }
}

pub(crate) fn start_asset_loader(asset_root: impl AsRef<Path>) -> Receiver<Event> {
    let (tx, rx) = std::sync::mpsc::channel();
    let root = asset_root.as_ref().to_path_buf();
    thread::spawn(move || load_assets(root, tx));
    rx
}

fn load_assets(root: PathBuf, tx: Sender<Event>) {
    tracing_tracy::client::set_thread_name!("Asset loader");
    let _span = tracing::info_span!("Asset loading");
    let gfx_dir = root.join("gfx");
    let font_dir = root.join("fonts");

    // Load bytes off-thread, decode on the main thread.
    load_dir(&gfx_dir, AssetKind::Texture, &tx);
    load_dir(&font_dir, AssetKind::Font, &tx);
}

fn load_dir(base: &Path, kind: AssetKind, tx: &Sender<Event>) {
    let mut files = Vec::new();
    collect_files(base, &mut files);

    for path in files {
        if !is_allowed_extension(kind, &path) {
            continue;
        }

        let key = match asset_key(base, &path) {
            Some(key) => key,
            None => {
                continue;
            }
        };

        let bytes = match fs::read(&path) {
            Ok(bytes) => bytes,
            Err(_) => {
                continue;
            }
        };

        let _ = tx.send(Event {
            kind,
            name: key,
            bytes,
        });
    }
}

fn collect_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(entries) => entries,
        Err(_) => {
            return;
        }
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_files(&path, out);
        } else if path.is_file() {
            out.push(path);
        }
    }
}

fn is_allowed_extension(kind: AssetKind, path: &Path) -> bool {
    let ext = match path.extension().and_then(|ext| ext.to_str()) {
        Some(ext) => ext.to_ascii_lowercase(),
        None => return false,
    };

    match kind {
        AssetKind::Texture => ext == "png",
        AssetKind::Font => ext == "ttf" || ext == "otf",
    }
}

fn asset_key(base: &Path, path: &Path) -> Option<String> {
    let rel = path.strip_prefix(base).ok()?;
    let rel_no_ext = rel.with_extension("");
    let mut key = String::new();

    for (idx, part) in rel_no_ext.components().enumerate() {
        if idx > 0 {
            key.push('/');
        }
        key.push_str(&part.as_os_str().to_string_lossy());
    }

    if key.is_empty() { None } else { Some(key) }
}
