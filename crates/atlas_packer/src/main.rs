use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use image::GenericImage;
use serde::Serialize;

const DEFAULT_INPUT_DIR: &str = "assets/atlas/src";
const DEFAULT_OUTPUT_DIR: &str = "assets/atlas/out";
const MIN_ATLAS_SIZE: u32 = 256;

#[derive(Serialize)]
struct AtlasFile {
    image: String,
    size: [u32; 2],
    sprites: HashMap<String, AtlasRect>,
}

#[derive(Serialize, Clone, Copy)]
struct AtlasRect {
    x: u32,
    y: u32,
    w: u32,
    h: u32,
}

struct Sprite {
    name: String,
    image: image::RgbaImage,
    w: u32,
    h: u32,
    x: u32,
    y: u32,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let input_dir = args.get(1).map(String::as_str).unwrap_or(DEFAULT_INPUT_DIR);
    let output_dir = args
        .get(2)
        .map(String::as_str)
        .unwrap_or(DEFAULT_OUTPUT_DIR);

    let input_dir = Path::new(input_dir);
    let output_dir = Path::new(output_dir);

    let mut sprites = match load_sprites(input_dir) {
        Ok(sprites) => sprites,
        Err(err) => {
            eprintln!("atlas_packer: {err}");
            std::process::exit(1);
        }
    };

    if sprites.is_empty() {
        eprintln!("atlas_packer: no sprites found in {}", input_dir.display());
        std::process::exit(1);
    }

    pack_sprites(&mut sprites);
    if let Err(err) = write_outputs(output_dir, &sprites) {
        eprintln!("atlas_packer: {err}");
        std::process::exit(1);
    }
}

fn load_sprites(input_dir: &Path) -> Result<Vec<Sprite>, String> {
    let mut files = Vec::new();
    collect_pngs(input_dir, &mut files)?;

    let mut sprites = Vec::new();
    for path in files {
        let name = sprite_name(input_dir, &path)
            .ok_or_else(|| format!("failed to derive sprite name for {}", path.display()))?;
        let image = image::open(&path)
            .map_err(|err| format!("failed to read {}: {err}", path.display()))?
            .to_rgba8();
        let (w, h) = image.dimensions();
        sprites.push(Sprite {
            name,
            image,
            w,
            h,
            x: 0,
            y: 0,
        });
    }

    Ok(sprites)
}

fn collect_pngs(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries =
        fs::read_dir(dir).map_err(|err| format!("failed to read dir {}: {err}", dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|err| format!("failed to read dir entry: {err}"))?;
        let path = entry.path();
        if path.is_dir() {
            collect_pngs(&path, out)?;
        } else if path.is_file() && is_png(&path) {
            out.push(path);
        }
    }
    Ok(())
}

fn is_png(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.eq_ignore_ascii_case("png"))
        .unwrap_or(false)
}

fn sprite_name(base: &Path, path: &Path) -> Option<String> {
    let rel = path.strip_prefix(base).ok()?;
    let rel_no_ext = rel.with_extension("");
    let mut name = String::new();
    for (idx, part) in rel_no_ext.components().enumerate() {
        if idx > 0 {
            name.push('/');
        }
        name.push_str(&part.as_os_str().to_string_lossy());
    }
    if name.is_empty() { None } else { Some(name) }
}

fn pack_sprites(sprites: &mut [Sprite]) {
    sprites.sort_by(|a, b| b.h.cmp(&a.h).then_with(|| b.w.cmp(&a.w)));

    let max_w = sprites.iter().map(|s| s.w).max().unwrap_or(1);
    let atlas_w = next_pow2(max_w.max(MIN_ATLAS_SIZE));

    let mut x = 0;
    let mut y = 0;
    let mut row_h = 0;

    for sprite in sprites.iter_mut() {
        if x + sprite.w > atlas_w {
            y += row_h;
            x = 0;
            row_h = 0;
        }

        sprite.x = x;
        sprite.y = y;

        x += sprite.w;
        row_h = row_h.max(sprite.h);
    }
}

fn write_outputs(output_dir: &Path, sprites: &[Sprite]) -> Result<(), String> {
    fs::create_dir_all(output_dir)
        .map_err(|err| format!("failed to create {}: {err}", output_dir.display()))?;

    let atlas_w = sprites
        .iter()
        .map(|s| s.x + s.w)
        .max()
        .unwrap_or(MIN_ATLAS_SIZE)
        .max(MIN_ATLAS_SIZE);
    let atlas_h = sprites
        .iter()
        .map(|s| s.y + s.h)
        .max()
        .unwrap_or(MIN_ATLAS_SIZE)
        .max(MIN_ATLAS_SIZE);

    let mut atlas = image::RgbaImage::new(atlas_w, atlas_h);
    for sprite in sprites {
        atlas
            .copy_from(&sprite.image, sprite.x, sprite.y)
            .map_err(|err| format!("failed to blit {}: {err}", sprite.name))?;
    }

    let atlas_path = output_dir.join("atlas.png");
    atlas
        .save(&atlas_path)
        .map_err(|err| format!("failed to write {}: {err}", atlas_path.display()))?;

    let mut sprite_map = HashMap::new();
    for sprite in sprites {
        sprite_map.insert(
            sprite.name.clone(),
            AtlasRect {
                x: sprite.x,
                y: sprite.y,
                w: sprite.w,
                h: sprite.h,
            },
        );
    }

    let atlas_data = AtlasFile {
        image: "atlas.png".to_string(),
        size: [atlas_w, atlas_h],
        sprites: sprite_map,
    };

    let ron = ron::ser::to_string_pretty(
        &atlas_data,
        ron::ser::PrettyConfig::new().new_line("\n".to_string()),
    )
    .map_err(|err| format!("failed to serialize atlas: {err}"))?;

    let ron_path = output_dir.join("atlas.ron");
    fs::write(&ron_path, ron)
        .map_err(|err| format!("failed to write {}: {err}", ron_path.display()))?;

    Ok(())
}

fn next_pow2(value: u32) -> u32 {
    value.next_power_of_two()
}
