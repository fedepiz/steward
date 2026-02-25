use std::cmp::Reverse;
use std::collections::HashSet;
use std::error::Error;
use std::ffi::OsStr;
use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};

use image::{GenericImage, ImageFormat, Rgba, RgbaImage};

const DEFAULT_SRC_ROOT: &str = "assets/atlas/src";
const DEFAULT_OUT_ROOT: &str = "assets/atlas/out";
const SPRITE_MARGIN: u32 = 4;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug)]
struct SourceImage {
    name: String,
    image: RgbaImage,
}

impl SourceImage {
    fn width(&self) -> u32 {
        self.image.width()
    }

    fn height(&self) -> u32 {
        self.image.height()
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct Placement {
    x: u32,
    y: u32,
    w: u32,
    h: u32,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("atlas_packer error: {err}");
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let src_root = args
        .next()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(DEFAULT_SRC_ROOT));
    let out_root = args
        .next()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(DEFAULT_OUT_ROOT));

    if args.next().is_some() {
        return Err("usage: atlas_packer [src_root] [out_root]".into());
    }

    let atlas_dirs = collect_atlas_dirs(&src_root)?;
    if atlas_dirs.is_empty() {
        println!(
            "No atlas source directories found in {}",
            src_root.display()
        );
        return Ok(());
    }

    fs::create_dir_all(&out_root)?;
    for src_dir in atlas_dirs {
        pack_one_atlas(&src_dir, &out_root)?;
    }

    Ok(())
}

fn collect_atlas_dirs(src_root: &Path) -> Result<Vec<PathBuf>> {
    let mut dirs = Vec::new();
    for entry in fs::read_dir(src_root)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            dirs.push(entry.path());
        }
    }

    dirs.sort_by_key(|p| p.file_name().map(|name| name.to_os_string()));
    Ok(dirs)
}

fn pack_one_atlas(src_dir: &Path, out_root: &Path) -> Result<()> {
    let atlas_name = src_dir
        .file_name()
        .and_then(OsStr::to_str)
        .ok_or_else(|| format!("Invalid atlas directory name: {}", src_dir.display()))?
        .to_owned();

    let images = collect_source_images(src_dir)?;
    if images.is_empty() {
        println!("Skipping {} (no png files)", src_dir.display());
        return Ok(());
    }

    let (atlas_width, atlas_height, placements) = compute_layout(&images);
    let atlas_image = render_atlas(&images, &placements, atlas_width, atlas_height)?;
    write_atlas_outputs(out_root, &atlas_name, &atlas_image, &images, &placements)?;

    println!(
        "Packed atlas '{}' -> {} sprites, {}x{}",
        atlas_name,
        images.len(),
        atlas_width,
        atlas_height
    );

    Ok(())
}

fn collect_source_images(src_dir: &Path) -> Result<Vec<SourceImage>> {
    let mut png_paths = Vec::new();
    for entry in fs::read_dir(src_dir)? {
        let entry = entry?;
        if !entry.file_type()?.is_file() {
            continue;
        }

        if is_png(entry.path().as_path()) {
            png_paths.push(entry.path());
        }
    }

    png_paths.sort_by_key(|p| p.file_name().map(|name| name.to_os_string()));

    let mut seen = HashSet::new();
    let mut images = Vec::with_capacity(png_paths.len());
    for path in png_paths {
        let name = path
            .file_stem()
            .and_then(OsStr::to_str)
            .ok_or_else(|| format!("Invalid sprite file name: {}", path.display()))?
            .to_owned();

        if !seen.insert(name.clone()) {
            return Err(format!("Duplicate sprite name '{name}' in {}", src_dir.display()).into());
        }

        let image = image::open(&path)
            .map_err(|err| format!("Failed to read png {}: {err}", path.display()))?
            .to_rgba8();

        images.push(SourceImage { name, image });
    }

    Ok(images)
}

fn is_png(path: &Path) -> bool {
    path.extension()
        .and_then(OsStr::to_str)
        .is_some_and(|ext| ext.eq_ignore_ascii_case("png"))
}

fn compute_layout(images: &[SourceImage]) -> (u32, u32, Vec<Placement>) {
    if images.is_empty() {
        return (0, 0, Vec::new());
    }

    let mut order: Vec<usize> = (0..images.len()).collect();
    order.sort_by_key(|&i| {
        (
            Reverse(images[i].height()),
            Reverse(images[i].width()),
            &images[i].name,
        )
    });

    let max_w = images.iter().map(SourceImage::width).max().unwrap_or(1);
    let total_area: u64 = images
        .iter()
        .map(|image| image.width() as u64 * image.height() as u64)
        .sum();
    let area_based = (total_area as f64).sqrt().ceil() as u32;

    let mut width = max_w.max(1).next_power_of_two();
    let target = area_based.max(max_w).next_power_of_two();
    let max_candidate = target.saturating_mul(4).max(width);

    let mut best: Option<(u64, u64, u64, u32, u32, Vec<Placement>)> = None;
    loop {
        let (height, placements) = layout_with_width(images, &order, width);
        let area = width as u64 * height as u64;
        let ratio_penalty = (width as i64 - height as i64).unsigned_abs() as u64;
        let score = area.saturating_mul(10) + ratio_penalty.saturating_mul(ratio_penalty);

        match &best {
            Some((best_score, best_area, best_penalty, ..))
                if score > *best_score
                    || (score == *best_score
                        && (area > *best_area
                            || (area == *best_area && ratio_penalty >= *best_penalty))) => {}
            _ => best = Some((score, area, ratio_penalty, width, height, placements)),
        }

        if width >= max_candidate {
            break;
        }

        let next = width.saturating_mul(2);
        if next == width {
            break;
        }
        width = next;
    }

    let (_, _, _, best_w, best_h, placements) = best.expect("layout candidates are never empty");
    (best_w, best_h, placements)
}

fn layout_with_width(images: &[SourceImage], order: &[usize], width: u32) -> (u32, Vec<Placement>) {
    let mut placements = vec![Placement::default(); images.len()];

    let mut cursor_x = 0u32;
    let mut cursor_y = 0u32;
    let mut row_height = 0u32;

    for &idx in order {
        let w = images[idx].width();
        let h = images[idx].height();

        if cursor_x > 0 && cursor_x + w > width {
            cursor_y += row_height + SPRITE_MARGIN;
            cursor_x = 0;
            row_height = 0;
        }

        placements[idx] = Placement {
            x: cursor_x,
            y: cursor_y,
            w,
            h,
        };

        cursor_x += w + SPRITE_MARGIN;
        row_height = row_height.max(h);
    }

    let atlas_height = if order.is_empty() {
        0
    } else {
        cursor_y + row_height
    };
    (atlas_height, placements)
}

fn render_atlas(
    images: &[SourceImage],
    placements: &[Placement],
    width: u32,
    height: u32,
) -> Result<RgbaImage> {
    let mut atlas = RgbaImage::from_pixel(width, height, Rgba([0, 0, 0, 0]));

    for (image, placement) in images.iter().zip(placements.iter()) {
        atlas.copy_from(&image.image, placement.x, placement.y)?;
    }

    Ok(atlas)
}

fn write_atlas_outputs(
    out_root: &Path,
    atlas_name: &str,
    atlas_image: &RgbaImage,
    images: &[SourceImage],
    placements: &[Placement],
) -> Result<()> {
    let atlas_dir = out_root.join(atlas_name);
    fs::create_dir_all(&atlas_dir)?;

    let atlas_png = atlas_dir.join("atlas.png");
    atlas_image.save_with_format(&atlas_png, ImageFormat::Png)?;

    let mut csv = String::new();
    csv.push_str("name,x,y,w,h\n");
    for (image, placement) in images.iter().zip(placements.iter()) {
        let name = csv_escape(&image.name);
        writeln!(
            &mut csv,
            "{},{},{},{},{}",
            name, placement.x, placement.y, placement.w, placement.h
        )?;
    }

    fs::write(atlas_dir.join("atlas.csv"), csv)?;
    Ok(())
}

fn csv_escape(value: &str) -> String {
    if value.contains([',', '"', '\n', '\r']) {
        format!("\"{}\"", value.replace('"', "\"\""))
    } else {
        value.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_respects_margin() {
        let images = vec![
            test_image("a", 24, 10),
            test_image("b", 12, 18),
            test_image("c", 8, 8),
            test_image("d", 16, 16),
        ];

        let (_, _, placements) = compute_layout(&images);

        for i in 0..placements.len() {
            for j in (i + 1)..placements.len() {
                assert!(has_required_margin(
                    placements[i],
                    placements[j],
                    SPRITE_MARGIN
                ));
            }
        }
    }

    #[test]
    fn csv_escape_quotes_when_needed() {
        assert_eq!(csv_escape("normal"), "normal");
        assert_eq!(csv_escape("a,b"), "\"a,b\"");
        assert_eq!(csv_escape("a\"b"), "\"a\"\"b\"");
    }

    fn test_image(name: &str, width: u32, height: u32) -> SourceImage {
        SourceImage {
            name: name.to_owned(),
            image: RgbaImage::from_pixel(width, height, Rgba([255, 255, 255, 255])),
        }
    }

    fn has_required_margin(a: Placement, b: Placement, margin: u32) -> bool {
        let gap_x = if a.x + a.w <= b.x {
            b.x - (a.x + a.w)
        } else if b.x + b.w <= a.x {
            a.x - (b.x + b.w)
        } else {
            0
        };

        let gap_y = if a.y + a.h <= b.y {
            b.y - (a.y + a.h)
        } else if b.y + b.h <= a.y {
            a.y - (b.y + b.h)
        } else {
            0
        };

        if gap_x == 0 && gap_y == 0 {
            return false;
        }

        if gap_x == 0 {
            return gap_y >= margin;
        }

        if gap_y == 0 {
            return gap_x >= margin;
        }

        true
    }
}
