use macroquad::prelude as mq;
use util::arena::Arena;

use crate::csv;

#[derive(Clone, Copy, Default)]
struct TerrainBand {
    max_height: f32,
    atlas_i: u8,
    atlas_j: u8,
}

pub(crate) struct TerrainRenderer {
    material: mq::Material,
    key_texture: mq::Texture2D,
    atlas_texture: mq::Texture2D,
}

impl TerrainRenderer {
    pub(crate) fn new(arena: &Arena) -> Self {
        let vert = std::fs::read_to_string("assets/shaders/terrain.vert")
            .expect("failed to read terrain vertex shader");
        let frag = std::fs::read_to_string("assets/shaders/terrain.frag")
            .expect("failed to read terrain fragment shader");

        let material = mq::load_material(
            mq::ShaderSource::Glsl {
                vertex: &vert,
                fragment: &frag,
            },
            mq::MaterialParams {
                uniforms: vec![mq::UniformDesc::new("pixel_scale", mq::UniformType::Float2)],
                textures: vec!["terrain_key".to_owned(), "terrain_atlas".to_owned()],
                ..Default::default()
            },
        )
        .expect("failed to create terrain material");

        let heightmap = load_png_image("assets/britain.png");
        let bands = load_terrain_bands(arena, "data/terrain_bands.csv");

        let mut key_image = mq::Image::gen_image_color(
            heightmap.width,
            heightmap.height,
            mq::Color::from_rgba(0, 0, 0, 0),
        );

        for y in 0..(heightmap.height as usize) {
            for x in 0..(heightmap.width as usize) {
                let height = heightmap.get_pixel(x as u32, y as u32).r;
                let [atlas_i, atlas_j] = pick_terrain_type(height, &bands);
                key_image.set_pixel(
                    x as u32,
                    y as u32,
                    mq::Color::from_rgba(atlas_i, atlas_j, 0, 255),
                );
            }
        }

        let key_texture = mq::Texture2D::from_image(&key_image);
        key_texture.set_filter(mq::FilterMode::Nearest);

        let atlas_image = load_png_image("assets/gfx/terrain_types.png");
        let atlas_texture = mq::Texture2D::from_image(&atlas_image);
        atlas_texture.set_filter(mq::FilterMode::Nearest);

        Self {
            material,
            key_texture,
            atlas_texture,
        }
    }

    pub(crate) fn draw(&self) {
        let world_size = mq::vec2(self.key_texture.width(), self.key_texture.height());
        self.material.set_uniform("pixel_scale", world_size);
        self.material
            .set_texture("terrain_key", self.key_texture.clone());
        self.material
            .set_texture("terrain_atlas", self.atlas_texture.clone());

        mq::gl_use_material(&self.material);
        mq::draw_rectangle(0., 0., world_size.x, world_size.y, mq::WHITE);
        mq::gl_use_default_material();
    }
}

fn load_png_image(path: &str) -> mq::Image {
    let bytes = std::fs::read(path).unwrap_or_else(|_| panic!("failed to read {path}"));
    mq::Image::from_file_with_format(&bytes, Some(mq::ImageFormat::Png))
        .unwrap_or_else(|_| panic!("failed to decode png {path}"))
}

fn load_terrain_bands(arena: &Arena, path: &str) -> Vec<TerrainBand> {
    let source_text = std::fs::read_to_string(path).unwrap_or_default();
    let table = csv::parse(arena, &source_text);

    let mut bands = Vec::new();
    for row in table.rows() {
        let h = row[0].as_str();
        if h.is_empty() || h.starts_with('#') {
            continue;
        }

        bands.push(TerrainBand {
            max_height: row[0].as_num(),
            atlas_i: row[1].as_num() as u8,
            atlas_j: row[2].as_num() as u8,
        });
    }

    bands.sort_by(|a, b| a.max_height.total_cmp(&b.max_height));
    bands
}

fn pick_terrain_type(height: f32, bands: &[TerrainBand]) -> [u8; 2] {
    let mut selected = bands.last().copied().unwrap_or_default();
    for band in bands {
        if height <= band.max_height {
            selected = *band;
            break;
        }
    }
    [selected.atlas_i, selected.atlas_j]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn picks_first_matching_band() {
        let bands = [
            TerrainBand {
                max_height: 0.2,
                atlas_i: 1,
                atlas_j: 0,
            },
            TerrainBand {
                max_height: 0.5,
                atlas_i: 2,
                atlas_j: 0,
            },
            TerrainBand {
                max_height: 1.0,
                atlas_i: 3,
                atlas_j: 0,
            },
        ];

        assert_eq!(pick_terrain_type(0.1, &bands), [1, 0]);
        assert_eq!(pick_terrain_type(0.5, &bands), [2, 0]);
        assert_eq!(pick_terrain_type(0.9, &bands), [3, 0]);
    }

    #[test]
    fn falls_back_to_last_band_for_high_values() {
        let bands = [
            TerrainBand {
                max_height: 0.3,
                atlas_i: 1,
                atlas_j: 1,
            },
            TerrainBand {
                max_height: 0.8,
                atlas_i: 2,
                atlas_j: 2,
            },
        ];

        assert_eq!(pick_terrain_type(0.95, &bands), [2, 2]);
    }
}
