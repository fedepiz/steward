use crate::draw::*;
use crate::{assets::TextureAtlas, things::ThingId};
use macroquad::prelude as mq;
use util::{
    arena::Arena,
    geom::{Rect, V2},
};

trait ToMq {
    type Out;

    fn to_mq(self) -> Self::Out;
}

impl ToMq for V2 {
    type Out = mq::Vec2;

    fn to_mq(self) -> Self::Out {
        mq::vec2(self.x, self.y)
    }
}

impl ToMq for Rect {
    type Out = mq::Rect;

    fn to_mq(self) -> Self::Out {
        mq::Rect::new(self.x, self.y, self.w, self.h)
    }
}

pub(crate) struct Board {
    camera: mq::Camera2D,
    hovered_id: ThingId,
    path_shader: mq::Material,
    path_texture: mq::Texture2D,
    sprite_shader: mq::Material,
    terrain_renderer: TerrainRenderer,
}

impl Board {
    pub(crate) fn new(scratch: &Arena) -> Board {
        let path_vert = std::fs::read_to_string("assets/shaders/path.vert")
            .expect("failed to read path vertex shader");
        let path_frag = std::fs::read_to_string("assets/shaders/path.frag")
            .expect("failed to read path fragment shader");
        let path_shader = mq::load_material(
            mq::ShaderSource::Glsl {
                vertex: &path_vert,
                fragment: &path_frag,
            },
            mq::MaterialParams {
                pipeline_params: sensible_pipeline_params(),
                uniforms: vec![mq::UniformDesc::new("path_size", mq::UniformType::Float2)],
                textures: vec!["tex".to_owned()],
                ..Default::default()
            },
        )
        .expect("failed to load path shader material");
        let path_texture = load_png_texture("assets/atlas/src/pawns/path2.png");
        path_texture.set_filter(mq::FilterMode::Nearest);

        let vert = std::fs::read_to_string("assets/shaders/sprite.vert")
            .expect("failed to read sprite vertex shader");
        let frag = std::fs::read_to_string("assets/shaders/sprite.frag")
            .expect("failed to read sprite fragment shader");

        let sprite_shader = mq::load_material(
            mq::ShaderSource::Glsl {
                vertex: &vert,
                fragment: &frag,
            },
            mq::MaterialParams {
                pipeline_params: sensible_pipeline_params(),
                uniforms: vec![
                    mq::UniformDesc::new("border_highlight", mq::UniformType::Float4),
                    mq::UniformDesc::new("time", mq::UniformType::Float1),
                    mq::UniformDesc::new("pulse_intensity", mq::UniformType::Float1),
                ],
                textures: vec!["atlas_texture".to_owned()],
                ..Default::default()
            },
        )
        .expect("failed to load sprite shader material");

        let terrain_renderer = TerrainRenderer::new(scratch);

        let mut board = Self {
            camera: mq::Camera2D::default(),
            hovered_id: ThingId::default(),
            path_shader,
            path_texture,
            sprite_shader,
            terrain_renderer,
        };

        let rect = mq::Rect::new(0., 0., mq::screen_width(), mq::screen_height());
        board.camera = mq::Camera2D::from_display_rect(rect);
        board.camera.target = mq::Vec2::ZERO;
        // For some reason, macroquad's coords seem to be upside-down
        board.camera.zoom.y *= -1.;
        board
    }

    pub(crate) fn hovered_id(&self) -> ThingId {
        self.hovered_id
    }

    pub(crate) fn set_camera(&mut self, translation: mq::Vec2, zoom: f32) {
        self.camera.target = translation;
        self.camera.zoom = mq::vec2(zoom / mq::screen_width(), zoom / mq::screen_height()) * 2.;
    }

    pub(crate) fn update_camera(&mut self, translation: mq::Vec2, zoom: f32) {
        let dt = mq::get_frame_time();
        let zoom_rate = self.camera.zoom.x * mq::screen_width() / 2.;
        self.camera.target += translation * 400. * dt / zoom_rate;
        self.camera.zoom *= 1.0 + (dt * zoom);
    }

    pub(crate) fn draw(
        &mut self,
        draw_data: &DrawData,
        sprite_atlas: &TextureAtlas,
        font: &mq::Font,
    ) {
        let max_layer = draw_data
            .sprites
            .iter()
            .map(|sprite| sprite.layer)
            .chain(draw_data.labels.iter().map(|label| label.layer))
            .max()
            .unwrap_or(0);

        mq::push_camera_state();
        mq::set_camera(&self.camera);

        self.terrain_renderer.draw();

        let time = mq::get_time() as f32;

        {
            const PATH_THICKNESS: f32 = 1.0;

            self.path_shader
                .set_texture("tex", self.path_texture.clone());

            mq::gl_use_material(&self.path_shader);

            for path in &draw_data.paths {
                let delta = path.end - path.start;
                let length = delta.magnitude();
                if length <= 0.0001 {
                    continue;
                }

                self.path_shader
                    .set_uniform("path_size", (length, PATH_THICKNESS));

                let angle = delta.y.atan2(delta.x);
                mq::draw_texture_ex(
                    &self.path_texture,
                    path.start.x,
                    path.start.y - PATH_THICKNESS * 0.5,
                    mq::WHITE,
                    mq::DrawTextureParams {
                        dest_size: Some(mq::vec2(length, PATH_THICKNESS)),
                        rotation: angle,
                        pivot: Some(path.start.to_mq()),
                        ..Default::default()
                    },
                );
            }
            mq::gl_use_default_material();
        }

        for layer in 0..=max_layer {
            // Draw labels
            for label in &draw_data.labels {
                if label.layer != layer {
                    continue;
                }

                let font = Some(font);
                let scale = 2. / (self.camera.zoom.x * mq::screen_width());
                let measure = mq::measure_text(label.text, font, label.font_size, scale);
                let pad = 4. * scale;
                let vspace = pad;

                let pos = label.pos.to_mq() + mq::vec2(-measure.width / 2., measure.height);
                mq::draw_rectangle(
                    pos.x - pad,
                    pos.y - pad + vspace - measure.offset_y,
                    measure.width + pad * 2.,
                    measure.height + pad * 2.,
                    mq::BLACK.with_alpha(0.5),
                );

                let color = if label.highighted {
                    mq::YELLOW
                } else {
                    mq::WHITE
                };
                mq::draw_text_ex(
                    label.text,
                    pos.x,
                    pos.y + vspace,
                    mq::TextParams {
                        font,
                        font_size: label.font_size,
                        color,
                        font_scale: scale,
                        ..Default::default()
                    },
                );
            }

            // Draw sprites
            {
                self.sprite_shader.set_uniform("time", time);
                self.sprite_shader
                    .set_texture("atlas_texture", sprite_atlas.texture().clone());
                mq::gl_use_material(&self.sprite_shader);

                for sprite in &draw_data.sprites {
                    if sprite.layer != layer {
                        continue;
                    }

                    let border_highlight = if sprite.border_highlight {
                        mq::YELLOW.with_alpha(0.9)
                    } else {
                        mq::Color::default()
                    };
                    self.sprite_shader
                        .set_uniform("border_highlight", (border_highlight,));
                    self.sprite_shader
                        .set_uniform("pulse_intensity", sprite.pulse_intensity);

                    let source = sprite_atlas.get(sprite.image);
                    mq::draw_texture_ex(
                        sprite_atlas.texture(),
                        sprite.bounds.x,
                        sprite.bounds.y,
                        mq::WHITE,
                        mq::DrawTextureParams {
                            source: Some(source),
                            dest_size: Some(sprite.bounds.size().to_mq()),
                            ..Default::default()
                        },
                    );
                }
                mq::gl_use_default_material();
            }
        }
        mq::pop_camera_state();

        {
            let mouse_pos: mq::Vec2 = mq::mouse_position().into();
            let pick_pos = self.camera.screen_to_world(mouse_pos);
            self.hovered_id = draw_data
                .clickboxes
                .iter()
                .rev()
                .find(|cb| cb.bounds.to_mq().contains(pick_pos))
                .map(|cb| cb.id)
                .unwrap_or_default();
        }
    }
}

fn sensible_pipeline_params() -> mq::PipelineParams {
    mq::PipelineParams {
        color_blend: Some(macroquad::miniquad::BlendState::new(
            macroquad::miniquad::Equation::Add,
            macroquad::miniquad::BlendFactor::Value(macroquad::miniquad::BlendValue::SourceAlpha),
            macroquad::miniquad::BlendFactor::OneMinusValue(
                macroquad::miniquad::BlendValue::SourceAlpha,
            ),
        )),
        ..Default::default()
    }
}

fn load_png_texture(path: &str) -> mq::Texture2D {
    let bytes = std::fs::read(path).unwrap_or_else(|_| panic!("failed to read {path}"));
    let image = mq::Image::from_file_with_format(&bytes, Some(mq::ImageFormat::Png))
        .unwrap_or_else(|_| panic!("failed to decode png {path}"));
    mq::Texture2D::from_image(&image)
}

pub(crate) struct GuiRenderer {
    material: mq::Material,
    background_texture: mq::Texture2D,
    corner_radius: f32,
    background_intensity: f32,
}

impl GuiRenderer {
    pub(crate) fn new() -> Self {
        let vert = std::fs::read_to_string("assets/shaders/widget.vert")
            .expect("failed to read widget vertex shader");
        let frag = std::fs::read_to_string("assets/shaders/widget.frag")
            .expect("failed to read widget fragment shader");

        let material = mq::load_material(
            mq::ShaderSource::Glsl {
                vertex: &vert,
                fragment: &frag,
            },
            mq::MaterialParams {
                pipeline_params: sensible_pipeline_params(),
                uniforms: vec![
                    mq::UniformDesc::new("fill_color", mq::UniformType::Float4),
                    mq::UniformDesc::new("stroke_color", mq::UniformType::Float4),
                    mq::UniformDesc::new("stroke_thickness", mq::UniformType::Float1),
                    mq::UniformDesc::new("rect_size", mq::UniformType::Float2),
                    mq::UniformDesc::new("corner_radius", mq::UniformType::Float1),
                    mq::UniformDesc::new("background_intensity", mq::UniformType::Float1),
                    mq::UniformDesc::new("atlas_region", mq::UniformType::Float4),
                    mq::UniformDesc::new("pulse_intensity", mq::UniformType::Float1),
                    mq::UniformDesc::new("time", mq::UniformType::Float1),
                    mq::UniformDesc::new("shadow_strength", mq::UniformType::Float1),
                    mq::UniformDesc::new("shadow_size", mq::UniformType::Float1),
                ],
                textures: vec!["background_tex".to_owned()],
                ..Default::default()
            },
        )
        .expect("failed to load widget shader material");

        let background_texture = load_png_texture("assets/gfx/widget.png");
        background_texture.set_filter(mq::FilterMode::Nearest);

        Self {
            material,
            background_texture,
            corner_radius: 6.0,
            background_intensity: 0.25,
        }
    }

    pub(crate) fn draw(
        &mut self,
        scratch: &Arena,
        draw_list: &[gui::Draw],
        font: &mq::Font,
        icons: &TextureAtlas,
    ) {
        let mq_color = |x: gui::RGBA| mq::Color::new(x.r, x.g, x.b, x.a);

        mq::gl_use_default_material();

        self.material
            .set_texture("background_tex", self.background_texture.clone());

        let time = mq::get_time() as f32;

        for item in draw_list {
            let bounds = item.bounds;
            let has_stroke = item.stroke.0.a > 0.0 && item.stroke.1 > 0.0;
            let has_fill = item.fill.a > 0.0;

            if has_fill || has_stroke {
                self.material
                    .set_uniform("fill_color", (mq_color(item.fill),));
                self.material
                    .set_uniform("stroke_color", (mq_color(item.stroke.0),));
                self.material.set_uniform("stroke_thickness", item.stroke.1);
                self.material
                    .set_uniform("rect_size", (bounds.w.max(0.0), bounds.h.max(0.0)));
                self.material
                    .set_uniform("corner_radius", self.corner_radius);
                self.material.set_uniform(
                    "background_intensity",
                    if has_fill {
                        self.background_intensity
                    } else {
                        0.0
                    },
                );
                self.material
                    .set_uniform("atlas_region", (0.0_f32, 0.0_f32, 1.0_f32, 1.0_f32));
                self.material.set_uniform("pulse_intensity", item.pulse);
                self.material.set_uniform("time", time);
                self.material.set_uniform("shadow_strength", item.shadow);
                self.material.set_uniform("shadow_size", item.bounds.h);

                mq::gl_use_material(&self.material);
                mq::draw_rectangle(bounds.x, bounds.y, bounds.w, bounds.h, mq::WHITE);
                mq::gl_use_default_material();
            }

            let text = &item.text;
            if !text.string.is_empty() {
                let multiline = !text.centering[0] && !text.centering[1];
                let parts = wrap_text(scratch, &text.string, bounds, font, text.size, multiline);

                let align_x = if text.centering[0] {
                    ((item.bounds.w - parts.content_width) / 2.0).max(0.0)
                } else {
                    0.0
                };
                let align_y = if text.centering[1] {
                    ((item.bounds.h - parts.content_height) / 2.0).max(0.0)
                } else {
                    0.0
                };

                for part in parts.fragments {
                    if !part.is_sprite {
                        mq::draw_text_ex(
                            part.text,
                            part.pos.x + align_x,
                            part.pos.y + align_y,
                            mq::TextParams {
                                font: Some(font),
                                font_size: text.size,
                                color: mq_color(text.color),
                                ..Default::default()
                            },
                        );
                    } else {
                        let source = icons.get(part.text);
                        let size = part.width as f32;
                        mq::draw_texture_ex(
                            icons.texture(),
                            part.pos.x + align_x,
                            part.pos.y + align_y,
                            mq::WHITE,
                            mq::DrawTextureParams {
                                dest_size: Some(mq::vec2(size, size)),
                                source: Some(source),
                                ..Default::default()
                            },
                        );
                    }
                }
            }
        }
    }
}

struct Fragment<'a> {
    text: &'a str,
    pos: V2,
    is_sprite: bool,
    width: f32,
}

#[derive(Default)]
struct WrappedText<'a> {
    fragments: &'a [Fragment<'a>],
    content_width: f32,
    content_height: f32,
}

fn wrap_text<'a>(
    arena: &'a Arena,
    text: &'a str,
    bounds: Rect,
    font: &mq::Font,
    font_size: u16,
    multiline: bool,
) -> WrappedText<'a> {
    let mut fragments = arena.new_vec();
    let mut truncated = false;

    let mut cursor = bounds.corner();
    let mut max_line_width: f32 = 0.0;
    let mut line_count = 1;

    let measure = mq::measure_text("C", Some(font), font_size, 1.);
    let line_advance = measure.height + measure.offset_y;
    let offset_y = measure.offset_y;
    let line_height = measure.height;

    let mut skip_next = false;
    for word in words(text) {
        if skip_next {
            skip_next = false;
            continue;
        }
        let is_newline = word == "\n";
        if is_newline {
            // New line
            if !multiline {
                truncated = true;
                break;
            }
            max_line_width = max_line_width.max(cursor.x - bounds.x);
            cursor.x = bounds.x;
            cursor.y += line_advance;
            line_count += 1;
        } else {
            let is_sprite = word.starts_with("$sprite$");
            let word = if is_sprite {
                word.split_terminator('$').skip(2).next().unwrap_or("")
            } else {
                word
            };

            const SPRITE_SCALE: f32 = 2.;
            let width = if is_sprite {
                font_size as f32 * SPRITE_SCALE
            } else {
                mq::measure_text(word, Some(font), font_size, 1.).width
            };

            if cursor.x + measure.width > bounds.x + bounds.w {
                // New line
                if !multiline {
                    truncated = true;
                    break;
                }
                max_line_width = max_line_width.max(cursor.x - bounds.x);
                cursor.x = bounds.x;
                cursor.y += line_advance;
                line_count += 1;
            }
            if cursor.y > bounds.y + bounds.h {
                break;
            }
            let y = if is_sprite {
                -width * (SPRITE_SCALE - 1.) / 4.
            } else {
                offset_y
            };
            fragments.push(Fragment {
                text: word,
                pos: cursor + V2::new(0., y),
                is_sprite,
                width,
            });
            skip_next = is_sprite;
            cursor.x += width;
        }
    }

    if !multiline && truncated {
        // Keep room for an ellipsis by trimming already-laid-out fragments if needed.
        while let Some(last) = fragments.last() {
            if !last.text.chars().all(|ch| ch.is_whitespace()) {
                break;
            }
            let width = mq::measure_text(last.text, Some(font), font_size, 1.).width;
            cursor.x -= width;
            fragments.pop();
        }

        let right = bounds.x + bounds.w;
        let ellipsis = ["...", "..", "."]
            .into_iter()
            .find(|token| mq::measure_text(token, Some(font), font_size, 1.).width <= bounds.w)
            .unwrap_or("");

        if !ellipsis.is_empty() {
            let width = mq::measure_text(ellipsis, Some(font), font_size, 1.).width;

            while cursor.x + width > right {
                let Some(last) = fragments.pop() else {
                    break;
                };
                cursor.x -= mq::measure_text(last.text, Some(font), font_size, 1.).width;
            }

            if cursor.x + width <= right {
                fragments.push(Fragment {
                    text: ellipsis,
                    pos: cursor + V2::new(0., offset_y),
                    is_sprite: false,
                    width,
                });
                cursor.x += width;
            }
        }
    }

    max_line_width = max_line_width.max(cursor.x - bounds.x);
    let content_height = if fragments.is_empty() {
        0.0
    } else {
        line_height + (line_count - 1) as f32 * line_advance
    };

    WrappedText {
        fragments: fragments.into_bump_slice(),
        content_width: max_line_width,
        content_height,
    }
}

fn words(source: &str) -> WordsIter<'_> {
    WordsIter { source, cursor: 0 }
}

struct WordsIter<'a> {
    source: &'a str,
    cursor: usize,
}

impl<'a> Iterator for WordsIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.source.len() {
            return None;
        }

        let start = self.cursor;
        let mut chars = self.source[start..].char_indices();
        let (_, first) = chars.next()?;

        if first.is_whitespace() {
            let end = start + first.len_utf8();
            self.cursor = end;
            return Some(&self.source[start..end]);
        }

        let mut end = self.source.len();
        for (offset, ch) in chars {
            if ch.is_whitespace() {
                end = start + offset;
                break;
            }
        }

        self.cursor = end;
        Some(&self.source[start..end])
    }
}

#[derive(Clone, Copy, Default)]
struct TerrainBand {
    max_height: f32,
    atlas_i: u8,
    atlas_j: u8,
}

struct TerrainRenderer {
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
