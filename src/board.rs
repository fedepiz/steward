use crate::{assets::TextureAtlas, terrain::TerrainRenderer, things::ThingId};
use macroquad::prelude as mq;
use util::arena::{AVec, Arena};

#[derive(Clone, Copy, Default)]
pub(crate) struct Path {
    pub start: mq::Vec2,
    pub end: mq::Vec2,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Sprite {
    pub image: &'static str,
    pub bounds: mq::Rect,
    pub layer: u8,
    pub border_highlight: mq::Color,
    pub pulse_intensity: f32,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Label<'a> {
    pub text: &'a str,
    pub pos: mq::Vec2,
    pub font_size: u16,
    pub color: mq::Color,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Clickbox {
    pub id: ThingId,
    pub bounds: mq::Rect,
}

pub(crate) struct DrawData<'a> {
    pub paths: AVec<'a, Path>,
    pub sprites: AVec<'a, Sprite>,
    pub labels: AVec<'a, Label<'a>>,
    pub clickboxes: AVec<'a, Clickbox>,
}

impl<'a> DrawData<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            paths: arena.new_vec_with_capacity(1000),
            sprites: arena.new_vec_with_capacity(1000),
            labels: arena.new_vec_with_capacity(1000),
            clickboxes: arena.new_vec_with_capacity(1000),
        }
    }

    pub fn prepare(&mut self) {
        self.sprites.sort_by_key(|x| x.layer);
    }
}

pub(crate) struct Board {
    camera: mq::Camera2D,
    hovered_id: ThingId,
    path_shader: mq::Material,
    path_texture: mq::Texture2D,
    sprite_shader: mq::Material,
}

impl Board {
    pub(crate) fn new() -> Board {
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

        let mut board = Self {
            camera: mq::Camera2D::default(),
            hovered_id: ThingId::default(),
            path_shader,
            path_texture,
            sprite_shader,
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
        terrain_renderer: &TerrainRenderer,
        sprite_atlas: &TextureAtlas,
        font: &mq::Font,
    ) {
        mq::push_camera_state();
        mq::set_camera(&self.camera);

        terrain_renderer.draw();

        let time = mq::get_time() as f32;

        {
            const PATH_THICKNESS: f32 = 1.0;

            self.path_shader
                .set_texture("tex", self.path_texture.clone());

            mq::gl_use_material(&self.path_shader);

            for path in &draw_data.paths {
                let delta = path.end - path.start;
                let length = delta.length();
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
                        pivot: Some(path.start),
                        ..Default::default()
                    },
                );
            }
            mq::gl_use_default_material();
        }

        {
            self.sprite_shader.set_uniform("time", time);
            self.sprite_shader
                .set_texture("atlas_texture", sprite_atlas.texture().clone());
            mq::gl_use_material(&self.sprite_shader);

            for sprite in &draw_data.sprites {
                self.sprite_shader
                    .set_uniform("border_highlight", (sprite.border_highlight,));
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
                        dest_size: Some(sprite.bounds.size()),
                        ..Default::default()
                    },
                );
            }
            mq::gl_use_default_material();
        }

        for label in &draw_data.labels {
            let font = Some(font);
            let scale = 2. / (self.camera.zoom.x * mq::screen_width());
            let measure = mq::measure_text(label.text, font, label.font_size, scale);
            let pad = 4. * scale;
            let vspace = pad;

            let pos = label.pos + mq::vec2(-measure.width / 2., measure.height);
            mq::draw_rectangle(
                pos.x - pad,
                pos.y - pad + vspace - measure.offset_y,
                measure.width + pad * 2.,
                measure.height + pad * 2.,
                mq::BLACK.with_alpha(0.5),
            );
            mq::draw_text_ex(
                label.text,
                pos.x,
                pos.y + vspace,
                mq::TextParams {
                    font,
                    font_size: label.font_size,
                    color: label.color,
                    font_scale: scale,
                    ..Default::default()
                },
            );
        }
        mq::pop_camera_state();

        {
            let mouse_pos: mq::Vec2 = mq::mouse_position().into();
            let pick_pos = self.camera.screen_to_world(mouse_pos);
            self.hovered_id = draw_data
                .clickboxes
                .iter()
                .rev()
                .find(|cb| cb.bounds.contains(pick_pos))
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
