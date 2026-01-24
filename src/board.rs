use bumpalo::Bump;
use macroquad::prelude as mq;
use simulation::MapTerrain;
use util::string_pool::{SpanHandle, StringPool};

use crate::assets::Assets;

/// Label descriptor for creating pawns. Takes text by reference.
#[derive(Clone, Copy, Default)]
pub struct LabelDesc<'a> {
    pub text: &'a str,
    pub size: u16,
    pub color: mq::Color,
}

/// Pawn descriptor for adding pawns to the board. A POD that borrows label text.
#[derive(Clone, Copy, Default)]
pub struct PawnDesc<'a> {
    pub bounds: mq::Rect,
    pub fill: mq::Color,
    pub stroke: mq::Color,
    pub label: LabelDesc<'a>,
    pub image: &'static str,
}

/// Handle to a pawn in the board. Valid only for the current frame.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PawnId(pub usize);

/// Internal label storage with text as a span into the shared buffer.
#[derive(Clone, Copy, Default)]
struct Label {
    text_span: SpanHandle,
    size: u16,
    color: mq::Color,
}

/// Internal pawn storage. All data is Copy.
#[derive(Clone, Copy, Default)]
struct Pawn {
    image: &'static str,
    bounds: mq::Rect,
    fill: mq::Color,
    stroke: mq::Color,
    label: Label,
}

struct DrawLabel<'a> {
    background: mq::Rect,
    text: &'a str,
    x: f32,
    y: f32,
    params: mq::TextParams<'a>,
}

impl Pawn {
    /// Draws the pawn's shape (fill and stroke) in world space.
    fn draw_shape(&self, assets: &Assets) {
        let draw_texture = |name, color| {
            if let Some((texture, source)) = assets.get_atlas_sprite(name) {
                mq::draw_texture_ex(
                    texture,
                    self.bounds.x,
                    self.bounds.y,
                    color,
                    mq::DrawTextureParams {
                        dest_size: Some(self.bounds.size()),
                        source: Some(source),
                        ..Default::default()
                    },
                );
            }
        };

        if self.fill.a > 0.0 {
            draw_texture("background", self.fill);
        }
        draw_texture(self.image, mq::WHITE);

        if self.stroke.a > 0.0 {
            draw_texture("border", self.stroke)
        }
    }

    /// Draws the pawn's label in world space.
    /// Uses fixed font_size with inverse zoom scaling for constant screen size.
    fn draw_label<'a>(
        &'a self,
        inv_scale: f32,
        font: Option<&'a mq::Font>,
        strings: &'a StringPool,
    ) -> Option<DrawLabel<'a>> {
        let text = strings.get(self.label.text_span);
        if text.is_empty() || self.label.size == 0 {
            return None;
        }

        // Screen-space constants, scaled to world space
        let padding = 4. * inv_scale;
        let gap = 2. * padding;

        // Use fixed font_size, scale down to world space
        let font_scale = inv_scale;
        let text_dims = mq::measure_text(text, font, self.label.size, font_scale);

        // Position at bottom-center of pawn, centered horizontally
        let text_x = self.bounds.x + self.bounds.w / 2.0 - text_dims.width / 2.0;
        let text_y = self.bounds.y + self.bounds.h + gap + text_dims.height;

        Some(DrawLabel {
            background: mq::Rect::new(
                text_x - padding,
                text_y - text_dims.height - padding,
                text_dims.width + padding * 2.,
                text_dims.height + padding * 2.,
            ),
            text,
            x: text_x,
            y: text_y,
            params: mq::TextParams {
                font,
                font_size: self.label.size,
                font_scale,
                color: self.label.color,
                ..Default::default()
            },
        })
    }
}

/// The game board, managing camera, pawns, and their text storage.
/// Renders to an internal texture rather than directly to screen.
pub struct Board {
    camera: mq::Camera2D,
    terrain_texture: mq::RenderTarget,
    terrain_hash: u64,
    render_target: mq::RenderTarget,
    pawns: Vec<Pawn>,
    strings: StringPool,
    tile_size: f32,
}

impl Board {
    pub fn new(tile_size: f32, width: u32, height: u32) -> Self {
        let render_target = mq::render_target(width, height);
        render_target.texture.set_filter(mq::FilterMode::Linear);

        // Zoom so that (width/2 x height/2) world units are visible
        let mut zoom = mq::vec2(2.0 / width as f32, 2.0 / height as f32);
        // For some unfathomable and probably buggy reason, the zoom's y axis is flipped by default.
        zoom.y *= -1.;

        Self {
            camera: mq::Camera2D {
                target: mq::vec2(0.0, 0.0),
                zoom,
                render_target: Some(render_target.clone()),
                ..Default::default()
            },
            terrain_texture: mq::render_target(0, 0),
            terrain_hash: 0,
            render_target,
            pawns: Vec::new(),
            strings: StringPool::default(),
            tile_size,
        }
    }

    /// Returns the texture that the board renders to.
    pub fn texture(&self) -> mq::Texture2D {
        self.render_target.texture.clone()
    }

    /// Returns the PawnId of the topmost pawn at the given screen coordinates, or None.
    pub fn pick_pawn(&self, screen_pos: mq::Vec2) -> Option<PawnId> {
        let world_pos = self.camera.screen_to_world(screen_pos);

        // Iterate in reverse to get topmost (last drawn) pawn first
        for (i, pawn) in self.pawns.iter().enumerate().rev() {
            if pawn.bounds.contains(world_pos) {
                return Some(PawnId(i));
            }
        }
        None
    }

    pub fn coords_of(&self, screen_pos: mq::Vec2) -> mq::Vec2 {
        self.camera.screen_to_world(screen_pos) / self.tile_size
    }

    /// Clears all pawns and text for a new frame.
    pub fn reset(&mut self) {
        self.pawns.clear();
        self.strings.clear();
    }

    /// Adds a pawn to the board and returns its ID.
    pub fn add_pawn(&mut self, desc: PawnDesc) -> PawnId {
        let text_span = self.strings.push_str(desc.label.text);

        let bounds = mq::Rect::new(
            desc.bounds.x * self.tile_size,
            desc.bounds.y * self.tile_size,
            desc.bounds.w * self.tile_size,
            desc.bounds.h * self.tile_size,
        );

        let pawn = Pawn {
            image: desc.image,
            bounds,
            fill: desc.fill,
            stroke: desc.stroke,
            label: Label {
                text_span,
                size: desc.label.size,
                color: desc.label.color,
            },
        };

        let id = PawnId(self.pawns.len());
        self.pawns.push(pawn);
        id
    }

    /// Move camera to precise location
    pub fn move_camera_to(&mut self, world_coords: mq::Vec2) {
        self.camera.target = world_coords * self.tile_size;
    }

    /// Translates the camera by the given delta in world units.
    pub fn move_camera(&mut self, delta: mq::Vec2) {
        let zoom_rate = 2. / (self.camera.zoom.x * mq::screen_width());
        self.camera.target += delta * zoom_rate;
    }

    /// Scales the camera zoom by the given factor.
    /// Values > 1 zoom in, values < 1 zoom out.
    pub fn zoom_camera(&mut self, factor: f32) {
        self.camera.zoom *= factor;
    }

    fn prepare_terrain_texture(&mut self, terrain: &MapTerrain) {
        if self.terrain_hash == terrain.hash {
            return;
        }

        let (w, h) = terrain.size;
        self.terrain_texture = mq::render_target(w as u32, h as u32);
        self.terrain_texture
            .texture
            .set_filter(mq::FilterMode::Nearest);

        self.terrain_hash = terrain.hash;

        let mut camera = mq::Camera2D::from_display_rect(mq::Rect::new(0., 0., w as f32, h as f32));
        camera.render_target = Some(self.terrain_texture.clone());
        mq::push_camera_state();
        mq::set_camera(&camera);
        mq::clear_background(mq::SKYBLUE);

        for (idx, &(r, g, b)) in terrain.tiles.iter().enumerate() {
            let x = (idx % w) as f32;
            let y = (idx / w) as f32;
            let color = mq::Color::from_rgba(r, g, b, 255);
            mq::draw_rectangle(x, y, 1., 1., color);
        }

        mq::pop_camera_state();
    }

    fn visible_world_rect(&self) -> mq::Rect {
        let view_min = self.camera.screen_to_world(mq::vec2(0.0, 0.0));
        let view_max = self
            .camera
            .screen_to_world(mq::vec2(mq::screen_width(), mq::screen_height()));
        let world_min = mq::vec2(view_min.x.min(view_max.x), view_min.y.min(view_max.y));
        let world_max = mq::vec2(view_min.x.max(view_max.x), view_min.y.max(view_max.y));
        mq::Rect::new(
            world_min.x,
            world_min.y,
            world_max.x - world_min.x,
            world_max.y - world_min.y,
        )
    }

    fn rects_overlap(a: mq::Rect, b: mq::Rect) -> bool {
        let a_right = a.x + a.w;
        let a_bottom = a.y + a.h;
        let b_right = b.x + b.w;
        let b_bottom = b.y + b.h;
        a.x < b_right && a_right > b.x && a.y < b_bottom && a_bottom > b.y
    }

    fn visible_terrain_slice(&self, tex: &mq::Texture2D) -> Option<(mq::Vec2, mq::Vec2, mq::Rect)> {
        let view = self.visible_world_rect();
        let world_min = mq::vec2(view.x, view.y);
        let world_max = mq::vec2(view.x + view.w, view.y + view.h);

        let tex_size = tex.size();
        let map_w = tex_size.x * self.tile_size;
        let map_h = tex_size.y * self.tile_size;

        let clamped_min = mq::vec2(world_min.x.clamp(0.0, map_w), world_min.y.clamp(0.0, map_h));
        let clamped_max = mq::vec2(world_max.x.clamp(0.0, map_w), world_max.y.clamp(0.0, map_h));

        if clamped_min.x >= clamped_max.x || clamped_min.y >= clamped_max.y {
            return None;
        }

        let src_x = clamped_min.x / self.tile_size;
        let src_y = tex_size.y - (clamped_max.y / self.tile_size);
        let src_w = (clamped_max.x - clamped_min.x) / self.tile_size;
        let src_h = (clamped_max.y - clamped_min.y) / self.tile_size;

        Some((
            clamped_min,
            clamped_max - clamped_min,
            mq::Rect::new(src_x, src_y, src_w, src_h),
        ))
    }

    /// Draws all pawns to the internal render target.
    pub fn draw(&mut self, arena: &Bump, assets: &Assets, terrain: Option<&MapTerrain>) {
        self.camera.render_target = Some(self.render_target.clone());
        mq::set_camera(&self.camera);
        mq::clear_background(mq::LIGHTGRAY);
        let view = self.visible_world_rect();
        // Cache inverse zoom scale (world units per screen pixel) for label sizing.
        let inv_scale = {
            let origin_screen = self.camera.world_to_screen(mq::vec2(0.0, 0.0));
            let unit_screen = self.camera.world_to_screen(mq::vec2(0.0, 1.0));
            let zoom_scale = (unit_screen.y - origin_screen.y).abs();
            1. / zoom_scale
        };

        // Prepare (if necessary) and load terrain
        {
            if let Some(terrain) = terrain {
                self.prepare_terrain_texture(terrain);
            }
            let tex = &self.terrain_texture.texture;
            if tex.size() != mq::Vec2::ZERO {
                if let Some((dest_pos, dest_size, source)) = self.visible_terrain_slice(tex) {
                    mq::draw_texture_ex(
                        tex,
                        dest_pos.x,
                        dest_pos.y,
                        mq::WHITE,
                        mq::DrawTextureParams {
                            dest_size: Some(dest_size),
                            source: Some(source),
                            rotation: 0.,
                            flip_x: false,
                            flip_y: true,
                            pivot: None,
                        },
                    );
                }
            }
        }

        // Extract all the label data
        let mut draw_labels = bumpalo::collections::Vec::with_capacity_in(self.pawns.len(), arena);

        for pawn in &self.pawns {
            if Self::rects_overlap(pawn.bounds, view) {
                let draw = pawn.draw_label(inv_scale, assets.get_font("board"), &self.strings);
                draw_labels.extend(draw);
            }
        }

        for label in &draw_labels {
            mq::draw_rectangle(
                label.background.x,
                label.background.y,
                label.background.w,
                label.background.h,
                mq::Color::default().with_alpha(0.6),
            );
        }

        for label in draw_labels {
            mq::draw_text_ex(label.text, label.x, label.y, label.params);
        }

        for pawn in &self.pawns {
            if Self::rects_overlap(pawn.bounds, view) {
                pawn.draw_shape(assets);
            }
        }

        mq::set_default_camera();
    }
}
