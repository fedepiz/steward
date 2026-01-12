use macroquad::prelude as mq;
use simulation::MapTerrain;
use util::string_pool::{SpanHandle, StringPool};

/// Stroke style for pawn outlines.
/// Set thickness to 0 to disable stroke.
#[derive(Clone, Copy, Default)]
pub struct Stroke {
    pub color: mq::Color,
    pub thickness: f32,
}

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
    pub stroke: Stroke,
    pub label: LabelDesc<'a>,
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
    bounds: mq::Rect,
    fill: mq::Color,
    stroke: Stroke,
    label: Label,
}

impl Pawn {
    /// Draws the pawn's shape (fill and stroke) in world space.
    fn draw_shape(&self) {
        if self.fill.a > 0.0 {
            mq::draw_rectangle(
                self.bounds.x,
                self.bounds.y,
                self.bounds.w,
                self.bounds.h,
                self.fill,
            );
        }

        if self.stroke.thickness > 0.0 && self.stroke.color.a > 0.0 {
            mq::draw_rectangle_lines(
                self.bounds.x,
                self.bounds.y,
                self.bounds.w,
                self.bounds.h,
                self.stroke.thickness,
                self.stroke.color,
            );
        }
    }

    /// Draws the pawn's label in world space.
    /// Uses fixed font_size with inverse zoom scaling for constant screen size.
    fn draw_label(&self, camera: &mq::Camera2D, font: Option<&mq::Font>, strings: &StringPool) {
        let text = strings.get(self.label.text_span);
        if text.is_empty() || self.label.size == 0 {
            return;
        }

        // Calculate zoom scale (screen pixels per world unit)
        let origin_screen = camera.world_to_screen(mq::vec2(0.0, 0.0));
        let unit_screen = camera.world_to_screen(mq::vec2(0.0, 1.0));
        let zoom_scale = (unit_screen.y - origin_screen.y).abs();

        // Inverse scale to maintain constant screen size
        let inv_scale = 1. / zoom_scale;

        // Screen-space constants, scaled to world space
        let padding = 4. * inv_scale;
        let gap = 2. * padding;

        // Use fixed font_size, scale down to world space
        let font_scale = inv_scale;
        let text_dims = mq::measure_text(text, font, self.label.size, font_scale);

        // Position at bottom-center of pawn, centered horizontally
        let text_x = self.bounds.x + self.bounds.w / 2.0 - text_dims.width / 2.0;
        let text_y = self.bounds.y + self.bounds.h + gap + text_dims.height;

        // Draw semi-transparent background
        mq::draw_rectangle(
            text_x - padding,
            text_y - text_dims.height - padding,
            text_dims.width + padding * 2.0,
            text_dims.height + padding * 2.0,
            mq::Color::default().with_alpha(0.6),
        );

        // Draw the label text
        mq::draw_text_ex(
            text,
            text_x,
            text_y - text_dims.height + text_dims.offset_y,
            mq::TextParams {
                font,
                font_size: self.label.size,
                font_scale,
                color: self.label.color,
                ..Default::default()
            },
        );
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
    font: mq::Font,
    tile_size: f32,
}

impl Board {
    pub fn new(tile_size: f32, width: u32, height: u32, font: mq::Font) -> Self {
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
            font,
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

    /// Draws all pawns to the internal render target.
    pub fn draw(&mut self, terrain: &MapTerrain) {
        self.camera.render_target = Some(self.render_target.clone());
        mq::set_camera(&self.camera);
        mq::clear_background(mq::LIGHTGRAY);

        {
            self.prepare_terrain_texture(terrain);
            let tex = &self.terrain_texture.texture;
            if tex.size() != mq::Vec2::ZERO {
                mq::draw_texture_ex(
                    tex,
                    0.,
                    0.,
                    mq::WHITE,
                    mq::DrawTextureParams {
                        dest_size: Some(tex.size() * self.tile_size),
                        source: None,
                        rotation: 0.,
                        flip_x: false,
                        flip_y: true,
                        pivot: None,
                    },
                );
            }
        }

        for pawn in &self.pawns {
            pawn.draw_shape();
        }

        for pawn in &self.pawns {
            pawn.draw_label(&self.camera, Some(&self.font), &self.strings);
        }

        mq::set_default_camera();
    }
}
