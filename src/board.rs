use macroquad::prelude as mq;

/// Stroke style for pawn outlines.
/// Set thickness to 0 to disable stroke.
#[derive(Default)]
pub struct Stroke {
    pub color: mq::Color,
    pub thickness: f32,
}

/// Text label displayed below a pawn.
/// Set size to 0 or text to empty to disable label.
#[derive(Default)]
pub struct Label {
    pub text: String,
    pub size: f32,
    pub color: mq::Color,
}

/// A drawable game piece on the board.
/// Features are disabled via zero/default values rather than Option types.
#[derive(Default)]
pub struct Pawn {
    pub bounds: mq::Rect,
    pub fill: mq::Color,
    pub stroke: Stroke,
    pub label: Label,
}

impl Pawn {
    /// Draws the pawn's shape (fill and stroke) in world space.
    /// Called with the camera active.
    fn draw_shape(&self) {
        // Draw fill only if alpha > 0 (not transparent)
        if self.fill.a > 0.0 {
            mq::draw_rectangle(
                self.bounds.x,
                self.bounds.y,
                self.bounds.w,
                self.bounds.h,
                self.fill,
            );
        }

        // Draw stroke only if thickness > 0 and not transparent
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
    /// Uses dynamic font_size for crisp rendering and font_scale to maintain world dimensions.
    /// Called with camera active so background/padding scale automatically.
    fn draw_label(&self, camera: &mq::Camera2D) {
        if self.label.text.is_empty() || self.label.size <= 0.0 {
            return;
        }

        // Calculate zoom scale for crisp text rendering
        let origin_screen = camera.world_to_screen(mq::vec2(0.0, 0.0));
        let unit_screen = camera.world_to_screen(mq::vec2(0.0, 1.0));
        let zoom_scale = (unit_screen.y - origin_screen.y).abs();

        // Rasterize at screen size for crispness, scale to world size
        let screen_font_size = (self.label.size * zoom_scale).clamp(4.0, 200.0) as u16;
        let font_scale = self.label.size / screen_font_size as f32;

        // World-space constants (scale automatically with camera)
        let gap = 4.0;
        let padding = 2.0;

        // Measure text in world units
        let text_dims = mq::measure_text(&self.label.text, None, screen_font_size, font_scale);

        // Position at bottom-center of pawn, centered horizontally
        let text_x = self.bounds.x + self.bounds.w / 2.0 - text_dims.width / 2.0;
        let text_y = self.bounds.y + self.bounds.h + gap + text_dims.height;

        // Draw semi-transparent background
        mq::draw_rectangle(
            text_x - padding,
            text_y - text_dims.height - padding,
            text_dims.width + padding * 2.0,
            text_dims.height + padding * 2.0,
            mq::Color::new(0.0, 0.0, 0.0, 0.6),
        );

        // Draw the label text
        mq::draw_text_ex(
            &self.label.text,
            text_x,
            text_y,
            mq::TextParams {
                font_size: screen_font_size,
                font_scale,
                color: self.label.color,
                ..Default::default()
            },
        );
    }
}

/// The game board, managing camera and rendering of pawns.
pub struct Board {
    pub camera: mq::Camera2D,
}

impl Board {
    pub fn new() -> Self {
        Self {
            camera: mq::Camera2D {
                target: mq::vec2(0.0, 0.0),
                // Zoom is set so that ~800x450 world units are visible
                // (half of 1600x900 window, since zoom is relative to center)
                zoom: mq::vec2(1.0 / 400.0, 1.0 / 225.0),
                ..Default::default()
            },
        }
    }

    /// Translates the camera by the given delta in world units.
    pub fn move_camera(&mut self, delta: mq::Vec2) {
        self.camera.target += delta;
    }

    /// Scales the camera zoom by the given factor.
    /// Values > 1 zoom in, values < 1 zoom out.
    pub fn zoom_camera(&mut self, factor: f32) {
        self.camera.zoom *= factor;
    }

    /// Draws all pawns on the board.
    /// Everything is drawn in world space with the camera active.
    /// Labels use dynamic font sizing for crisp rendering at any zoom level.
    pub fn draw(&self, pawns: &[Pawn]) {
        mq::set_camera(&self.camera);

        for pawn in pawns {
            pawn.draw_shape();
        }

        for pawn in pawns {
            pawn.draw_label(&self.camera);
        }

        // Reset camera for subsequent drawing (e.g., egui)
        mq::set_default_camera();
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}
