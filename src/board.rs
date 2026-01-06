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

    /// Draws the pawn's label in screen space.
    /// Called after resetting to default camera to keep text crisp at any zoom level.
    /// The camera is passed to convert world position to screen coordinates.
    fn draw_label(&self, camera: &mq::Camera2D) {
        if self.label.text.is_empty() || self.label.size <= 0.0 {
            return;
        }

        // Convert the bottom-center of the pawn from world space to screen space
        let world_pos = mq::vec2(
            self.bounds.x + self.bounds.w / 2.0,
            self.bounds.y + self.bounds.h,
        );
        let screen_pos = camera.world_to_screen(world_pos);

        // Measure text to center it horizontally
        let text_dims = mq::measure_text(&self.label.text, None, self.label.size as u16, 1.0);
        let label_x = screen_pos.x - text_dims.width / 2.0;
        let label_y = screen_pos.y + text_dims.height + 8.0;

        // Draw semi-transparent background behind text
        let padding = 4.0;
        mq::draw_rectangle(
            label_x - padding,
            label_y - text_dims.height - padding,
            text_dims.width + padding * 2.0,
            text_dims.height + padding * 2.0,
            mq::Color::new(0.0, 0.0, 0.0, 0.6),
        );

        // Draw the label text
        mq::draw_text(
            &self.label.text,
            label_x,
            label_y,
            self.label.size,
            self.label.color,
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
    /// Shapes are drawn in world space (affected by camera).
    /// Labels are drawn in screen space (always crisp, fixed size).
    pub fn draw(&self, pawns: &[Pawn]) {
        // Draw shapes in world space with camera transformation
        mq::set_camera(&self.camera);
        for pawn in pawns {
            pawn.draw_shape();
        }

        // Draw labels in screen space to keep text crisp at any zoom level
        mq::set_default_camera();
        for pawn in pawns {
            pawn.draw_label(&self.camera);
        }
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}
