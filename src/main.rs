mod board;

use board::{Label, Pawn, Stroke};
use macroquad::prelude as mq;

fn main() {
    let conf = mq::Conf {
        window_title: "Steward".to_string(),
        window_width: 1600,
        window_height: 900,
        high_dpi: true,
        ..Default::default()
    };

    macroquad::Window::from_config(conf, amain());
}

/// Processes keyboard input for camera control.
/// WASD for translation, Q/E for zoom.
fn keyboard_input(board: &mut board::Board) {
    const CAMERA_SPEED: f32 = 200.0; // World units per second
    const ZOOM_SPEED: f32 = 1.0; // Zoom factor per second

    // Translation bindings: (key, dx, dy) where d values are direction multipliers
    const TRANSLATION_BINDINGS: &[(mq::KeyCode, f32, f32)] = &[
        (mq::KeyCode::W, 0.0, -1.0),
        (mq::KeyCode::S, 0.0, 1.0),
        (mq::KeyCode::A, -1.0, 0.0),
        (mq::KeyCode::D, 1.0, 0.0),
    ];

    // Zoom bindings: (key, direction) where positive zooms in, negative zooms out
    const ZOOM_BINDINGS: &[(mq::KeyCode, f32)] = &[(mq::KeyCode::Q, 1.0), (mq::KeyCode::E, -1.0)];

    let dt = mq::get_frame_time();

    // Accumulate translation from all pressed keys
    let mut camera_delta = mq::Vec2::ZERO;
    for &(key, dx, dy) in TRANSLATION_BINDINGS {
        if mq::is_key_down(key) {
            camera_delta.x += dx * CAMERA_SPEED * dt;
            camera_delta.y += dy * CAMERA_SPEED * dt;
        }
    }
    board.move_camera(camera_delta);

    // Accumulate zoom from all pressed keys
    let mut zoom_delta = 0.0;
    for &(key, dz) in ZOOM_BINDINGS {
        if mq::is_key_down(key) {
            zoom_delta += dz * ZOOM_SPEED * dt;
        }
    }
    if zoom_delta != 0.0 {
        // Convert additive delta to multiplicative factor
        board.zoom_camera(1.0 + zoom_delta);
    }
}

async fn amain() {
    // Configure egui scaling for high DPI
    egui_macroquad::cfg(|ctx| {
        ctx.set_pixels_per_point(1.6);
    });

    let mut board = board::Board::new();

    // Test pawns demonstrating different rendering features
    let pawns = vec![
        // Red pawn: fill + stroke + label
        Pawn {
            bounds: mq::Rect::new(-50.0, -50.0, 100.0, 100.0),
            fill: mq::RED,
            stroke: Stroke {
                color: mq::BLACK,
                thickness: 3.0,
            },
            label: Label {
                text: "Red Pawn".to_string(),
                size: 20.0,
                color: mq::WHITE,
            },
        },
        // Blue pawn: fill + label (no stroke)
        Pawn {
            bounds: mq::Rect::new(100.0, -30.0, 80.0, 80.0),
            fill: mq::BLUE,
            label: Label {
                text: "Blue".to_string(),
                ..Default::default()
            },
            ..Default::default()
        },
        // Green pawn: stroke only (no fill, no label)
        Pawn {
            bounds: mq::Rect::new(-150.0, 50.0, 60.0, 60.0),
            fill: mq::Color::new(0.0, 0.0, 0.0, 0.0),
            stroke: Stroke {
                color: mq::GREEN,
                thickness: 2.0,
            },
            ..Default::default()
        },
    ];

    // Main game loop
    loop {
        // Exit on Escape
        if mq::is_key_pressed(mq::KeyCode::Escape) {
            break;
        }

        keyboard_input(&mut board);

        // Draw egui overlay
        egui_macroquad::ui(|ctx| {
            egui::Window::new("Hello").show(ctx, |ui| {
                ui.label("Hello, World!");
            });
        });

        // Render frame
        mq::clear_background(mq::LIGHTGRAY);
        board.draw(&pawns);
        egui_macroquad::draw();

        mq::next_frame().await;
    }
}
