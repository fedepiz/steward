mod board;

use board::{LabelDesc, PawnDesc, Stroke};
use macroquad::prelude as mq;
use simulation::{MapItemId, SimulationActor};

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

type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;

fn make_pawns<'a, 'b>(
    arena: &'a bumpalo::Bump,
    items: &'b simulation::MapItems,
    selected: Option<MapItemId>,
) -> AVec<'a, PawnDesc<'b>> {
    let size = 20.;
    let text_size = 26;

    let mut vec = bumpalo::vec![in arena];
    vec.extend(items.iter().map(|item| {
        let is_selected = Some(item.id) == selected;

        PawnDesc {
            bounds: mq::Rect::new(
                item.x * size,
                item.y * size,
                item.width * size,
                item.height * size,
            ),
            fill: mq::RED,
            stroke: Stroke {
                color: if is_selected { mq::YELLOW } else { mq::BLACK },
                thickness: 4.,
            },
            label: LabelDesc {
                text: item.name,
                size: text_size,
                color: if is_selected { mq::YELLOW } else { mq::WHITE },
            },
        }
    }));
    vec
}

async fn amain() {
    // Configure egui scaling for high DPI
    egui_macroquad::cfg(|ctx| {
        ctx.set_pixels_per_point(1.6);
    });

    let mut selected_item = None;

    let mut board = {
        let font =
            mq::load_ttf_font_from_bytes(include_bytes!("../assets/fonts/board.ttf")).unwrap();
        board::Board::new(mq::screen_width() as u32, mq::screen_height() as u32, font)
    };

    let mut arena = bumpalo::Bump::new();

    let mut sim_actor = SimulationActor::start();
    let mut is_paused = false;

    let mut reload = true;

    // Main game loop
    loop {
        arena.reset();

        let mut request = simulation::Request::new();

        if reload {
            request.init = true;
            reload = false;
        }

        request.advance_time = !is_paused && !reload;

        if let Some(id) = selected_item {
            request.view_map_item("selected_item", id)
        }

        let response = sim_actor.tick(request);

        let map_items = &response.map_items;

        // Draw egui overlay
        let mut ui_wants_pointer = false;
        let mut ui_wants_keyboard = false;
        egui_macroquad::ui(|ctx| {
            let objs = &response.objects;
            egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
                ui.horizontal_centered(|ui| {
                    if let Some(root) = objs.lookup_by_tag("root") {
                        ui.label(objs.str(root, "tick_num"));
                    }
                });
            });
            if let Some(root) = response.objects.lookup_by_tag("selected_item") {
                egui::Window::new("Selected Item").show(ctx, |ui| {
                    egui::Grid::new("overview-grid")
                        .striped(true)
                        .show(ui, |ui| {
                            ui.label("Name");
                            ui.label(objs.str(root, "name"));
                        })
                });
            }

            ui_wants_pointer = ctx.wants_pointer_input();
            ui_wants_keyboard = ctx.wants_keyboard_input();
        });

        // Reset board for new frame and add pawns
        board.reset();
        for desc in make_pawns(&arena, map_items, selected_item) {
            board.add_pawn(desc);
        }

        if !ui_wants_pointer {
            let hovered_pawn = board.pick_pawn(mq::mouse_position().into());
            if mq::is_mouse_button_pressed(mq::MouseButton::Left) {
                selected_item = hovered_pawn.map(|id| map_items.get_by_index(id.0).id);
            }
        }

        if !ui_wants_keyboard {
            keyboard_input(&mut board);

            // Exit on Escape
            if mq::is_key_pressed(mq::KeyCode::Escape) {
                break;
            }

            if mq::is_key_pressed(mq::KeyCode::Space) {
                is_paused = !is_paused;
            }

            if mq::is_key_pressed(mq::KeyCode::R) {
                reload = true;
            }
        }

        // Render board to its texture
        board.draw();

        // Draw board texture to screen
        mq::clear_background(mq::BLACK);
        mq::draw_texture_ex(
            &board.texture(),
            0.0,
            0.0,
            mq::WHITE,
            mq::DrawTextureParams {
                dest_size: Some(mq::vec2(mq::screen_width(), mq::screen_height())),
                flip_y: true,
                ..Default::default()
            },
        );
        egui_macroquad::draw();

        mq::next_frame().await;
    }
}
