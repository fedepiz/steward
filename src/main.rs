mod board;

use board::{LabelDesc, PawnDesc, Stroke};
use macroquad::prelude as mq;
use simulation::{MapItemId, SimulationActor};
use tracing_subscriber::layer::SubscriberExt;

fn main() {
    // 1. Setup the subscriber with the Tracy layer
    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(tracing_tracy::TracyLayer::default()),
    )
    .expect("setup tracy layer");

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
    let text_size = 26;

    let mut vec = bumpalo::vec![in arena];
    vec.extend(items.iter().map(|item| {
        let is_selected = Some(item.id) == selected;

        PawnDesc {
            bounds: mq::Rect::new(item.x, item.y, item.width, item.height),
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

const TILE_SIZE: f32 = 20.;

#[derive(Default)]
struct Actions {
    move_to_pos: Option<mq::Vec2>,
    move_to_item: Option<MapItemId>,
}

impl Actions {
    fn apply(self, request: &mut simulation::Request) {
        request.move_to_pos = self.move_to_pos.map(|pos| (pos.x, pos.y));
        request.move_to_item = self.move_to_item;
    }
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
        board::Board::new(
            TILE_SIZE,
            mq::screen_width() as u32,
            mq::screen_height() as u32,
            font,
        )
    };
    board.move_camera_to(mq::vec2(580., 512.));
    let billboard_font =
        mq::load_ttf_font_from_bytes(include_bytes!("../assets/fonts/board.ttf")).unwrap();

    let mut arena = bumpalo::Bump::new();

    let mut sim_actor = SimulationActor::start();
    let mut is_paused = false;

    let mut reload = true;
    let mut actions = Actions::default();

    // Main game loop
    loop {
        let tracing_span = tracing::info_span!("Main Loop").entered();

        arena.reset();

        let mut request = simulation::Request::new();
        std::mem::take(&mut actions).apply(&mut request);

        if reload {
            let map_image = mq::load_image("assets/britain.png").await.unwrap();
            request.init = Some(simulation::InitRequest {
                map_width: map_image.width(),
                map_height: map_image.height(),
                elevations: map_image
                    .bytes
                    .chunks(4)
                    .map(|x| ((x[0] as f32 / 255.) * 100.) as u8)
                    .collect(),
            });
            request.extract_terrain = true;
            reload = false;
        }

        request.advance_time = !is_paused && !reload && !sim_actor.has_critical_error();

        request.higlighted_item = selected_item;

        if let Some(id) = selected_item {
            request.view_map_item("selected_item", id)
        }

        let response = sim_actor.tick(request);

        let map_items = &response.map_items;

        // Draw egui overlay
        let mut ui_wants_pointer = false;
        let mut ui_wants_keyboard = false;
        egui_macroquad::ui(|ctx| {
            gui(ctx, response);
            ui_wants_pointer = ctx.wants_pointer_input();
            ui_wants_keyboard = ctx.wants_keyboard_input();
        });

        // Reset board for new frame and add pawns
        board.reset();

        for desc in make_pawns(&arena, map_items, selected_item) {
            board.add_pawn(desc);
        }

        if !ui_wants_pointer {
            let mouse_pos = mq::mouse_position().into();
            let hovered_pawn = board.pick_pawn(mouse_pos);
            let hoevered_item = hovered_pawn.map(|id| map_items.get_by_index(id.0).id);

            if mq::is_mouse_button_pressed(mq::MouseButton::Left) {
                selected_item = hoevered_item;
            }

            let world_pos = board.coords_of(mouse_pos);
            if mq::is_mouse_button_pressed(mq::MouseButton::Right) {
                if hovered_pawn.is_some() {
                    actions.move_to_item = hoevered_item;
                } else {
                    actions.move_to_pos = Some(world_pos);
                }
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
        board.draw(response.map_terrain.as_ref());

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

        let (billboard_text, billboard_color) = if sim_actor.has_critical_error() {
            ("Critical Error", mq::RED)
        } else if is_paused {
            ("Paused", mq::WHITE)
        } else {
            ("", mq::WHITE)
        };

        if !billboard_text.is_empty() {
            draw_billboard_text(billboard_text, billboard_color, &billboard_font);
        }

        std::mem::drop(tracing_span);
        mq::next_frame().await;
    }
}

fn draw_billboard_text(text: &str, color: mq::Color, font: &mq::Font) {
    let font_size = 100.0;
    let text_dims = mq::measure_text(text, Some(font), font_size as u16, 1.0);

    let x = mq::screen_width() / 2.0 - text_dims.width / 2.0;
    let y = mq::screen_height() * 0.33 - text_dims.height / 2.0 + text_dims.offset_y;

    mq::draw_text_ex(
        text,
        x,
        y,
        mq::TextParams {
            font: Some(font),
            font_size: font_size as u16,
            color,
            ..Default::default()
        },
    );
}

fn gui(ctx: &egui::Context, response: &simulation::Response) {
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
                    ui.end_row();
                    ui.label("Renown");
                    ui.label(objs.str(root, "renown"));
                    ui.end_row();
                })
        });
    }
}
