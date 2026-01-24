mod assets;
mod board;

use board::{LabelDesc, PawnDesc, Stroke};
use macroquad::prelude as mq;
use simulation::{MapItemId, SimulationActor};
use tracing_subscriber::layer::SubscriberExt;

#[cfg(target_os = "windows")]
const GLOBAL_SCALING: f32 = 1.;

#[cfg(target_os = "macos")]
const GLOBAL_SCALING: f32 = 1.;

#[cfg(not(target_os = "windows"))]
#[cfg(not(target_os = "macos"))]
const GLOBAL_SCALING: f32 = 1.5;

fn main() {
    // 1. Setup the subscriber with the Tracy layer
    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(tracing_tracy::TracyLayer::default()),
    )
    .expect("setup tracy layer");

    let conf = mq::Conf {
        window_title: "Steward".to_string(),
        window_width: (1600. * GLOBAL_SCALING) as i32,
        window_height: (900. * GLOBAL_SCALING) as i32,
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
        let fill = mq::Color::from_rgba(item.color.0, item.color.1, item.color.2, 255);

        PawnDesc {
            image: item.image,
            bounds: mq::Rect::new(item.x, item.y, item.width, item.height),
            fill,
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
        ctx.set_pixels_per_point(1.6 * GLOBAL_SCALING as f32);
    });

    let mut selected_item = None;
    let mut assets = assets::Assets::start_loading("assets");

    let mut board = board::Board::new(
        TILE_SIZE,
        mq::screen_width() as u32,
        mq::screen_height() as u32,
    );

    board.move_camera_to(mq::vec2(580., 512.));

    let mut arena = bumpalo::Bump::new();

    let mut sim_actor = SimulationActor::start();
    let mut is_paused = false;
    let mut show_fps = false;

    let mut reload = true;
    let mut actions = Actions::default();

    const DESIRED_FPS: f32 = 60.;
    const TICK_RATE_SECS: f32 = 1. / DESIRED_FPS;
    let mut next_tick_time: f32 = 0.0;
    // Main game loop
    loop {
        let tracing_span = tracing::info_span!("Main Loop").entered();

        arena.reset();

        assets.tick();

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

        let has_tick = {
            next_tick_time += mq::get_frame_time();
            let has_tick = (next_tick_time / TICK_RATE_SECS).floor().max(0.) as u32;
            next_tick_time = next_tick_time % TICK_RATE_SECS;
            has_tick
        };

        let tick_speed = {
            let mut speed = 1;
            if mq::is_key_down(mq::KeyCode::LeftControl) {
                speed *= 2;
            }
            if mq::is_key_down(mq::KeyCode::LeftShift) {
                speed *= 5;
            }
            speed
        };
        request.advance_ticks = has_tick * tick_speed;

        if is_paused || reload || sim_actor.has_critical_error() {
            request.advance_ticks = 0;
        }

        request.highlighted_item = selected_item;

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
            let hovered_item = hovered_pawn.map(|id| map_items.get_by_index(id.0).id);

            if mq::is_mouse_button_pressed(mq::MouseButton::Left) {
                selected_item = hovered_item;
            }

            let world_pos = board.coords_of(mouse_pos);
            if mq::is_mouse_button_pressed(mq::MouseButton::Right) {
                if hovered_pawn.is_some() {
                    actions.move_to_item = hovered_item;
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

            if mq::is_key_pressed(mq::KeyCode::F12) {
                show_fps = !show_fps;
            }
        }

        // Render board to its texture
        board.draw(&assets, response.map_terrain.as_ref());

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

        {
            let (text, color) = if sim_actor.has_critical_error() {
                ("Critical Error", mq::RED)
            } else if is_paused {
                ("Paused", mq::WHITE)
            } else if tick_speed > 1 {
                let text = bumpalo::format!(in &arena, "Speed {}x", tick_speed);
                (text.into_bump_str(), mq::WHITE)
            } else {
                ("", mq::WHITE)
            };

            let font = assets.get_font("board");
            if !text.is_empty() {
                draw_overlay_text(text, color, font, 100.0, mq::vec2(0.5, 0.25));
            }

            if show_fps {
                let text = format!("{} fps", mq::get_fps());
                draw_overlay_text(&text, mq::WHITE, font, 24.0, mq::vec2(1.0, 1.0));
            }
        }
        std::mem::drop(tracing_span);
        mq::next_frame().await;
    }
}

fn draw_overlay_text(
    text: &str,
    color: mq::Color,
    font: Option<&mq::Font>,
    font_size: f32,
    anchor: mq::Vec2,
) {
    let text_dims = mq::measure_text(text, font, font_size as u16, 1.0);
    let max_x = (mq::screen_width() - text_dims.width).max(0.0);
    let max_y = (mq::screen_height() - text_dims.height).max(0.0);

    let x = max_x * anchor.x;
    let y = max_y * anchor.y + text_dims.offset_y;

    mq::draw_text_ex(
        text,
        x,
        y,
        mq::TextParams {
            font,
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

                    let vars = [
                        ("renown", "Renown"),
                        ("population", "Population"),
                        ("food", "Food"),
                        ("prosperity", "Prosperity"),
                        ("goods", "Goods"),
                        ("minerals", "Minerals"),
                        ("farmer_opportunity", "Farmer Opportunity"),
                        ("miner_opportunity", "Miner Opportunity"),
                        ("caravan_opportunity", "Caravan Opportunity"),
                    ];
                    for (id, label) in vars {
                        if let Some(value) = objs.try_str(root, id) {
                            ui.label(label);
                            ui.label(value);
                            ui.end_row();
                        }
                    }
                })
        });
    }
}
