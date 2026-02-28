mod assets;
mod board;
mod build_ui;
mod draw;
mod simulation;
mod things;

use crate::{assets::*, things::*};
use board::*;
use gui::Gui;
use macroquad::prelude as mq;
use simulation::*;
use strum::{EnumCount, EnumIter};
use util::arena::Arena;
use util::geom::*;

#[derive(Default)]
pub(crate) struct UiData {
    pub open_panels: [bool; Panel::COUNT],
    pub is_paused: bool,
}

impl UiData {
    const NUM_MESSAGE_PER_PAGE: usize = 10;
}

#[derive(Clone, Copy, EnumIter, EnumCount, Debug)]
pub(crate) enum Panel {
    Dummy,
    Messages,
    Orders,
}

impl Default for Panel {
    fn default() -> Self {
        Self::Dummy
    }
}

impl UiData {
    pub fn is_panel_open(&self, panel: Panel) -> bool {
        self.open_panels[panel as usize]
    }
}

fn main() {
    use tracing_subscriber::layer::SubscriberExt;

    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(tracing_tracy::TracyLayer::default()),
    )
    .expect("setup tracy layer");

    tracing_tracy::client::set_thread_name!("Main  Thread");

    let config = mq::Conf {
        window_width: 1600,
        window_height: 900,
        ..Default::default()
    };
    macroquad::Window::from_config(config, amain());
}

#[derive(Default, Clone, Copy, Debug)]
struct Command {
    kind: CommandKind,
    thing: ThingId,
    panel: Panel,
    list: List,
    num: f32,
}

impl Command {
    fn with_thing(kind: CommandKind, thing: ThingId) -> Self {
        Self {
            kind,
            thing,
            ..Default::default()
        }
    }

    fn with_num(kind: CommandKind, num: f32) -> Self {
        Self {
            kind,
            num,
            ..Default::default()
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum CommandKind {
    Nothing,
    Despawn,
    DespawnAllInList,
    SetSelectedEntity,
    SetSelectedMessage,
    TogglePanel,
    ChangeMessagePage,
}

impl Default for CommandKind {
    fn default() -> Self {
        Self::Nothing
    }
}

async fn amain() {
    // Arena that is never reset
    let eternal_arena = Arena::new();
    // Arena that is reset per frame
    let mut frame_arena = Arena::new();

    // Necessary on certain annoying platforms that do not want to obey my screen sizings...
    mq::request_new_screen_size(mq::screen_width(), mq::screen_height());
    mq::next_frame().await;

    let mut sim = setup(&frame_arena);

    let mut board = Board::new(&frame_arena);
    board.set_camera(mq::vec2(600., 500.), 20.);
    let world_font = mq::load_ttf_font("assets/fonts/board.ttf").await.unwrap();
    let ui_font = mq::load_ttf_font("assets/fonts/ui_bold.ttf").await.unwrap();
    let overlay_font = mq::load_ttf_font("assets/fonts/board.ttf").await.unwrap();

    let sprite_atlas =
        load_texture_atlas(&eternal_arena, &frame_arena, "assets/atlas/out/pawns").await;

    let mut gui = Gui::default();
    let mut gui_renderer = board::GuiRenderer::new();

    let mut ui_data = UiData::default();

    let mut request = crate::simulation::Request::default();
    let mut response = simulation::tick(&mut sim, Request::default(), &frame_arena);

    loop {
        tracing_tracy::client::frame_mark();
        request.select_entity = response.selected_entity;

        request.message_page = response.messages.current_page;
        request.messages_per_page = UiData::NUM_MESSAGE_PER_PAGE;
        request.message_expended = response.messages.expanded.map(|x| x.0).unwrap_or_default();

        let gui_output;

        {
            let mut commands = frame_arena.new_vec_with_capacity(10);

            if mq::is_key_pressed(mq::KeyCode::Escape) {
                return;
            }
            if mq::is_key_pressed(mq::KeyCode::M) {
                commands.push(Command {
                    kind: CommandKind::TogglePanel,
                    panel: Panel::Messages,
                    ..Default::default()
                });
            }

            if mq::is_key_pressed(mq::KeyCode::O) {
                commands.push(Command {
                    kind: CommandKind::TogglePanel,
                    panel: Panel::Orders,
                    ..Default::default()
                });
            }

            gui_output = build_ui::root(
                &mut gui,
                &frame_arena,
                &sim,
                &response,
                &ui_data,
                &mut commands,
            );

            if !gui_output.is_mouse_over_ui && mq::is_mouse_button_pressed(mq::MouseButton::Left) {
                commands.push(Command::with_thing(
                    CommandKind::SetSelectedEntity,
                    board.hovered_id(),
                ));
            }

            // Perform commands
            for command in commands {
                match command.kind {
                    CommandKind::Nothing => {}
                    CommandKind::Despawn => request.despawns.push(command.thing),
                    CommandKind::DespawnAllInList => {
                        sim.things.with_commands(|ctx, commands| {
                            for id in ctx.iter_list(command.list, command.thing) {
                                commands.despawn(id);
                            }
                        });
                    }
                    CommandKind::SetSelectedEntity => request.select_entity = command.thing,
                    CommandKind::SetSelectedMessage => request.message_expended = command.thing,
                    CommandKind::TogglePanel => {
                        let idx = command.panel as usize;
                        if ui_data.open_panels[idx] {
                            ui_data.open_panels[idx] = false;
                        } else {
                            ui_data.open_panels = Default::default();
                            ui_data.open_panels[idx] = true;
                        }
                    }
                    CommandKind::ChangeMessagePage => {
                        request.message_page = (response.messages.current_page as i32
                            + command.num as i32)
                            .max(0) as usize
                    }
                }
            }
        };

        let time_speed = if ui_data.is_paused {
            0
        } else if mq::is_key_down(mq::KeyCode::LeftShift) {
            5
        } else {
            1
        };
        {
            let _span = tracing::info_span!("Present").entered();
            {
                let mut translation = mq::Vec2::ZERO;
                let mut zoom = 0.0;
                for (key, dx, dy, dz) in [
                    (mq::KeyCode::W, 0., -1., 0.),
                    (mq::KeyCode::S, 0., 1., 0.),
                    (mq::KeyCode::A, -1., 0., 0.),
                    (mq::KeyCode::D, 1., 0., 0.),
                    (mq::KeyCode::Q, 0., 0., 1.),
                    (mq::KeyCode::E, 0., 0., -1.),
                ] {
                    if mq::is_key_down(key) {
                        translation.x += dx;
                        translation.y += dy;
                        zoom += dz
                    }
                }

                board.update_camera(translation, zoom);
            }

            if mq::is_key_pressed(mq::KeyCode::Space) {
                ui_data.is_paused = !ui_data.is_paused;
            }

            response.draw_data.prepare();

            // Actuall draw to screen
            mq::clear_background(mq::LIGHTGRAY);
            board.draw(&response.draw_data, &sprite_atlas, &world_font);
            gui_renderer.draw(&frame_arena, gui_output.draw_list, &ui_font, &sprite_atlas);

            if time_speed == 0 {
                draw_overlay_text(
                    "Paused",
                    mq::WHITE,
                    Some(&overlay_font),
                    100,
                    mq::vec2(0.5, 0.1),
                );
            } else if time_speed > 1 {
                draw_overlay_text(
                    frame_arena.fmt(format_args!("Speed {time_speed}x")),
                    mq::WHITE,
                    Some(&overlay_font),
                    100,
                    mq::vec2(0.5, 0.1),
                );
            }
        }

        // Having realeased the simulation, send off the request. This way, simulation could work in parallel with us
        request.delta = mq::get_frame_time();
        request.advance_time = time_speed;

        std::mem::drop(response);
        frame_arena.reset();

        response = simulation::tick(&mut sim, std::mem::take(&mut request), &frame_arena);
        mq::next_frame().await;
    }
}

fn draw_overlay_text(
    text: &str,
    color: mq::Color,
    font: Option<&mq::Font>,
    font_size: u16,
    anchor: mq::Vec2,
) {
    let text_dims = mq::measure_text(text, font, font_size, 1.0);
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
