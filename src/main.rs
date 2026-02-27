mod assets;
mod board;
mod build_ui;
mod simulation;
mod things;

use std::sync::{Arc, Mutex};

use crate::build_ui::{Panel, UiData};
use crate::{assets::*, things::*};
use board::*;
use gui::Gui;
use macroquad::prelude as mq;
use simulation::*;
use util::arena::Arena;
use util::geom::*;

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
}

impl Command {
    fn with_thing(kind: CommandKind, thing: ThingId) -> Self {
        Self {
            kind,
            thing,
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

    let sim = setup(&frame_arena);

    let mut board = Board::new(&frame_arena);
    board.set_camera(mq::vec2(600., 500.), 20.);
    let world_font = mq::load_ttf_font("assets/fonts/board.ttf").await.unwrap();
    let ui_font = mq::load_ttf_font("assets/fonts/ui_bold.ttf").await.unwrap();

    let sprite_atlas =
        load_texture_atlas(&eternal_arena, &frame_arena, "assets/atlas/out/pawns").await;

    let mut gui = Gui::default();
    let mut gui_renderer = board::GuiRenderer::new();

    let sim = Arc::new(Mutex::new(sim));
    let (req_tx, req_rx) = std::sync::mpsc::channel();

    {
        let sim = Arc::clone(&sim);
        std::thread::spawn(move || {
            tracing_tracy::client::set_thread_name!("Simulation thread");

            loop {
                match req_rx.recv() {
                    Ok(request) => {
                        let mut sim = sim.lock().unwrap();
                        tick(&mut sim, request);
                    }
                    Err(_) => {
                        return;
                    }
                }
            }
        });
    }

    let mut ui_data = UiData::default();

    loop {
        tracing_tracy::client::frame_mark();
        frame_arena.reset();
        let mut draw_data = DrawData::new(&frame_arena);

        if mq::is_key_pressed(mq::KeyCode::Escape) {
            return;
        }

        let gui_output;

        {
            // This part "locks" the simulation
            let sim = &mut *sim.lock().unwrap();
            let _span = tracing::info_span!("Locked Sim").entered();
            let mut commands = frame_arena.new_vec_with_capacity(10);

            gui_output =
                build_ui::root(&mut gui, &frame_arena, &sim.things, &ui_data, &mut commands);

            render_things(&mut draw_data, &sim.things, ui_data.selected_entity);

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
                    CommandKind::Despawn => {
                        sim.things.despawn(command.thing);
                    }
                    CommandKind::DespawnAllInList => {
                        sim.things.with_commands(|ctx, commands| {
                            for id in ctx.iter_list(List::Messages, command.thing) {
                                commands.despawn(id);
                            }
                        });
                    }
                    CommandKind::SetSelectedEntity => ui_data.selected_entity = command.thing,
                    CommandKind::SetSelectedMessage => ui_data.selected_message = command.thing,
                    CommandKind::TogglePanel => ui_data.toggle_panel(command.panel),
                }
            }
        };

        // Having realeased the simulation, send off the request. This way, simulation could work in parallel with us
        let mut request = Request::default();
        request.delta = mq::get_frame_time();
        request.advance_time = 1;
        req_tx.send(request).unwrap();

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

            draw_data.prepare();

            // Actuall draw to screen
            mq::clear_background(mq::LIGHTGRAY);
            board.draw(&draw_data, &sprite_atlas, &world_font);
            gui_renderer.draw(&frame_arena, gui_output.draw_list, &ui_font);
        }
        mq::next_frame().await;
    }
}

fn render_things(draw_data: &mut DrawData, things: &Things, selected_id: ThingId) {
    let _span = tracing::info_span!("main::render_things").entered();
    // "Render" entities
    things.readonly_pass(|ctx, this| {
        if !this.flag(Flag::IsVisible) {
            return;
        }
        if this.body.size > 0 && !this.sprite().is_empty() {
            let is_selected = this.id() == selected_id;

            let size = this.body.size as f32;
            let xy = mq::Vec2::new(this.body.x, this.body.y) - size / 2.;
            let bounds = mq::Rect::new(xy.x, xy.y, size, size);

            let sprite = Sprite {
                image: this.sprite(),
                bounds,
                layer: this.body.layer,
                border_highlight: if is_selected {
                    mq::YELLOW.with_alpha(0.9)
                } else {
                    mq::Color::new(0., 0., 0., 0.)
                },
                pulse_intensity: if is_selected { 1.0 } else { 0.0 },
            };
            draw_data.sprites.push(sprite);

            let show_name = is_selected || this.flag(Flag::IsLocation);
            if show_name {
                let name = this.name();
                if !name.is_empty() {
                    let color = if is_selected { mq::YELLOW } else { mq::WHITE };
                    let layer = this.body.layer.max(if is_selected { 3 } else { 0 });
                    draw_data.labels.push(Label {
                        text: name,
                        pos: xy + mq::vec2(size / 2., size),
                        font_size: 24,
                        color,
                        layer,
                    });
                }
            }

            draw_data.clickboxes.push(Clickbox {
                id: this.id(),
                bounds,
            });
        }
        if this.flag(Flag::IsPath) {
            let a = this.link(self::Link::A);
            let b = this.link(self::Link::B);
            if a.is_valid() && b.is_valid() {
                let a_pos = mq::vec2(ctx[a].body.x, ctx[a].body.y);
                let b_pos = mq::vec2(ctx[b].body.x, ctx[b].body.y);
                draw_data.paths.push(Path {
                    start: a_pos,
                    end: b_pos,
                });
            }
        }
    });
}

fn render_message<'a>(arena: &'a Arena, ctx: &Things, message: ThingId) -> &'a str {
    let message = &ctx[message];
    let params = &[
        ctx[message.link(Link::A)].name(),
        ctx[message.link(Link::B)].name(),
    ];
    render_template_string(arena, message.name(), params)
}

fn render_template_string<'a>(arena: &'a Arena, template: &str, params: &[&str]) -> &'a str {
    let mut buffer = arena.new_string_with_capacity(template.len() * 2);

    let mut iter = template.chars();
    while let Some(ch) = iter.next() {
        if ch != '#' {
            buffer.push(ch);
        } else {
            if let Some(next) = iter.next() {
                if let Some(digit) = next.to_digit(10) {
                    let value = params.get(digit as usize).copied().unwrap_or("???");
                    buffer.push_str(value);
                } else {
                    buffer.push('#');
                    buffer.push(next);
                }
            } else {
                buffer.push('#');
            }
        }
    }

    buffer.into_bump_str()
}
