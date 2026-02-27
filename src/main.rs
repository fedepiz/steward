mod assets;
mod board;
mod simulation;
mod things;

use std::sync::{Arc, Mutex};

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
}

#[derive(Clone, Copy, Debug)]
enum CommandKind {
    Nothing,
    Despawn,
    SetSelected,
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
    let font = mq::load_ttf_font("assets/fonts/board.ttf").await.unwrap();

    let mut selected_id = ThingId::default();

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
            let mut sim = sim.lock().unwrap();
            let _span = tracing::info_span!("Locked Sim").entered();
            let mut command = Command::default();

            gui_output = {
                let ui_data = build_ui::UiData {
                    selected_id,
                    messages: &sim.response.messages,
                    ..Default::default()
                };
                build_ui::root(&mut gui, &frame_arena, &sim.things, &ui_data, &mut command)
            };

            render_things(&mut draw_data, &sim.things, selected_id);

            if !gui_output.is_mouse_over_ui && mq::is_mouse_button_pressed(mq::MouseButton::Left) {
                command.kind = CommandKind::SetSelected;
                command.thing = board.hovered_id();
            }

            // Perform commands
            match command.kind {
                CommandKind::Nothing => {}
                CommandKind::Despawn => {
                    sim.things.despawn(command.thing);
                }
                CommandKind::SetSelected => selected_id = command.thing,
            }
        };

        // Having realeased the simulation, send off the request. This way, simulation could work in parallel with us
        let mut request = Request::default();
        request.delta = mq::get_frame_time();
        request.message_first = 0;
        request.message_count = 10;
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
            board.draw(&draw_data, &sprite_atlas, &font);
            gui_renderer.draw(gui_output.draw_list, &font);
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

mod build_ui {
    use gui::*;
    use macroquad::prelude as mq;
    use util::{arena::Arena, geom::*};

    use super::*;

    #[derive(Default)]
    pub struct UiData<'a> {
        pub selected_id: ThingId,
        pub messages: &'a [ThingId],
    }

    pub(super) fn root<'a>(
        gui: &'a mut Gui,
        arena: &'a Arena,
        things: &Things,
        data: &UiData,
        command: &mut Command,
    ) -> Output<'a> {
        gui.frame(
            arena,
            Input {
                screen_size: V2::new(mq::screen_width(), mq::screen_height()),
                mouse_pos: mq::mouse_position().into(),
                mouse_down: mq::is_mouse_button_down(mq::MouseButton::Left),
                mouse_pressed: mq::is_mouse_button_pressed(mq::MouseButton::Left),
            },
            |gui| {
                let mut gui = gui.plus();

                gui.panel(|mut gui| {
                    gui.heading("Test Panel", 4.);

                    if gui.button("Hello") {
                        println!("A");
                    }

                    if gui.button("Goodbye") {
                        println!("B");
                    }

                    if gui.button_sized("X##hello", 1., 1.) {
                        println!("X")
                    }
                });

                if !data.selected_id.is_null() {
                    let this = &things[data.selected_id];
                    gui.panel(|mut gui| {
                        gui.inner().center_on_growth_axis(false);
                        gui.inner().screen_pos(V2::new(0., 0.5));
                        gui.heading("Selected Entity", 6.);

                        gui.label(gui.arena().fmt(format_args!("Name: {}", this.name())));
                    });
                }

                if !data.messages.is_empty() {
                    gui.panel(|mut gui| {
                        gui.heading("Messages", 10.);

                        gui.inner().screen_pos(V2::new(1., 0.5));
                        gui.inner().center_on_growth_axis(false);

                        for i in 0..10 {
                            if let Some(msg_id) = data.messages.get(i).copied() {
                                gui.row(|mut gui| {
                                    let text = render_message(gui.arena(), things, msg_id);
                                    gui.line_sized(text, 9.);

                                    let btn_text =
                                        gui.arena().fmt(format_args!("X##del_msg_{}", i));

                                    if gui.button_sized(btn_text, 1., 1.) {
                                        command.kind = CommandKind::Despawn;
                                        command.thing = msg_id;
                                    }
                                });
                            } else {
                                gui.label("");
                            }
                        }
                    });
                }
            },
        )
    }
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
