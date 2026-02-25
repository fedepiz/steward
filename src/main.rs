mod assets;
mod board;
mod csv;
mod terrain;
mod things;

use std::collections::HashMap;

use crate::{assets::*, terrain::TerrainRenderer, things::*};
use board::*;
use macroquad::prelude as mq;
use util::arena::Arena;

fn main() {
    let config = mq::Conf {
        window_width: 1600,
        window_height: 900,
        ..Default::default()
    };
    macroquad::Window::from_config(config, amain());
}

fn setup(scratch: &Arena) -> Things {
    let mut ctx = Things::new();

    let csv = csv::parse_file(scratch, "data/init.csv");
    let mut tag_map = HashMap::new();
    for row in csv.rows() {
        match row[0].as_str() {
            "spawn_settlement" => {
                let this = ctx.spawn();
                tag_map.insert(row[1].as_str(), this.id());
                this.set_name(row[2].as_str().to_string().leak());
                this.set_sprite(row[3].as_str().to_string().leak());
                this.body = Body {
                    x: row[4].as_num(),
                    y: row[5].as_num(),
                    size: 4,
                    layer: 0,
                };
                this.set_flag(Flag::IsLocation, true);
                this.set_flag(Flag::IsSettlement, true);
            }
            "spawn_waypoint" => {
                let this = ctx.spawn();
                tag_map.insert(row[1].as_str(), this.id());
                this.set_sprite("way_5");
                this.body = Body {
                    x: row[2].as_num(),
                    y: row[3].as_num(),
                    size: 2,
                    layer: 0,
                };
                this.set_flag(Flag::IsLocation, true);
            }
            "spawn_person" => {
                let this = ctx.spawn();
                tag_map.insert(row[1].as_str(), this.id());
                this.set_name(row[2].as_str().to_string().leak());
                this.set_sprite(row[3].as_str().to_string().leak());
                this.body = Body {
                    size: 2,
                    layer: 1,
                    ..Default::default()
                }
            }
            _ => {}
        }
    }

    let find_id = |tag: &str| tag_map.get(tag).copied().unwrap_or_default();

    for row in csv.rows() {
        match row[0].as_str() {
            "spawn_person" => {
                let my_id = find_id(row[1].as_str());
                let location = find_id(row[4].as_str());
                ctx.add_to_list(List::AtLocation, location, my_id);
                ctx[my_id].set_flag(Flag::Teleport, true);
            }
            "connect_path" => {
                let this = ctx.spawn();
                let [a, b] = [1, 2].map(|i| find_id(row[i].as_str()));
                this.set_link(Link::A, a);
                this.set_link(Link::B, b);
                this.set_flag(Flag::IsPath, true);
            }
            _ => {}
        }
    }

    ctx
}

async fn amain() {
    // Arena that is never reset
    let eternal_arena = Arena::new();
    // Arena that is reset per frame
    let mut frame_arena = Arena::new();

    // Necessary on certain annoying platforms that do not want to obey my screen sizings...
    mq::request_new_screen_size(mq::screen_width(), mq::screen_height());
    mq::next_frame().await;

    let mut things = setup(&frame_arena);

    let mut board = Board::new();
    board.set_camera(mq::vec2(600., 500.), 8.);
    let board_font = mq::load_ttf_font("assets/fonts/board.ttf").await.unwrap();
    let terrain_renderer = TerrainRenderer::new(&eternal_arena);

    let mut selected_id = ThingId::default();

    let sprite_atlas =
        load_texture_atlas(&eternal_arena, &frame_arena, "assets/atlas/out/pawns").await;

    loop {
        frame_arena.reset();
        if mq::is_key_pressed(mq::KeyCode::Escape) {
            return;
        }

        if mq::is_mouse_button_pressed(mq::MouseButton::Left) {
            selected_id = board.hovered_id();
        }

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

        let mut draw_data = DrawData::new(&frame_arena);

        // "Render" entities
        things.pass_readonly(|_, this| {
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
                        draw_data.labels.push(Label {
                            text: name,
                            pos: xy + mq::vec2(size / 2., size),
                            font_size: 24,
                            color,
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
                    let a_pos = mq::vec2(things[a].body.x, things[a].body.y);
                    let b_pos = mq::vec2(things[b].body.x, things[b].body.y);
                    draw_data.paths.push(Path {
                        start: a_pos,
                        end: b_pos,
                    });
                }
            }
        });

        draw_data.prepare();

        // Actuall draw to screen
        mq::clear_background(mq::LIGHTGRAY);
        board.draw(&draw_data, &terrain_renderer, &sprite_atlas, &board_font);

        tick(&mut things);

        mq::next_frame().await;
    }
}

fn tick(ctx: &mut Things) {
    ctx.pass(
        |_, _| true,
        |ctx, this| {
            if let Some(location) = this.parent(List::AtLocation).as_valid() {
                let location = &ctx[location];
                // Find my position around the target
                let angle = {
                    let idx = ctx
                        .iter_list(List::AtLocation, location.id())
                        .position(|x| this.id() == x)
                        .unwrap_or(0);
                    let len = location.list_len(List::AtLocation);
                    std::f32::consts::TAU * (idx as f32 / len as f32)
                };

                let radius = location.body.size as f32 * 0.75;

                let cx = location.body.x + angle.cos() * radius;
                let cy = location.body.y + angle.sin() * radius;

                if this.flag(Flag::Teleport) {
                    this.body.x = cx;
                    this.body.y = cy;
                }
            }
        },
    );
}
