mod assets;
mod board;
mod csv;
mod terrain;
mod things;

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
    board.set_camera(mq::vec2(600., 500.), 20.);
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
        things.readonly_pass(|_, this| {
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

#[derive(Clone, Copy, Default)]
struct V2 {
    x: f32,
    y: f32,
}

impl V2 {
    fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    fn magnitude(self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

impl std::ops::Add for V2 {
    type Output = V2;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl std::ops::Sub for V2 {
    type Output = V2;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl std::ops::Mul<f32> for V2 {
    type Output = V2;

    fn mul(self, rhs: f32) -> Self::Output {
        V2::new(self.x * rhs, self.y * rhs)
    }
}

fn setup(scratch: &Arena) -> Things {
    let mut ctx = Things::new();

    let csv = csv::parse_file(scratch, "data/init.csv");
    for row in csv.rows() {
        match row[0].as_str() {
            "spawn_settlement" => {
                let tag = row[1].as_str().to_string().leak();
                let this = ctx.spawn_with_tag(tag);
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
                this.set_flag(Flag::IsVisible, true);
            }
            "spawn_waypoint" => {
                let tag = row[1].as_str().to_string().leak();
                let this = ctx.spawn_with_tag(tag);
                this.set_sprite("way_5");
                this.body = Body {
                    x: row[2].as_num(),
                    y: row[3].as_num(),
                    size: 2,
                    layer: 0,
                };
                this.set_flag(Flag::IsLocation, true);
                this.set_flag(Flag::IsVisible, true);
            }
            "spawn_person" => {
                let tag = row[1].as_str().to_string().leak();
                let this = ctx.spawn_with_tag(tag);
                this.set_name(row[2].as_str().to_string().leak());
                this.set_sprite(row[3].as_str().to_string().leak());
                this.body = Body {
                    size: 2,
                    layer: 1,
                    ..Default::default()
                };
                this.set_flag(Flag::IsPerson, true);
                this.set_flag(Flag::IsVisible, true);
            }
            _ => {}
        }
    }

    for row in csv.rows() {
        match row[0].as_str() {
            "spawn_person" => {
                let my_id = ctx.lookup_tag(row[1].as_str());
                let location = ctx.lookup_tag(row[4].as_str());
                ctx.add_to_list(List::AtLocation, location, my_id);
                ctx[my_id].set_flag(Flag::Teleport, true);
            }
            "connect_path" => {
                let [a, b] = [1, 2].map(|i| ctx.lookup_tag(row[i].as_str()));
                let this = ctx.spawn();
                this.set_link(Link::A, a);
                this.set_link(Link::B, b);
                this.set_flag(Flag::IsPath, true);
                this.set_flag(Flag::IsVisible, true);
            }
            _ => {}
        }
    }

    ctx
}

fn tick(ctx: &mut Things) {
    ctx.write_pass(
        |_, _| true,
        |ctx, this, commands| {
            // Update body position for entities that are in a 'dependent' location
            if let Some(location) = this.parent(List::AtLocation).as_valid() {
                const MOVEMENT_SPEED: f32 = 2.0;
                let location = &ctx[location];
                // Find my position around the target
                let target = if this.flag(Flag::IsInside) {
                    V2::new(location.body.x, location.body.y)
                } else {
                    let idx = ctx
                        .iter_list(List::AtLocation, location.id())
                        .position(|x| this.id() == x)
                        .unwrap_or(0);
                    let len = location.list_len(List::AtLocation);
                    pos_around(location.body, idx, len)
                };

                // Caculate next immediate position
                let next_pos = if this.flag(Flag::Teleport) {
                    this.set_flag(Flag::Teleport, false);
                    target
                } else {
                    let current_pos = V2::new(this.body.x, this.body.y);
                    let dv = target - current_pos;
                    if dv.magnitude() < 0.1 {
                        target
                    } else {
                        current_pos + dv * mq::get_frame_time() * MOVEMENT_SPEED
                    }
                };

                // Update body
                this.body.x = next_pos.x;
                this.body.y = next_pos.y;

                // Temporary: travel to a new place!
                let new_location = ctx.lookup_tag("din_drust");
                commands.add_to_list(List::AtLocation, new_location, this.id());
            }
        },
    );
}

fn pos_around(body: Body, idx: usize, len: usize) -> V2 {
    // Find my position around the target
    let angle = std::f32::consts::TAU * (idx as f32 / len as f32);
    let radius = body.size as f32 * 0.75;
    let cx = body.x + angle.cos() * radius;
    let cy = body.y + angle.sin() * radius;
    V2::new(cx, cy)
}
