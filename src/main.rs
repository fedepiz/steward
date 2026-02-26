mod assets;
mod board;
mod things;

use std::marker::PhantomData;

use crate::{assets::*, things::*};
use board::*;
use gui::Gui;
use macroquad::prelude as mq;
use util::arena::Arena;
use util::geom::*;

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

    let mut sim = setup(&frame_arena);

    let mut board = Board::new(&frame_arena);
    board.set_camera(mq::vec2(600., 500.), 20.);
    let font = mq::load_ttf_font("assets/fonts/board.ttf").await.unwrap();

    let mut selected_id = ThingId::default();

    let sprite_atlas =
        load_texture_atlas(&eternal_arena, &frame_arena, "assets/atlas/out/pawns").await;

    let mut gui = Gui::default();
    let mut gui_renderer = board::GuiRenderer::new();

    loop {
        frame_arena.reset();
        if mq::is_key_pressed(mq::KeyCode::Escape) {
            return;
        }

        let gui_output = build_ui::root(&mut gui, &frame_arena, &sim.things, selected_id);
        if !gui_output.is_mouse_over_ui && mq::is_mouse_button_pressed(mq::MouseButton::Left) {
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
        render_things(&mut draw_data, &sim.things, selected_id);
        draw_data.prepare();

        // Actuall draw to screen
        mq::clear_background(mq::LIGHTGRAY);
        board.draw(&draw_data, &sprite_atlas, &font);
        gui_renderer.draw(gui_output.draw_list, &font);

        tick(&mut sim, &frame_arena);

        mq::next_frame().await;
    }
}

fn render_things(draw_data: &mut DrawData, things: &Things, selected_id: ThingId) {
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

struct Simulation {
    thick_num: u64,
    things: Things,
    nav_cache: NavCache,
}

struct NavCacheBuilder {
    graph: CsrBuilder<ThingId, ThingId>,
    cache_size: usize,
}

impl NavCacheBuilder {
    fn new(cache_size: usize) -> Self {
        Self {
            graph: CsrBuilder::new(),
            cache_size,
        }
    }

    fn add_connection(&mut self, a: ThingId, b: ThingId) {
        self.graph.push(a, b);
        self.graph.push(b, a);
    }

    fn build(self) -> NavCache {
        NavCache {
            graph: self.graph.build(),
            cache: Vec::with_capacity(self.cache_size),
            counters: NavCacheCounters::default(),
        }
    }
}

#[derive(Default, Clone, Copy)]
struct NavCacheEntry {
    source: ThingId,
    destination: ThingId,
    next_step: ThingId,
}

struct NavCache {
    graph: Csr<ThingId, ThingId>,
    cache: Vec<NavCacheEntry>,
    counters: NavCacheCounters,
}

#[derive(Default)]
struct NavCacheCounters {
    num_hits: u64,
    num_miss: u64,
    num_reset: u64,
}

impl NavCache {
    /// Returns the next hop from `source` toward `destination`.
    ///
    /// The cache stores `(source, destination) -> next_step` entries so repeated
    /// queries can skip pathfinding. On a cache miss, this runs A* over the CSR
    /// graph, caches each hop along the discovered path, and returns the first
    /// step after `source`.
    ///
    /// If no path is found (or `source == destination`), this returns a null
    /// `ThingId`.
    fn pathfind(
        &mut self,
        source: ThingId,
        destination: ThingId,
        cost_fn: &impl Fn(ThingId, ThingId) -> i32,
    ) -> ThingId {
        // Find an existing step, if one exists
        let entry = self
            .cache
            .iter()
            .find(|entry| entry.source == source && entry.destination == destination);

        // Return the next step if found
        match entry {
            Some(entry) => {
                self.counters.num_hits = self.counters.num_hits.saturating_add(1);
                return entry.next_step;
            }
            None => {}
        };

        // No step found, run pathfinding
        let pathfind_result = pathfinding::directed::astar::astar(
            &source,
            |&node| {
                self.graph
                    .get_slice(node)
                    .into_iter()
                    .map(move |&x| (x, cost_fn(node, x)))
            },
            |&node| cost_fn(node, destination),
            |&node| node == destination,
        );
        self.counters.num_miss = self.counters.num_miss.saturating_add(1);
        let path = pathfind_result
            .as_ref()
            .map(|x| x.0.as_slice())
            .unwrap_or_default();

        // Blow the cache if out of room
        if self.cache.len() + path.len() >= self.cache.capacity() {
            self.counters.num_reset = self.counters.num_reset.saturating_add(1);
            self.cache.clear();
        }

        // Cache the new steps
        self.cache
            .extend(path.windows(2).map(|steps| NavCacheEntry {
                source: steps[0],
                destination,
                next_step: steps[1],
            }));

        // And return the next one, if any
        path.get(1).copied().unwrap_or_default()
    }
}

fn setup(scratch: &Arena) -> Simulation {
    let mut things = Things::new();
    let ctx = &mut things;

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

    let mut nav_cache = NavCacheBuilder::new(1024);

    ctx.readonly_pass(|_, this| {
        if this.flag(Flag::IsPath) {
            let a = this.link(Link::A);
            let b = this.link(Link::B);
            nav_cache.add_connection(a, b);
        }
    });
    let nav_cache = nav_cache.build();

    Simulation {
        thick_num: 0,
        things,
        nav_cache,
    }
}

trait Slot {
    fn slot(&self) -> usize;
}

impl Slot for ThingId {
    fn slot(&self) -> usize {
        ThingId::slot(*self)
    }
}

struct CsrBuilder<K, V> {
    bins: Vec<usize>,
    entries: Vec<(K, V)>,
    max_slot: usize,
}

impl<K: Slot, V: Default + Clone> CsrBuilder<K, V> {
    /// Creates an empty CSR builder with fixed bin capacity for all thing slots.
    fn new() -> Self {
        Self {
            bins: vec![0; things::NUM_THINGS],
            entries: vec![],
            max_slot: 0,
        }
    }

    /// Appends one adjacency value under `key`.
    ///
    /// Internally this increments the per-slot bin count and records the entry
    /// for compaction during `build`.
    fn push(&mut self, key: K, value: V) {
        let slot = key.slot();
        self.bins[slot] += 1;
        self.entries.push((key, value));
        self.max_slot = slot.max(self.max_slot);
    }

    /// Compacts pushed entries into CSR layout.
    ///
    /// Produces prefix offsets per key slot and a contiguous value array where
    /// neighbors for a slot are stored in `values[offsets[i]..offsets[i + 1]]`.
    fn build(self) -> Csr<K, V> {
        let mut offsets = vec![0; self.max_slot + 2];
        // Prefix sum to calculate offsets
        for i in 0..=self.max_slot {
            offsets[i + 1] = offsets[i] + self.bins[i];
        }
        let mut counts = self.bins;
        counts.clear();
        counts.resize(self.max_slot + 1, 0);

        let mut values = vec![V::default(); self.entries.len()];

        for (key, value) in self.entries {
            let idx = offsets[key.slot()] + counts[key.slot()];
            counts[key.slot()] += 1;
            values[idx] = value
        }

        Csr {
            key_typ: PhantomData,
            offsets,
            values,
        }
    }
}

#[derive(Default)]
struct Csr<K, V> {
    key_typ: PhantomData<K>,
    offsets: Vec<usize>,
    values: Vec<V>,
}

impl<K: Slot, V> Csr<K, V> {
    /// Returns the adjacency slice for `id`.
    ///
    /// The returned slice is borrowed from internal CSR storage and is empty
    /// when the slot is out of range or has no entries.
    fn get_slice(&self, id: K) -> &[V] {
        let idx = id.slot();
        if idx + 1 >= self.offsets.len() {
            return &[];
        }
        let start = self.offsets[idx];
        let end = self.offsets[idx + 1];
        &self.values[start..end]
    }
}

fn tick(sim: &mut Simulation, scratch: &Arena) {
    sim.thick_num = sim.thick_num.wrapping_add(1);

    sim.things.write_pass(
        |_, _| true,
        |ctx, this, commands| {
            // As a test, send people to din drust
            if this.flag(Flag::IsPerson) && !this.flag(Flag::Test) {
                this.set_flag(Flag::Test, true);
                this.set_link(Link::Destination, ctx.lookup_tag("din_drust"));
                this.set_var(Var::MovementTime, 0.);
            }

            // If `this` has a destination, then it should try to move there (unless arrived)
            if let Some(destination) = this.link(Link::Destination).as_valid() {
                let current_location = this.parent(List::AtLocation);
                if current_location != destination {
                    let cost_fn = |x, y| {
                        let dist = (ctx[x].body.pos() - ctx[y].body.pos()).magnitude();
                        (dist * 25.).round().max(0.) as i32
                    };

                    if let Some(next_step) = sim
                        .nav_cache
                        .pathfind(current_location, destination, &cost_fn)
                        .as_valid()
                    {
                        let next_step_cost = cost_fn(current_location, next_step) as f32;
                        let mov_time = this.var(Var::MovementTime);
                        if mov_time >= next_step_cost {
                            this.set_var(Var::MovementTime, 0.);
                            commands.add_to_list(List::AtLocation, next_step, this.id());

                            // If we have arrived at the destination
                            if next_step == destination {
                                let message =
                                    commands.spawn_and_append_to_list(List::Messages, this.id());
                                message.set_name("#0 has arrived at #1");
                                message.set_link(Link::A, this.id());
                                message.set_link(Link::B, next_step);
                            }
                        } else {
                            this.set_var(Var::MovementTime, mov_time + 1.);
                        }
                    }
                }
            }

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
            }

            for message in ctx.iter_list(List::Messages, this.id()) {
                let message = &ctx[message];
                let params = &[
                    ctx[message.link(Link::A)].name(),
                    ctx[message.link(Link::B)].name(),
                ];
                let rendered = render_template_string(scratch, message.name(), params);
                println!("{rendered}");
                commands.despawn(message.id());
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

mod build_ui {
    use gui::*;
    use macroquad::prelude as mq;
    use util::{arena::Arena, geom::*};

    use crate::things::*;

    pub(super) fn root<'a>(
        gui: &'a mut Gui,
        arena: &'a Arena,
        things: &Things,
        selected_id: ThingId,
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

                if !selected_id.is_null() {
                    let this = &things[selected_id];
                    gui.panel(|mut gui| {
                        gui.inner().center_on_growth_axis(false);
                        gui.inner().screen_pos(V2::new(0., 0.5));
                        gui.heading("Selected Entity", 6.);

                        gui.label(gui.arena().fmt(format_args!("Name: {}", this.name())));
                    });
                }
            },
        )
    }
}
