use std::marker::PhantomData;

use util::{
    arena::Arena,
    geom::{Rect, V2},
};

use crate::{
    draw::*,
    things::{self, *},
};

pub(crate) struct Simulation {
    pub thick_num: u64,
    pub things: Things,
    nav_cache: NavCache,
}

impl Simulation {
    const PLAYER_TAG: &'static str = "player";

    pub(crate) fn player(&self) -> ThingId {
        self.things.lookup_tag(Self::PLAYER_TAG)
    }
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

pub(crate) fn setup(scratch: &Arena) -> Simulation {
    let mut things = Things::new();
    let ctx = &mut things;

    // Create the player thing
    ctx.spawn_with_tag(Simulation::PLAYER_TAG);

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
                this.set_name("Waypoint");
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
                this.set_flag(Flag::IsParty, true);
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

#[inline]
fn index_in_list(ctx: &Things, list: List, this: &Thing) -> Option<(ThingId, usize)> {
    let parent = this.parent(list).as_valid()?;
    ctx.iter_list(list, parent)
        .position(|x| this.id() == x)
        .map(|x| (parent, x))
}

#[derive(Default)]
pub(crate) struct Request {
    // Delta time, for animations
    pub delta: f32,
    // Turns to simulate
    pub advance_time: usize,
    // Selection
    // - entity selected
    pub select_entity: ThingId,
    // Messages
    // - page to show
    pub message_page: usize,
    // - how big is a page
    pub messages_per_page: usize,
    // - message to focus on
    pub message_expended: ThingId,
    // Despawns
    pub despawns: Vec<ThingId>,
}

#[derive(Default, Clone, Copy)]
pub(crate) struct OrderType {
    pub name: &'static str,
    completion_message: &'static str,
    move_to_destination: bool,
    wants_to_be_inside: bool,
}

const ORDER_TYPES: [OrderType; 3] = [
    OrderType {
        name: "Nothing",
        completion_message: "THIS IS A DUMMMY ORDER TYPE",
        move_to_destination: false,
        wants_to_be_inside: false,
    },
    OrderType {
        name: "Move to #1",
        completion_message: "#0 has arrived as #1",
        move_to_destination: true,
        wants_to_be_inside: false,
    },
    OrderType {
        name: "Enter #1",
        completion_message: "#0 has entered #1",
        move_to_destination: true,
        wants_to_be_inside: true,
    },
];

#[inline]
fn get_order_type(order: &Thing) -> &OrderType {
    &ORDER_TYPES[order.handle(Handle::Type) as usize]
}

#[inline]
fn render_order_name<'a>(arena: &'a Arena, ctx: &Things, order: &Thing) -> &'a str {
    let params = &[
        ctx[order.link(Link::Owner)].name(),
        ctx[order.link(Link::Destination)].name(),
    ];
    render_template_string(arena, order.name(), params)
}

#[inline]
fn render_message<'a>(arena: &'a Arena, ctx: &Things, message: ThingId) -> &'a str {
    let this = &ctx[message];
    let params = &[
        ctx[this.link(Link::A)].name(),
        ctx[this.link(Link::B)].name(),
    ];
    render_template_string(arena, this.name(), params)
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

pub(crate) struct Response<'a> {
    pub selected_entity: ThingId,
    pub messages: MessagesInfo<'a>,
    pub order: OrderInfo<'a>,
    pub draw_data: DrawData<'a>,
}

impl<'a> Response<'a> {
    fn new(arena: &'a Arena) -> Self {
        Self {
            selected_entity: ThingId::null(),
            messages: MessagesInfo::default(),
            order: OrderInfo::default(),
            draw_data: DrawData::new(arena),
        }
    }
}

#[derive(Default)]
pub(crate) struct MessagesInfo<'a> {
    pub expanded: Option<(ThingId, &'a str)>,
    pub list: &'a [(ThingId, &'a str)],
    pub current_page: usize,
    pub number_of_pages: usize,
}

#[derive(Default)]
pub(crate) struct OrderInfo<'a> {
    pub name: &'a str,
}

pub(crate) fn tick<'a>(sim: &mut Simulation, req: Request, arena: &'a Arena) -> Response<'a> {
    let _span = tracing::info_span!("Tick").entered();

    for id in req.despawns {
        sim.things.despawn(id);
    }

    let player = sim.player();

    for _ in 0..req.advance_time {
        sim.thick_num = sim.thick_num.wrapping_add(1);

        if sim.thick_num == 1 {
            for _ in 0..20 {
                sim.things.with_commands(|_, commands| {
                send_message(commands, "This is a long, $sprite$soldier :3 long long message, with lots of wordy words, and $sprite$celtic_town  it will hopefully wrap around. In fact, it also\n include\n new lines, \ttabs n shit", &[], player);
            })
            }
        }

        let _span = tracing::info_span!("Advance-Step").entered();
        sim.things.write_pass(
            |_, _| true,
            |ctx, this, commands| {
                // Automatic destruction of dependent objects
                if this.flag(Flag::MustBeOwned) && !ctx.exists(this.link(Link::Owner)) {
                    commands.despawn(this.id());
                }

                if this.flag(Flag::IsParty) {
                    // As a test, order people to din drust
                    if !this.flag(Flag::Test) {
                        this.set_flag(Flag::Test, true);

                        add_order(this, 1, ctx.lookup_tag("llan_heledd"), commands);
                        add_order(this, 2, ctx.lookup_tag("din_drust"), commands);
                    }

                    // Order completion
                    check_order_completion(ctx, this, commands, player);

                    // Movement
                    update_movement_intention(ctx, this);
                    progress_travel(ctx, this, commands, &mut sim.nav_cache);
                    let movement_status = update_body_of_local_things(ctx, this, req.delta);
                    let has_arrived = matches!(movement_status, MovementStatus::Arrived);

                    // A person is invisible if it is not yet arrived at its destination, nor is inside
                    this.set_flag(Flag::IsVisible, !has_arrived || !this.flag(Flag::IsInside));
                }
            },
        );
    }

    let mut response = Response::new(arena);
    let ctx = &sim.things;

    let selected_entity = if ctx.exists(req.select_entity) {
        req.select_entity
    } else {
        ThingId::null()
    };
    response.selected_entity = selected_entity;

    ctx.readonly_pass(|ctx, this| {
        render_thing(ctx, this, &mut response.draw_data, response.selected_entity);
    });

    if req.messages_per_page > 0 {
        let num_messages = player.get(&ctx).list_len(List::Messages);
        let number_of_pages = (num_messages.saturating_sub(1) / req.messages_per_page) + 1;
        let current_page = req.message_page.clamp(1, number_of_pages);
        let to_skip = current_page.saturating_sub(1) * req.messages_per_page;

        let list = arena.alloc_slice_exact(
            ctx.iter_list(List::Messages, player)
                .skip(to_skip)
                .take(num_messages)
                .map(|msg| (msg, render_message(arena, &ctx, msg))),
        );
        list.reverse();
        response.messages.list = list;
        response.messages.current_page = current_page;
        response.messages.number_of_pages = number_of_pages;
    };

    response.messages.expanded = req
        .message_expended
        .as_valid()
        .map(|id| (id, render_message(arena, &ctx, id)));

    {
        let selected_entity = selected_entity.get(ctx);
        let order = selected_entity.head(List::Orders).get_as_valid(ctx);
        response.order.name = order
            .map(|order| render_order_name(arena, &ctx, order))
            .unwrap_or("No order");
    }

    response
}

fn add_order(this: &mut Thing, order_type: u16, destination: ThingId, commands: &mut Commands) {
    let order = commands.spawn_and_append_to_list(List::Orders, this.id());
    order.set_handle(Handle::Type, order_type);
    let order_type = get_order_type(order);
    order.set_name(order_type.name);
    order.set_flag(Flag::IsOrder, true);
    order.set_link(Link::Destination, destination);
    assign_ownership(order, this.id());
}

fn check_order_completion(
    ctx: &Things,
    this: &mut Thing,
    commands: &mut Commands,
    player: ThingId,
) {
    if let Some(order) = this.head(List::Orders).get_as_valid(ctx) {
        let order_type = get_order_type(order);
        let location = this.parent(List::AtLocation);
        let arrived = location == order.link(Link::Destination);
        if arrived {
            // Order completed
            commands.despawn(order.id());
            commands.remove_from_list(List::Orders, order.id());
            // Send a message
            send_message(
                commands,
                order_type.completion_message,
                &[this.id(), location],
                player,
            );
        }
    }
}

fn update_movement_intention(ctx: &Things, this: &mut Thing) {
    // Take the current destination, and the destination from other sources
    let current_destination = this.link(Link::Destination);

    if let Some(order) = this.head(List::Orders).get_as_valid(ctx) {
        let order_type = get_order_type(order);

        let ordered_destination = if order_type.move_to_destination {
            order.link(Link::Destination)
        } else {
            current_destination
        };

        // If the current destination is different then the ordered one, we should
        // change our destination and reset the movmement timer, and go 'outside'
        if current_destination != ordered_destination {
            this.set_link(Link::Destination, ordered_destination);
            this.set_var(Var::MovementTime, 0.);
        }

        this.set_flag(Flag::WantsToBeInside, order_type.wants_to_be_inside);
    }
}

fn progress_travel(
    ctx: &Things,
    this: &mut Thing,
    commands: &mut Commands,
    nav_cache: &mut NavCache,
) {
    let current_location = this.parent(List::AtLocation);
    // Only makes sense if we have a destination
    let destination = this.link(Link::Destination);
    if let Some(destination) = destination.as_valid() {
        // If we are not yet arrived
        if current_location != destination {
            // The cost of moving between two edges
            let cost_fn = |x, y| {
                let dist = (ctx[x].body.pos() - ctx[y].body.pos()).magnitude();
                (dist * 25.).round().max(0.) as i32
            };

            // Resolve navigation. This should always work, to be honest...
            if let Some(next_step) = nav_cache
                .pathfind(current_location, destination, &cost_fn)
                .as_valid()
            {
                // Now we know where to go next, let's see if we are there yet
                let next_step_cost = cost_fn(current_location, next_step) as f32;
                // We have moved...this much (it would be reset if we changed destination)
                let mov_time = this.var(Var::MovementTime);
                if mov_time >= next_step_cost {
                    // We moved enough! Reset movement time, transfer location
                    this.set_var(Var::MovementTime, 0.);
                    commands.add_to_list(List::AtLocation, next_step, this.id());
                } else {
                    // Otherwise, just step up the movement time
                    this.set_var(Var::MovementTime, mov_time + 1.);
                }
            }
        }
    }

    let wants_to_be_inside = this.flag(Flag::WantsToBeInside);
    let is_inside =
        wants_to_be_inside && (destination.is_null() || current_location == destination);
    this.set_flag(Flag::IsInside, is_inside);
}

enum MovementStatus {
    Arrived,
    Moving,
}

fn update_body_of_local_things(ctx: &Things, this: &mut Thing, delta: f32) -> MovementStatus {
    if let Some((location, idx)) = index_in_list(ctx, List::AtLocation, this) {
        const MOVEMENT_SPEED: f32 = 2.0;
        let location = &ctx[location];
        // Find my position around the target
        let target = if this.flag(Flag::IsInside) {
            V2::new(location.body.x, location.body.y)
        } else {
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
                current_pos + dv * delta * MOVEMENT_SPEED
            }
        };

        // Update body
        this.body.x = next_pos.x;
        this.body.y = next_pos.y;

        if next_pos == target {
            MovementStatus::Arrived
        } else {
            MovementStatus::Moving
        }
    } else {
        MovementStatus::Moving
    }
}

fn pos_around(body: Body, idx: usize, len: usize) -> V2 {
    // Find my position around the target
    let angle = std::f32::consts::TAU * (idx as f32 / len as f32);
    let radius = body.size as f32 * 0.75;
    let cx = body.x + angle.cos() * radius;
    let cy = body.y + angle.sin() * radius;
    V2::new(cx, cy)
}

fn send_message(
    commands: &mut Commands,
    text: &'static str,
    params: &[ThingId],
    recepient: ThingId,
) {
    const LINKS: [Link; 2] = [Link::A, Link::B];
    assert!(params.len() <= LINKS.len());

    let message = commands.spawn_and_append_to_list(List::Messages, recepient);
    message.set_name(text);
    for (&link, &param) in LINKS.iter().zip(params) {
        message.set_link(link, param);
    }
}

fn assign_ownership(this: &mut Thing, owner: ThingId) {
    this.set_flag(Flag::MustBeOwned, true);
    this.set_link(Link::Owner, owner);
}

fn render_thing(ctx: &Things, this: &Thing, draw_data: &mut DrawData, selected_id: ThingId) {
    if !this.flag(Flag::IsVisible) {
        return;
    }
    if this.body.size > 0 && !this.sprite().is_empty() {
        let is_selected = this.id() == selected_id;

        let size = this.body.size as f32;
        let xy = V2::new(this.body.x, this.body.y) - size / 2.;
        let bounds = Rect::new(xy.x, xy.y, size, size);

        let sprite = Sprite {
            image: this.sprite(),
            bounds,
            layer: this.body.layer,
            border_highlight: is_selected,
            pulse_intensity: if is_selected { 1.0 } else { 0.0 },
        };
        draw_data.sprites.push(sprite);

        let show_name = is_selected || this.flag(Flag::IsSettlement);
        if show_name {
            let name = this.name();
            if !name.is_empty() {
                let layer = this.body.layer.max(if is_selected { 3 } else { 0 });
                draw_data.labels.push(Label {
                    text: name,
                    pos: xy + V2::new(size / 2., size),
                    font_size: 24,
                    highighted: is_selected,
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
            let a_pos = V2::new(ctx[a].body.x, ctx[a].body.y);
            let b_pos = V2::new(ctx[b].body.x, ctx[b].body.y);
            draw_data.paths.push(Path {
                start: a_pos,
                end: b_pos,
            });
        }
    }
}
