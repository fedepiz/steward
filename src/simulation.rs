use util::{
    arena::Arena,
    geom::{Rect, V2},
};

use crate::{
    draw::*,
    navigation::{NavCache, NavCacheBuilder},
    things::*,
};

// Abstract scale to the travel cost
const TRAVEL_COST_SCALE: f32 = 10.;
// Movement lerp speed for bodies
const MOVEMENT_LERP_SPEED: f32 = 2.0;

pub(crate) struct Simulation {
    pub thick_num: u64,
    pub things: Things,
    nav_cache: NavCache,
}

const NAMES: &'static [&'static str] = &[
    "Aneirin",
    "Cadfan",
    "Ceneu",
    "Clydno",
    "Cynfelyn",
    "Cynon",
    "Deroch",
    "Dumnagual",
    "Dyfnwal",
    "Eliffer",
    "Eugein",
    "Gwallog",
    "Gwenddoleu",
    "Iddon",
    "Llywarch",
    "Mabon",
    "Madog",
    "Morcant",
    "Myrddin",
    "Nudd",
    "Nwython",
    "Owain",
    "Pabo",
    "Peredur",
    "Rhun",
    "Rhydderch",
    "Selyf",
    "Talorc",
    "Teneu",
    "Tutagual",
];

pub(crate) struct TokenType {
    pub sprite: &'static str,
}

mod token_types {
    pub const KINSHIP: u16 = 1;
    pub const DREAD: u16 = 2;
    pub const GIFT: u16 = 3;
    pub const ENCUMBENT: u16 = 4;
}

const TOKEN_TYPES: [TokenType; 5] = [
    TokenType {
        sprite: "tok_generic",
    },
    TokenType {
        sprite: "tok_kinship",
    },
    TokenType {
        sprite: "tok_dread",
    },
    TokenType { sprite: "tok_gift" },
    TokenType {
        sprite: "tok_encumbent",
    },
];

const PLAYER_TAG: &'static str = "player";
const COMMS_TAG: &'static str = "communications";

pub(crate) fn setup(scratch: &Arena) -> Simulation {
    let mut things = Things::new();
    let ctx = &mut things;

    // Create the 'system' items
    ctx.spawn_with_tag(PLAYER_TAG);
    ctx.spawn_with_tag(COMMS_TAG);

    let csv = csv::parse_file(scratch, "data/init.csv");
    for row in csv.rows() {
        match row[0].as_str() {
            "spawn_settlement" => {
                // let kind = row[1].as_str();
                let tag = row[2].as_str().to_string().leak();
                let this = ctx.spawn_with_tag(tag);
                this.name = row[3].as_str().to_string().leak();
                this.sprite = row[4].as_str().to_string().leak();
                this.body = Body {
                    x: row[5].as_num(),
                    y: row[6].as_num(),
                    size: 4,
                    layer: 1,
                };
                this.set_flag(Flag::IsLocation, true);
                this.set_flag(Flag::IsSettlement, true);
                let settlement = this.id();

                // Populate the settlement with 10 tokens
                for _ in 0..10 {
                    create_token_at(ctx, settlement);
                }
            }
            "spawn_waypoint" => {
                let tag = row[1].as_str().to_string().leak();
                let this = ctx.spawn_with_tag(tag);
                this.name = "Waypoint";
                this.sprite = "way_5";
                this.body = Body {
                    x: row[2].as_num(),
                    y: row[3].as_num(),
                    size: 2,
                    layer: 0,
                };
                this.set_flag(Flag::IsLocation, true);
            }
            _ => {}
        }
    }

    for row in csv.rows() {
        match row[0].as_str() {
            "spawn_person" => {
                let tag = row[1].as_str().to_string().leak();
                let name = row[2].as_str().to_string().leak();
                let sprite = row[3].as_str().to_string().leak();
                let location = ctx.lookup_tag(row[4].as_str());

                let person = create_person(ctx, location, false);
                ctx.set_tag(tag, person);
                ctx[person].name = name;
                ctx[person].sprite = sprite;
            }
            "connect_path" => {
                let [a, b] = [1, 2].map(|i| ctx.lookup_tag(row[i].as_str()));
                let this = ctx.spawn();
                this.edge_from = a;
                this.edge_to = b;
                this.set_flag(Flag::IsPath, true);
            }
            _ => {}
        }
    }

    ctx.exclusive_pass(|ctx, this| {
        // Populate settlements
        if this.flag(Flag::IsSettlement) {
            let settlement = &this;
            // Get the list of tokens here
            let mut tokens = scratch
                .alloc_slice_iter(ctx.iter_list(List::TokensSourced, settlement.id()))
                .into_iter();

            let num_people = 3;
            let mut people = scratch.new_vec_with_capacity(num_people);

            for i in 0..num_people {
                let person = create_person(ctx, settlement.id(), true);
                people.push(person);

                let kinds: &[u16] = if i == 0 {
                    &[token_types::KINSHIP, token_types::ENCUMBENT]
                } else {
                    &[token_types::KINSHIP]
                };
                for &kind in kinds {
                    if let Some(token) = tokens.next().copied() {
                        ctx.remove_from_list(List::TokensHeld, token);
                        ctx.add_to_list(List::TokensHeld, person, token);
                        let token = &mut ctx[token];
                        token.kind = kind;
                    }
                }
            }

            // Strongest person is leader
            let strongest_person = people
                .iter()
                .copied()
                .max_by_key(|&person| calculate_tokens_at(ctx, person, settlement.id()))
                .unwrap_or_default();

            ctx.add_to_list(List::Possessions, strongest_person, settlement.id());
        }
    });

    // Use the "set_loyalty" commands
    let mut effects = Effects::new();
    for row in csv.rows() {
        match row[0].as_str() {
            "set_loyalty" => {
                let subordinate = ctx
                    .lookup_tag(row[1].as_str())
                    .get(&ctx)
                    .parent(List::Possessions);
                let superior = ctx
                    .lookup_tag(row[2].as_str())
                    .get(&ctx)
                    .parent(List::Possessions);
                ctx.add_to_list(List::Subordinates, superior, subordinate);
                effects.transfer_tokens(subordinate, superior, token_types::GIFT);
            }
            _ => {}
        }
    }
    effects.perform(ctx, scratch);

    // End-of-setup pass
    let mut nav_cache = NavCacheBuilder::new(1024);

    ctx.write_pass(|_, this, _| {
        if this.flag(Flag::IsPerson) {
            // Determine persons' sprite
            let has_subordinates = this.list_len(List::Subordinates) > 0;
            this.sprite = if has_subordinates { "noble" } else { "soldier" };
        }

        if this.flag(Flag::IsPath) {
            nav_cache.add_connection(this.edge_from, this.edge_to);
        }
    });

    let nav_cache = nav_cache.build();

    Simulation {
        thick_num: 0,
        things,
        nav_cache,
    }
}

fn create_person(ctx: &mut Things, location: ThingId, inside: bool) -> ThingId {
    let person = ctx.spawn();
    let name = {
        let idx = location.slot() * 13 + person.id().slot() * 17;
        NAMES[idx % NAMES.len()]
    };
    person.name = name;
    person.sprite = "soldier";
    person.set_flag(Flag::IsPerson, true);
    person.body = Body {
        size: 2,
        layer: 1,
        ..Default::default()
    };
    // Positioning
    person.set_flag(Flag::WantsToBeInside, inside);
    person.set_flag(Flag::IsInside, inside);
    person.set_flag(Flag::Teleport, true);

    let person = person.id();
    ctx.add_to_list(List::AtLocation, location, person);

    for _ in 0..5 {
        create_token_at(ctx, person);
    }

    person
}

fn create_token_at(ctx: &mut Things, source: ThingId) -> ThingId {
    let token = ctx.spawn();
    token.name = "Token";
    token.set_flag(Flag::IsToken, true);
    token.kind = 0;
    token.owner = source;

    let token = token.id();
    ctx.add_to_list(List::TokensSourced, source, token);
    token
}

fn start_activity(ctx: &Things, location: ThingId, commands: &mut Commands) -> ThingRef {
    let pos = ctx[location].body.pos();

    let (activity_ref, activity) = commands.spawn();
    activity.name = "Test activity";
    activity.sprite = "activity_assembly";
    activity.set_flag(Flag::IsActivity, true);
    activity.body = Body {
        x: pos.x,
        y: pos.y,
        size: 2,
        layer: 3,
    };
    commands.add_to_list(List::AtLocation, location, activity_ref);
    activity_ref
}

fn calculate_tokens_at(ctx: &Things, holder: ThingId, source: ThingId) -> usize {
    ctx.iter_list_get(List::TokensHeld, holder)
        .filter(|tok| tok.parent(List::TokensSourced) == source)
        .count()
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
    pub messages: MessageRequest,
    // Despawns
    pub despawns: Vec<ThingId>,
    // Communication
    // - pick communication type
    pub communication: CommunicationRequest,
}

#[derive(Default)]
pub(crate) struct MessageRequest {
    // - page to show
    pub current_page: usize,
    // - how big is a page
    pub page_size: usize,
    // - message to focus on
    pub expanded: ThingId,
    // - remove all the messages
    pub delete_all: bool,
}

#[derive(Default)]
pub(crate) struct CommunicationRequest {
    // The list of requested communication pieces that are confirmed
    pub enqueued_pieces: Vec<CommPieceRequest>,
    // Information about the currently open communication piece
    pub selected_option: Option<usize>,
    pub target: ThingId,
    // Please send
    pub send: bool,
}

pub(crate) struct CommPieceRequest {
    pub type_idx: usize,
    pub target: ThingId,
}

#[derive(Default, Clone, Copy)]
pub(crate) struct OrderType {
    pub name: &'static str,
    completion_message: &'static str,
    move_to_destination: bool,
    wants_to_be_inside: bool,
    wait_time: f32,
    trigger_activity: bool,
}

mod order_types {
    use crate::simulation::OrderType;

    pub const MOVE: OrderType = OrderType {
        name: "Move to #1",
        completion_message: "#0 has arrived to #1",
        move_to_destination: true,
        wants_to_be_inside: false,
        wait_time: 0.,
        trigger_activity: false,
    };
    pub const ENTER: OrderType = OrderType {
        name: "Enter #1",
        completion_message: "#0 has entered #1",
        move_to_destination: true,
        wants_to_be_inside: true,
        wait_time: 0.,
        trigger_activity: false,
    };
    pub const CLAIM_KINSHIP: OrderType = OrderType {
        name: "Claim Kinship at #0",
        completion_message: "#0 has arrived to #1",
        move_to_destination: true,
        wants_to_be_inside: false,
        wait_time: 200.,
        trigger_activity: true,
    };
}

const ORDER_TYPES: [OrderType; 5] = [
    OrderType {
        name: "Nothing",
        completion_message: "THIS IS A DUMMMY ORDER TYPE",
        move_to_destination: false,
        wants_to_be_inside: false,
        wait_time: 0.,
        trigger_activity: false,
    },
    order_types::MOVE,
    order_types::ENTER,
    OrderType {
        name: "Wait",
        completion_message: "#0 has entered #1",
        move_to_destination: false,
        wants_to_be_inside: false,
        wait_time: 1000.,
        trigger_activity: false,
    },
    order_types::CLAIM_KINSHIP,
];

#[inline]
fn get_order_type<'a>(order: &Thing) -> &'a OrderType {
    &ORDER_TYPES[order.kind as usize]
}

#[inline]
fn render_order_name<'a>(arena: &'a Arena, ctx: &Things, order: &Thing) -> &'a str {
    let params = &[ctx[order.owner].name, ctx[order.destination].name];
    render_template_string(arena, order.name, params)
}

#[inline]
fn render_message<'a>(arena: &'a Arena, ctx: &Things, message: ThingId) -> &'a str {
    let this = &ctx[message];
    let params = this.params.map(|x| ctx[x].name);
    render_template_string(arena, this.name, &params)
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

#[derive(Default, Clone, Copy)]
pub(crate) struct CommunicationType {
    short_name: &'static str,
    long_name: &'static str,
    target_type: TargetType,
    order: Option<&'static OrderType>,
}

#[derive(Clone, Copy, Default)]
struct TargetType {
    name: &'static str,
    flag: Flag,
}

impl TargetType {
    fn check(&self, this: &Thing) -> bool {
        this.flag(self.flag)
    }
}

const COMMUNICATION_TYPES: [CommunicationType; 3] = [
    CommunicationType {
        short_name: "Move",
        long_name: "Move to #0",
        target_type: TargetType {
            name: "location",
            flag: Flag::IsLocation,
        },
        order: Some(&order_types::MOVE),
    },
    CommunicationType {
        short_name: "Enter",
        long_name: "Enter #0",
        target_type: TargetType {
            name: "settlement",
            flag: Flag::IsSettlement,
        },
        order: Some(&order_types::ENTER),
    },
    CommunicationType {
        short_name: "Clm Kin.",
        long_name: "Claim the right of kinship at #0",
        target_type: TargetType {
            name: "settlement",
            flag: Flag::IsSettlement,
        },
        order: Some(&order_types::CLAIM_KINSHIP),
    },
];

pub(crate) struct Response<'a> {
    pub messages: MessagesInfo<'a>,
    pub order: OrderInfo<'a>,
    pub communication: CommunicationInfo<'a>,
    pub selected_entity: EntityInfo<'a>,
    pub draw_data: DrawData<'a>,
}

impl<'a> Response<'a> {
    fn new(arena: &'a Arena) -> Self {
        Self {
            messages: MessagesInfo::default(),
            order: OrderInfo::default(),
            selected_entity: EntityInfo::default(),
            communication: CommunicationInfo::default(),
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

pub(crate) struct CommPieceInfo<'a> {
    pub type_idx: usize,
    pub target: ThingId,
    pub name: &'a str,
}

#[derive(Default)]
pub(crate) struct CommunicationInfo<'a> {
    // List of option-buttons to pick form
    pub options: &'a [(usize, &'a str)],
    // Enqueued options
    pub enqueued_pieces: &'a [CommPieceInfo<'a>],
    // Text and type index of selected option
    pub selected_option: Option<(usize, &'a str)>,
    // The target that has been picked
    pub target: ThingId,
    // Pick a target please!
    pub pick_target: bool,
    // Are we ready to...
    // - send the whole thing
    pub ready_to_send: bool,
    // Did we just send
    pub just_sent: bool,
}

#[derive(Default)]
pub(crate) struct EntityInfo<'a> {
    pub id: ThingId,
    pub name: &'a str,
    pub sprite: &'a str,
    pub influence: Vec<TokenHolderInfo<'a>>,
    pub show_partecipants: bool,
    pub partecipants: Vec<(ThingId, &'static str)>,
}

pub(crate) struct TokenHolderInfo<'a> {
    pub id: ThingId,
    pub name: &'a str,
    pub tokens: TokenCount,
}

pub(crate) fn tick<'a>(sim: &mut Simulation, request: Request, arena: &'a Arena) -> Response<'a> {
    let _span = tracing::info_span!("Tick").entered();

    for id in request.despawns {
        sim.things.despawn(id);
    }

    let player = sim.things.lookup_tag(PLAYER_TAG);

    sim.things.with_commands(|ctx, commands| {
        if request.messages.delete_all {
            for id in ctx.iter_list(List::Messages, player) {
                commands.despawn(id);
            }
        }

        // Grab the selected entity mutably
        if let Some(this) = request.select_entity.get_as_valid_mut(ctx) {
            // We are sending a communication
            if request.communication.send {
                for piece in &request.communication.enqueued_pieces {
                    let typ = &COMMUNICATION_TYPES[piece.type_idx];
                    if let Some(order_type) = typ.order {
                        add_order(this, order_type, piece.target, commands);
                    }
                }
            }
        }
    });

    let mut response = Response::new(arena);

    let mut effects = Effects::new();

    for _ in 0..request.advance_time {
        sim.thick_num = sim.thick_num.wrapping_add(1);

        let _span = tracing::info_span!("Advance-Step").entered();
        sim.things.write_pass(|ctx, this, commands| {
            // Automatic destruction of dependent objects
            if !this.owner.is_null() && !ctx.exists(this.owner) {
                commands.despawn(this.id());
            }

            if this.flag(Flag::IsActivity) {
                let location = this.parent(List::AtLocation);

                let mut num_valid_partecipants = 0;
                // Eject partecipants that are no longer at this location
                for target in ctx.iter_list(List::Partecipants, this.id()) {
                    let target = &ctx[target];
                    let left_or_leaving = target.parent(List::AtLocation) != location
                        || target.destination != location;
                    if left_or_leaving {
                        commands.remove_from_list(List::Partecipants, target.id());
                    } else {
                        num_valid_partecipants += 1;
                    }
                }

                let is_complete = this.wait_time > 1000.;
                let is_over = is_complete || num_valid_partecipants == 0;

                // End of activity
                if is_over {
                    commands.despawn(this.id());
                }

                if is_complete {
                    // Enqueue some token transfers
                    let initiator = this.first(List::Partecipants);
                    let location = this.parent(List::AtLocation);
                    effects.transfer_tokens(location, initiator, token_types::DREAD);
                } else {
                    this.wait_time += 1.;
                }
            }

            if this.flag(Flag::IsPerson) {
                // Order completion
                check_order_completion(ctx, this, commands, player);

                // Movement
                update_intentions(ctx, this);
                progress_travel(ctx, this, commands, &mut sim.nav_cache);
                let movement_status = update_body_of_local_things(ctx, this, request.delta);
                let has_arrived = matches!(movement_status, MovementStatus::Arrived);

                // A person is invisible if it is not yet arrived at its destination, nor is inside
                this.set_flag(Flag::IsInvisible, has_arrived && this.flag(Flag::IsInside));
            }
        });
    }

    effects.perform(&mut sim.things, arena);

    let ctx = &sim.things;

    // Message overview
    if request.messages.page_size > 0 {
        let num_messages = player.get(&ctx).list_len(List::Messages);
        let number_of_pages = (num_messages.saturating_sub(1) / request.messages.page_size) + 1;
        let current_page = request.messages.current_page.clamp(1, number_of_pages);
        let to_skip = current_page.saturating_sub(1) * request.messages.page_size;

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

    // Expanded message
    response.messages.expanded = request
        .messages
        .expanded
        .as_valid()
        .map(|id| (id, render_message(arena, &ctx, id)));

    // Order
    {
        let selected_entity = request.select_entity.get(ctx);
        let order = selected_entity.first(List::Orders).get_as_valid(ctx);
        response.order.name = order
            .map(|order| render_order_name(arena, &ctx, order))
            .unwrap_or("No order");
    }

    // Communications
    let req = &request.communication;
    let can_enqueue = req.selected_option.is_some() && req.target.is_valid();
    if !req.send {
        let info = &mut response.communication;

        let new_piece = if can_enqueue {
            let target = req.target;
            req.selected_option
                .map(|type_idx| CommPieceRequest { type_idx, target })
        } else {
            None
        };

        info.enqueued_pieces = {
            let iter = req.enqueued_pieces.iter().chain(&new_piece).map(|piece| {
                let typ = &COMMUNICATION_TYPES[piece.type_idx];
                let target_name = piece.target.get(ctx).name;
                let template = arena.fmt(format_args!("$sprite$plus {}", typ.long_name));
                let name = render_template_string(arena, template, &[target_name]);
                CommPieceInfo {
                    type_idx: piece.type_idx,
                    target: piece.target,
                    name,
                }
            });
            let mut vec = arena.new_vec_with_capacity(req.enqueued_pieces.len() + 1);
            vec.extend(iter);
            vec.into_bump_slice()
        };

        info.options = arena.alloc_slice_exact(
            COMMUNICATION_TYPES
                .iter()
                .enumerate()
                .map(|(idx, typ)| (idx, typ.short_name)),
        );

        // If we have not enqueued the message, we carry over the data as it is
        if !can_enqueue {
            if let Some(idx) = req.selected_option {
                let typ = &COMMUNICATION_TYPES[idx];

                let target = if typ.target_type.check(req.target.get(ctx)) {
                    req.target
                } else {
                    ThingId::null()
                };

                let target_param = target
                    .get_as_valid(ctx)
                    .map(|x| x.name)
                    .unwrap_or(typ.target_type.name);

                let name = render_template_string(arena, typ.long_name, &[target_param]);
                info.selected_option = Some((idx, name));
                info.target = target;
            }
        }

        info.pick_target = info.selected_option.is_some() && info.target.is_null();
        info.ready_to_send = !info.enqueued_pieces.is_empty();
    }
    response.communication.just_sent = request.communication.send;

    // Big pass, including renderings
    {
        let _span = tracing::info_span!("Present pass").entered();
        // Determine the target type for highlighting (if any)
        let target_type = if !response.communication.pick_target {
            Default::default()
        } else {
            response
                .communication
                .selected_option
                .map(|x| COMMUNICATION_TYPES[x.0].target_type)
                .unwrap_or_default()
        };

        // PRESENT PASS
        ctx.readonly_pass(|ctx, this| {
            // Extract selected entity information
            if this.id() == request.select_entity {
                let info = &mut response.selected_entity;
                info.id = this.id();
                info.name = this.name;
                info.sprite = this.sprite;

                // Populate selected entity influence
                let tokens = &mut info.influence;
                for token in ctx.iter_list_get(List::TokensSourced, this.id()) {
                    let count = match tokens
                        .iter()
                        .position(|holder| holder.id == token.parent(List::TokensHeld))
                    {
                        Some(idx) => &mut tokens[idx],
                        None => {
                            let holder = token.parent(List::TokensHeld).get(ctx);
                            let entry = TokenHolderInfo {
                                id: holder.id(),
                                name: holder.name,
                                tokens: TokenCount::default(),
                            };
                            tokens.push(entry);
                            tokens.last_mut().unwrap()
                        }
                    };
                    count.tokens.0[token.kind as usize] += 1;
                }
                // Sort the entries. First the unclaimed one (null id),
                // thereafter the remaining in order of count.
                tokens.sort_by_key(|x| {
                    if x.id.is_null() {
                        0
                    } else {
                        1000 - x.tokens.total() + 1
                    }
                });

                // Populate selected entity paretcipants
                if this.flag(Flag::IsActivity) {
                    info.show_partecipants = true;
                    info.partecipants = ctx
                        .iter_list(List::Partecipants, this.id())
                        .map(|partecipant| {
                            let name = ctx[partecipant].name;
                            (partecipant, name)
                        })
                        .collect();
                }
            }

            render_thing(
                ctx,
                this,
                &mut response.draw_data,
                request.select_entity,
                target_type,
            );
        });
    }

    response
}

fn add_order(this: &mut Thing, typ: &OrderType, destination: ThingId, commands: &mut Commands) {
    // let order = commands.spawn_and_append_to_list(List::Orders, this.id());
    let (order_ref, order) = commands.spawn();

    let order_type_idx = ORDER_TYPES.iter().position(|x| x.name == typ.name).unwrap() as u16;
    order.kind = order_type_idx;

    let order_type = get_order_type(order);
    order.name = order_type.name;
    order.set_flag(Flag::IsOrder, true);
    order.destination = destination;
    order.wait_time = order_type.wait_time;
    order.owner = this.id();

    commands.add_to_list(List::Orders, this.id(), order_ref);
}

fn check_order_completion(
    ctx: &Things,
    this: &mut Thing,
    commands: &mut Commands,
    player: ThingId,
) {
    if let Some(order) = this.first(List::Orders).get_as_valid(ctx) {
        let order_type = get_order_type(order);
        let location = this.parent(List::AtLocation);
        let arrived = location == order.destination;
        let waited_sufficiently = this.wait_time >= order.wait_time;
        if arrived && waited_sufficiently {
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

            this.current_order = ThingId::null();

            if order_type.trigger_activity {
                let activity = start_activity(ctx, location, commands);
                commands.add_to_list(List::Partecipants, activity, this.id());
            }
        }
    }
}

fn update_intentions(ctx: &Things, this: &mut Thing) {
    // Take the current destination, and the destination from other sources
    let current_destination = this.destination;

    if let Some(order) = this.first(List::Orders).get_as_valid(ctx) {
        let order_type = get_order_type(order);

        // This is a new order! Reset stuff like wait time etc
        if this.current_order != order.id() {
            this.current_order = order.id();
            this.wait_time = 0.;
        }

        let ordered_destination = if order_type.move_to_destination {
            order.destination
        } else {
            current_destination
        };

        // If the current destination is different then the ordered one, we should
        // change our destination and reset the movmement timer, and go 'outside'
        if current_destination != ordered_destination {
            this.destination = ordered_destination;
            this.movement_time = 0.;
        }

        let current_location = this.parent(List::AtLocation);
        // If we are at ordered destination, so wait timer should increase
        if current_location == ordered_destination {
            this.wait_time += 1.;
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
    if let Some(destination) = this.destination.as_valid() {
        // If we are not yet arrived
        if current_location != destination {
            // The cost of moving between two edges
            let cost_fn = |x, y| {
                let dist = (ctx[x].body.pos() - ctx[y].body.pos()).magnitude();
                (dist * TRAVEL_COST_SCALE).round().max(0.) as i32
            };

            // Resolve navigation. This should always work, to be honest...
            if let Some(next_step) = nav_cache
                .pathfind(current_location, destination, &cost_fn)
                .as_valid()
            {
                // Now we know where to go next, let's see if we are there yet
                let next_step_cost = cost_fn(current_location, next_step) as f32;
                // We have moved...this much (it would be reset if we changed destination)
                if this.movement_time >= next_step_cost {
                    // We moved enough! Reset movement time, transfer location
                    this.movement_time = 0.;
                    commands.add_to_list(List::AtLocation, next_step, this.id());
                } else {
                    // Otherwise, just step up the movement time
                    this.movement_time += 1.;
                }
            }
        }
    }

    let wants_to_be_inside = this.flag(Flag::WantsToBeInside);
    let is_inside =
        wants_to_be_inside && (this.destination.is_null() || current_location == this.destination);
    this.set_flag(Flag::IsInside, is_inside);
}

enum MovementStatus {
    Arrived,
    Moving,
}

fn update_body_of_local_things(ctx: &Things, this: &mut Thing, delta: f32) -> MovementStatus {
    let location = match this.parent(List::AtLocation).get_as_valid(ctx) {
        Some(x) => x,
        None => return MovementStatus::Moving,
    };

    // Find my position around the target
    let target = if this.flag(Flag::IsInside) {
        V2::new(location.body.x, location.body.y)
    } else {
        let mut idx: usize = 0;
        let mut len: usize = 0;
        for thing in ctx.iter_list_get(List::AtLocation, location.id()) {
            // We only consider the people at a location
            if !thing.flag(Flag::IsPerson) {
                continue;
            }

            if thing.id() == this.id() {
                idx = len;
            }
            if !thing.flag(Flag::IsInside) {
                len += 1;
            }
        }
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
            current_pos + dv * delta * MOVEMENT_LERP_SPEED
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
    let (msg_ref, message) = commands.spawn();
    message.name = text;
    for (slot, value) in message.params.iter_mut().zip(params) {
        *slot = *value;
    }
    commands.add_to_list(List::Messages, recepient, msg_ref);
}

fn render_thing(
    ctx: &Things,
    this: &Thing,
    draw_data: &mut DrawData,
    selected_id: ThingId,
    target_type: TargetType,
) {
    if this.flag(Flag::IsInvisible) {
        return;
    }

    if this.body.size > 0 && !this.sprite.is_empty() {
        let is_selected = this.id() == selected_id;

        let size = this.body.size as f32;
        let xy = V2::new(this.body.x, this.body.y) - size / 2.;
        let bounds = Rect::new(xy.x, xy.y, size, size);

        let border_highlight = if is_selected {
            HighlightType::Selection
        } else if target_type.check(this) {
            HighlightType::Target
        } else {
            HighlightType::Nothing
        };

        let transparency_intensity = if this.flag(Flag::IsActivity) {
            0.5
        } else {
            0.0
        };

        let sprite = Sprite {
            image: this.sprite,
            bounds,
            layer: this.body.layer,
            border_highlight,
            pulse_intensity: if is_selected { 1.0 } else { 0.0 },
            transparency_intensity,
        };
        draw_data.sprites.push(sprite);

        let show_name = is_selected || this.flag(Flag::IsSettlement);
        if show_name {
            let name = this.name;
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
        let a = this.edge_from;
        let b = this.edge_to;
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub(crate) struct TokenCount([usize; TOKEN_TYPES.len()]);

impl TokenCount {
    pub fn total(&self) -> usize {
        self.iter().map(|x| x.1).sum()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static TokenType, usize)> {
        self.0
            .iter()
            .enumerate()
            .map(|(idx, value)| (&TOKEN_TYPES[idx], *value))
    }
}

#[derive(Default)]
struct Effects {
    transfer_tokens: Vec<TransferToken>,
}

impl Effects {
    fn transfer_tokens(&mut self, source: ThingId, recepient: ThingId, change_type_to: u16) {
        self.transfer_tokens.push(TransferToken {
            source,
            recepient,
            change_type_to,
        });
    }
}

struct TransferToken {
    source: ThingId,
    recepient: ThingId,
    change_type_to: u16,
}

impl Effects {
    fn new() -> Self {
        Self::default()
    }
    fn perform(self, ctx: &mut Things, arena: &Arena) {
        ctx.with_commands(|ctx, _| {
            // Apply token transferral
            for transfer in self.transfer_tokens {
                // Get all the tokens at the source
                let tokens = arena.alloc_slice_iter(
                    ctx.iter_list_get(List::TokensSourced, transfer.source)
                        .filter(|tok| transfer.recepient != tok.parent(List::TokensHeld)),
                );
                // Score and sort the tokens
                tokens.sort_by_key(|token| if token.kind == 0 { 100 } else { 0 });

                // Re-assign and transform type
                let best_tok = tokens.last().map(|x| x.id()).unwrap_or_default();
                ctx[best_tok].kind = transfer.change_type_to;
                ctx.add_to_list(List::TokensHeld, transfer.recepient, best_tok);
            }
        });
    }
}
