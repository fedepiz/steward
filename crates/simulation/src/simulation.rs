use bumpalo::Bump;
use slotmap::{Key, KeyData};
use util::string_pool::*;

use crate::{
    agents::{self, Agent, AgentId, Agents, Behavior, Hierarchy, Location, Task, TaskKind, Var},
    geom::V2,
    movement::{self, SpatialMap},
    names::Names,
    objects::{Objects, ObjectsBuilder},
    parties::{self, *},
    terrain_map::TerrainMap,
};

#[derive(Default)]
pub(crate) struct Simulation {
    pub turn_num: usize,
    pub terrain_map: TerrainMap,
    pub names: Names,
    pub parties: Parties,
    movement_cache: movement::MovementCache,
    pub agents: Agents,
    player_party_goal: parties::Goal,
}

impl Simulation {
    pub(crate) fn tick(&mut self, request: Request, arena: &Bump) -> Response {
        let _span = tracing::info_span!("Tick").entered();
        tick(self, request, arena)
    }
}

fn tick(sim: &mut Simulation, mut request: Request, arena: &Bump) -> Response {
    if let Some(req) = request.init.take() {
        *sim = Simulation::default();
        crate::init::init(sim, req);
    }

    // Input handling
    {
        // From move pos
        if let Some((x, y)) = request.move_to_pos.take() {
            sim.player_party_goal = Goal::MoveTo(V2::new(x, y));
        }

        // From move to target
        if let Some(item_id) = request.move_to_item.take() {
            let target = PartyId::from(KeyData::from_ffi(item_id.0));
            sim.player_party_goal = Goal::ToParty {
                target,
                distance: 0.,
            };
        }
    }

    if request.advance_time {
        sim.turn_num = sim.turn_num.wrapping_add(1);

        spawn_farmers(sim, arena);
        move_farmers(sim, arena);

        if sim.turn_num % 100 == 0 {
            food_production_and_consumption(sim, arena);
        }

        let mut movement_elements = Vec::with_capacity(sim.parties.len());

        // Parties logical update
        for party in sim.parties.iter() {
            let goal = determine_party_goal(sim, party);

            let detection = sim.parties.detections.get_for(party.id);

            let movement_target = determine_party_movement_target(party, detection, goal);

            // Resolve movement target
            let (destination, direct) = match movement_target {
                MovementTarget::Immobile => (party.body.pos, false),
                MovementTarget::FixedPos(pos) => (pos, false),
                MovementTarget::Party(id) => (sim.parties[id].body.pos, false),
            };

            // Actual movement element
            movement_elements.push(movement::Element {
                id: party.id,
                speed: party.speed,
                pos: party.body.pos,
                destination,
                direct,
            });
        }

        // Apply movement results back to parties
        let movement_result = movement::tick_movement(
            &mut sim.movement_cache,
            &movement_elements,
            &sim.terrain_map,
        );

        // Party detection check
        let spatial_map = &movement_result.spatial_map;
        {
            let mut detections = std::mem::take(&mut sim.parties.detections);
            self::detections(&mut detections, &sim.parties, spatial_map);
            sim.parties.detections = detections;
        }

        // Iterate writeback
        {
            let mut positions = movement_result.positions.into_iter();

            for party in sim.parties.iter_mut() {
                let (id, pos) = positions.next().unwrap();
                assert!(id == party.id);
                party.body.pos = pos;
            }
        }

        // Determine agent location
        {
            let _span = tracing::info_span!("Calculate agent locations").entered();
            let mut scratch = vec![];

            let output: Vec<_> = sim
                .agents
                .iter()
                .map(|agent| {
                    let location = location_of_agent(sim, agent, &mut scratch);
                    (agent.id, location)
                })
                .collect();

            // Writeback
            for (id, location) in output {
                sim.agents[id].location = location;
            }
        }
    }

    let mut response = Response::default();
    view(sim, &request, &mut response);
    response
}

fn location_of_agent(
    sim: &Simulation,
    agent: &Agent,
    scratch: &mut Vec<(AgentId, f32)>,
) -> Location {
    // Disembodied agent has no location
    if agent.party.is_null() {
        return Location::Nowhere;
    }

    // If the agent is a settlement, then it's at a settlement!
    let party = &sim.parties[agent.party];
    if party.is_location {
        return Location::AtAgent(agent.id);
    }

    // Otherwise, look at the detections, collecting into scratch space
    let detections = sim.parties.detections.get_for(party.id);

    scratch.clear();

    // Only consider parties in the target set
    scratch.extend(
        detections
            .iter()
            .filter(|det| !det.location_agent.is_null())
            .map(|det| (det.location_agent, det.distance)),
    );

    // Get the closest. If none, we are just "far out"
    scratch
        .iter()
        .min_by_key(|(_, d)| (d * 100.).round() as i64)
        .map(|&(id, distance)| {
            if distance <= 0. {
                Location::AtAgent(id)
            } else {
                Location::Near(id)
            }
        })
        .unwrap_or(Location::FarOut)
}

impl movement::MovementGraph for TerrainMap {
    fn size(&self) -> (usize, usize) {
        self.size()
    }

    fn get_speed_at(&self, x: i64, y: i64) -> f32 {
        let x = x.max(0) as usize;
        let y = y.max(0) as usize;
        let terrain = self.terrain_at(x, y);
        terrain.movement_speed_multiplier
    }
}

#[derive(Default)]
pub struct Request {
    pub init: Option<InitRequest>,
    pub advance_time: bool,
    pub move_to_pos: Option<(f32, f32)>,
    pub move_to_item: Option<MapItemId>,
    pub highlighted_item: Option<MapItemId>,
    pub extract_terrain: bool,
    strings: StringPool,
    view_entities: Vec<ViewEntity>,
}

#[derive(Default)]
pub struct InitRequest {
    pub map_width: usize,
    pub map_height: usize,
    pub elevations: Vec<u8>,
}

impl Request {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn view_map_item(&mut self, key: &str, id: MapItemId) {
        let entity = PartyId::from(KeyData::from_ffi(id.0));
        self.view_entity(key, entity);
    }

    fn view_entity(&mut self, key: &str, entity: PartyId) {
        let key = self.strings.push_str(key);
        self.view_entities.push(ViewEntity { tag: key, entity });
    }
}

fn detections(detections: &mut Detections, entities: &Parties, spatial_map: &SpatialMap) {
    detections.clear();

    let mut scratch = Vec::with_capacity(100);
    for entity in entities.iter() {
        const DETECTION_RANGE: f32 = 20.;
        let neighbours = spatial_map.search(entity.body.pos, DETECTION_RANGE);
        for target in neighbours {
            // Do not look at yourself
            if target == entity.id {
                continue;
            }
            let target = &entities[target];
            let center_to_center_distance = V2::distance(entity.body.pos, target.body.pos);
            let collision_range = (entity.body.size + target.body.size) / 2.;

            // Distance net of sizes. 0 or lower means collision
            let distance = center_to_center_distance - collision_range;

            let location_agent = if target.is_location {
                target.agent
            } else {
                AgentId::null()
            };

            scratch.push(Detection {
                target: target.id,
                location_agent,
                distance,
            });
        }
        detections.set(entity.id, &scratch);
        scratch.clear();
    }
}

#[derive(Clone, Copy)]
struct ViewEntity {
    tag: SpanHandle,
    entity: PartyId,
}

fn view(sim: &Simulation, req: &Request, response: &mut Response) {
    let _span = tracing::info_span!("View").entered();

    {
        let _span = tracing::info_span!("Objects").entered();
        let mut ctx = ObjectsBuilder::default();
        // Create a default zero object, so that ObjectId::default() is valid but unused
        ctx.spawn(|_| {});
        // Create the 'global' object
        ctx.spawn(|ctx| {
            ctx.tag("root");
            ctx.fmt("tick_num", format_args!("Turn number: {}", sim.turn_num));
        });

        // Create requested objects
        for view_entity in &req.view_entities {
            let party = &sim.parties[view_entity.entity];
            ctx.spawn(|ctx| {
                let tag = req.strings.get(view_entity.tag);
                ctx.tag(tag);
                ctx.display("name", sim.names.resolve(party.name));

                let agent = &sim.agents[party.agent];

                ctx.display("food", agent.get_var(Var::Food));

                if agent.in_set(agents::Set::People) {
                    ctx.display("renown", agent.get_var(Var::Renown));
                }

                if agent.in_set(agents::Set::Settlements) {
                    ctx.display("population", agent.get_var(Var::Population));
                    ctx.fmt(
                        "prosperity",
                        format_args!("{:1.2}%", agent.get_var(Var::Prosperity) * 100.),
                    );
                }
            });
        }

        response.objects = ctx.build();
    }

    {
        let _span = tracing::info_span!("Map Items").entered();
        let ctx = &mut response.map_items;
        let highlighted_entity = req
            .highlighted_item
            .map(|id| id.as_entity())
            .unwrap_or_default();

        // Get parties and types, sorted by layer
        let mut parties: Vec<_> = sim
            .parties
            .iter()
            .map(|party| (party, sim.parties.get_type(party.type_id).layer))
            .collect();

        parties.sort_by_key(|(_, layer)| *layer);

        for (entity, _) in parties {
            // TODO: Filter here for being in view
            let typ = sim.parties.get_type(entity.type_id);

            let is_highlighted = highlighted_entity == entity.id;
            let show_name = is_highlighted || typ.always_show_name;

            let name = if show_name {
                let name = sim.names.resolve(entity.name);
                ctx.names.push(name)
            } else {
                Default::default()
            };

            ctx.entries.push(MapItemData {
                id: entity.id.data().as_ffi(),
                name,
                image: typ.image,
                body: entity.body,
            });
        }
    }

    if req.extract_terrain {
        let _span = tracing::info_span!("Map Terrain").entered();
        let mut map_terrain = MapTerrain::default();
        map_terrain.hash = sim.terrain_map.hash();
        map_terrain.size = sim.terrain_map.size();
        map_terrain.tiles = sim
            .terrain_map
            .iter_terrains()
            .map(|typ| typ.color)
            .collect();
        response.map_terrain = Some(map_terrain);
    }
}

#[derive(Default)]
pub struct Response {
    pub objects: Objects,
    pub map_items: MapItems,
    pub map_terrain: Option<MapTerrain>,
}

#[derive(Default)]
pub struct MapItems {
    names: StringPool,
    entries: Vec<MapItemData>,
}

impl MapItems {
    fn get(&self, data: &MapItemData) -> MapItem<'_> {
        MapItem {
            id: MapItemId(data.id),
            name: self.names.get(data.name),
            image: data.image,
            x: data.body.pos.x - data.body.size / 2.,
            y: data.body.pos.y - data.body.size / 2.,
            width: data.body.size,
            height: data.body.size,
        }
    }

    pub fn get_by_index(&self, index: usize) -> MapItem<'_> {
        let data = self.entries.get(index).copied().unwrap_or_default();
        self.get(&data)
    }

    pub fn iter(&self) -> impl Iterator<Item = MapItem<'_>> + use<'_> {
        self.entries.iter().map(|data| self.get(data))
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MapItemId(pub u64);

impl MapItemId {
    pub(crate) fn as_entity(self) -> PartyId {
        PartyId::from(KeyData::from_ffi(self.0))
    }
}

#[derive(Default, Clone, Copy)]
struct MapItemData {
    id: u64,
    name: SpanHandle,
    image: &'static str,
    body: Body,
}

pub struct MapItem<'a> {
    pub id: MapItemId,
    pub name: &'a str,
    pub image: &'static str,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

#[derive(Default)]
pub struct MapTerrain {
    pub hash: u64,
    pub size: (usize, usize),
    pub tiles: Vec<(u8, u8, u8)>,
}

fn spawn_farmers(sim: &mut Simulation, arena: &Bump) {
    let mut spawns = bumpalo::collections::Vec::new_in(arena);
    let farmer_type = sim.parties.find_type_by_tag("farmers").unwrap().id;

    for agent in sim.agents.iter_set(crate::agents::Set::Villages) {
        // Check for all the dependents
        let dependents = sim.agents.children_of(Hierarchy::Attachment, agent.id);
        // Do we have a farmer?
        let has_farmer = dependents
            .into_iter()
            .any(|child| sim.agents[child].in_set(agents::Set::Farmers));

        // If we do not have a farmer, we will need to spawn one
        if !has_farmer {
            let party = &sim.parties[agent.party];
            spawns.push((agent.id, party.body.pos));
        }
    }

    // Spawn the farmers as necessary
    for (center, pos) in spawns {
        // Spawn agent (now ihere, later floated out)
        let party = sim.parties.spawn_with_type(farmer_type);
        party.body.pos = pos;

        let agent = sim.agents.spawn();
        agent.name = party.name;

        // Tie agent and party together
        agent.party = party.id;
        // NOTE: This should ideally be auto-computed, but that means moving spawns around.
        // Spawning should ideally not happen here anyways.
        agent.location = Location::AtAgent(center);
        party.agent = agent.id;

        let agent = agent.id;
        sim.agents.add_to_set(agents::Set::Farmers, agent);
        sim.agents.set_parent(Hierarchy::Attachment, center, agent);
    }
}

fn move_farmers(sim: &mut Simulation, arena: &Bump) {
    let mut behavior_changes = bumpalo::collections::Vec::new_in(arena);
    let mut var_changes = bumpalo::collections::Vec::new_in(arena);
    const MIN_CARRIED_FOOD: i64 = 100;
    const MAX_CARRIED_FOOD: i64 = 500;
    const MAX_CARRIED_PROP: f64 = 0.5;

    for farmer in sim.agents.iter_set(agents::Set::Farmers) {
        let home = sim.agents.parent_of(Hierarchy::Attachment, farmer.id);
        let target = sim.agents.parent_of(Hierarchy::LocalMarket, home);

        let current_food = farmer.get_var(Var::Food) as i64;

        // If the farmer is at home, and it has a valid target, move to the target
        if farmer.location == Location::AtAgent(home) && !target.is_null() {
            let home = &sim.agents[home];

            let mut at_settlement = home.get_var(Var::Food) as i64;
            // Export up to a fixed proportion of settlement stock, capped by what's available.
            let max_exportable = (at_settlement as f64 * MAX_CARRIED_PROP)
                .min(at_settlement as f64)
                .round() as i64;

            if current_food + max_exportable >= MIN_CARRIED_FOOD {
                // Load as much as allowed, then shed any excess to respect carrying cap.
                let mut on_farmer = current_food + max_exportable;
                let put_down = (on_farmer - MAX_CARRIED_FOOD).max(0);
                on_farmer -= put_down;
                // Settlement stock decreases by export, then regains whatever couldn't be carried.
                at_settlement = at_settlement - max_exportable + put_down;

                var_changes.push((farmer.id, Var::Food, on_farmer as f64));
                var_changes.push((home.id, Var::Food, at_settlement as f64));

                behavior_changes.push((farmer.id, Behavior::GoTo(target)));
            }
        } else
        // If the farmer is at the target, comeback home
        if farmer.location == Location::AtAgent(target) {
            let target = &sim.agents[target];
            let on_farmer = farmer.get_var(Var::Food) as i64;
            let on_settlement = target.get_var(Var::Food) as i64;
            let new_at_settlement = on_farmer + on_settlement;

            var_changes.push((farmer.id, Var::Food, 0.));
            var_changes.push((target.id, Var::Food, new_at_settlement as f64));

            behavior_changes.push((farmer.id, Behavior::GoTo(home)));
        }
    }

    for (id, behavior) in behavior_changes {
        sim.agents[id].behavior = behavior;
    }

    for (id, var, value) in var_changes {
        sim.agents[id].vars_mut().with(var, value);
    }
}

fn food_production_and_consumption(sim: &mut Simulation, arena: &Bump) {
    let mut changes = bumpalo::collections::Vec::new_in(arena);
    for settlement in sim.agents.iter_set(agents::Set::Settlements) {
        let mut food = settlement.get_var(Var::Food) as i64;
        let population = settlement.get_var(Var::Population) as i64;
        let food_production = 100;
        let mut max_food = 0;
        if settlement.in_set(agents::Set::Villages) {
            max_food += 1000;
        }
        if settlement.in_set(agents::Set::Hillforts) {
            max_food += 10_000;
        }
        if settlement.in_set(agents::Set::Towns) {
            max_food += 10_000;
        }
        food = (food + food_production - population).min(max_food);
        if food < 0 {
            // Shortfall
            food = 0;
        }
        changes.push((settlement.id, food as f64));
    }

    for (agent, food) in changes {
        let agent = &mut sim.agents[agent];
        agent.vars_mut().with(Var::Food, food);
    }
}

fn determine_party_goal(sim: &Simulation, party: &Party) -> Goal {
    if party.speed == 0.0 {
        return Goal::Idle;
    }

    let agent = match sim.agents.get(party.agent) {
        Some(agent) => agent,
        None => return Goal::Idle,
    };

    match agent.behavior {
        Behavior::Idle => Goal::Idle,
        Behavior::Player => sim.player_party_goal,
        Behavior::Test => Goal::MoveTo(V2::splat(500.)),
        Behavior::GoTo(target) => sim
            .agents
            .get(target)
            .and_then(|agent| sim.parties.get(agent.party))
            .map(|party| Goal::ToParty {
                target: party.id,
                distance: std::f32::NEG_INFINITY,
            })
            .unwrap_or_default(),
    }
}

pub(crate) fn determine_party_movement_target(
    _: &Party,
    detections: &[Detection],
    goal: Goal,
) -> MovementTarget {
    match goal {
        Goal::Idle => MovementTarget::Immobile,
        Goal::MoveTo(pos) => MovementTarget::FixedPos(pos),
        Goal::ToParty { target, distance } => {
            let close_to_target = detections
                .iter()
                .find(|d| target == d.target)
                .map(|d| d.distance <= distance)
                .unwrap_or(false);

            if !close_to_target {
                MovementTarget::Party(target)
            } else {
                MovementTarget::Immobile
            }
        }
    }
}
