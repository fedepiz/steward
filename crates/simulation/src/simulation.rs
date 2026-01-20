use bumpalo::Bump;
use slotmap::{Key, KeyData};
use util::string_pool::*;

use crate::{
    agents::{self, *},
    geom::V2,
    movement::{self, SpatialMap},
    names::Names,
    objects::*,
    parties::{self, *},
    terrain_map::TerrainMap,
};

type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;

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

        spawn_worker(
            sim,
            arena,
            "farmers",
            agents::Set::Villages,
            agents::Flag::IsFarmer,
            &[],
        );

        spawn_worker(
            sim,
            arena,
            "miners",
            agents::Set::Mines,
            agents::Flag::IsMiner,
            &[(Var::ProsperityBonus, 0.01)],
        );

        agent_tasking(sim, arena);

        if sim.turn_num % 100 == 0 {
            food_production_and_consumption(sim, arena);
            // Food stock sets the target population; this nudges population toward that target.
            population_from_food(sim, arena);
            // Prosperity drifts toward a baseline over time.
            prosperity_towards_equilibrium(sim, arena);
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

                {
                    // Display food
                    let stored = agent.get_var(Var::FoodStored);
                    if agent.in_set(Set::Settlements) {
                        let capacity = agent.get_var(Var::FoodCapacity);
                        ctx.display("food", format_args!("{stored}/{capacity}"));
                    } else {
                        ctx.display("food", stored);
                    }
                }

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

fn spawn_worker(
    sim: &mut Simulation,
    arena: &Bump,
    party_type: &str,
    source_set: agents::Set,
    worker_flag: agents::Flag,
    vars: &[(Var, f64)],
) {
    let mut spawns = AVec::new_in(arena);
    let party_type = sim.parties.find_type_by_tag(party_type).unwrap().id;

    for agent in sim.agents.iter_set(source_set) {
        // Check for all the dependents
        let dependents = sim.agents.children_of(Hierarchy::Attachment, agent.id);
        // Do we have a worker?
        let has_worker = dependents
            .into_iter()
            .any(|child| sim.agents[child].flags.get(worker_flag));

        // If we do not have a worker, we will need to spawn one
        if !has_worker {
            let spawn = SpawnSubagent {
                party_type,
                parent: agent.id,
                flags: Flags::default().with(worker_flag),
                vars,
            };
            spawns.push(spawn);
        }
    }

    // Spawn the miners as necessary
    for spawn in spawns {
        spawn_subagent(sim, spawn);
    }
}

struct SpawnSubagent<'a> {
    party_type: PartyTypeId,
    parent: AgentId,
    flags: Flags,
    vars: &'a [(Var, f64)],
}

fn spawn_subagent(sim: &mut Simulation, spawn: SpawnSubagent) {
    // Spawn agent (now ihere, later floated out)
    let pos = sim.parties[sim.agents[spawn.parent].party].body.pos;
    let party = sim.parties.spawn_with_type(spawn.party_type);
    party.body.pos = pos;

    let agent = sim.agents.spawn();
    agent.name = party.name;

    // Tie agent and party together
    agent.party = party.id;
    // NOTE: This should ideally be auto-computed, but that means moving spawns around.
    // Spawning should ideally not happen here anyways.
    agent.location = Location::AtAgent(spawn.parent);
    agent.flags = spawn.flags;
    agent.vars_mut().set_many(spawn.vars);

    party.agent = agent.id;

    let agent = agent.id;
    sim.agents
        .set_parent(Hierarchy::Attachment, spawn.parent, agent);
}

// Units of food consumed per unit population per economic tick.
const FOOD_CONSUMPTION_PER_HEAD: f64 = 0.01;

fn food_production_and_consumption(sim: &mut Simulation, arena: &Bump) {
    // Prosperity scales the base food output from villages and towns.
    const BASE_FOOD_PRODUCTION: f64 = 100.0;
    const PROSPERITY_IMPACT_ON_FOOD_PRODUCTION: f64 = 0.1;
    let mut changes = AVec::new_in(arena);
    for settlement in sim.agents.iter_set(agents::Set::Settlements) {
        let mut food = settlement.get_var(Var::FoodStored) as i64;
        let population = settlement.get_var(Var::Population);
        let prosperity = settlement.get_var(Var::Prosperity).clamp(0.0, 1.0);
        let max_food = settlement.get_var(Var::FoodCapacity) as i64;
        let food_demand = (population * FOOD_CONSUMPTION_PER_HEAD).round() as i64;

        let food_production = {
            let modifier = 1.0 + prosperity * PROSPERITY_IMPACT_ON_FOOD_PRODUCTION;
            (BASE_FOOD_PRODUCTION * modifier).round().max(0.0) as i64
        };
        food = (food + food_production - food_demand).min(max_food);
        if food < 0 {
            // Shortfall
            food = 0;
        }
        changes.push((settlement.id, food as f64));
    }

    for (agent, food) in changes {
        let agent = &mut sim.agents[agent];
        agent.vars_mut().with(Var::FoodStored, food);
    }
}

fn population_from_food(sim: &mut Simulation, arena: &Bump) {
    // Convert food stock into a target population and converge gradually.
    const POPULATION_CONVERGENCE: f64 = 0.01;
    // The target is some kind of multiple of the food consumption
    const TICK_OF_SLACK: f64 = 100.;
    let mut changes = AVec::new_in(arena);

    for settlement in sim.agents.iter_set(agents::Set::Settlements) {
        let food = settlement.get_var(Var::FoodStored);
        let population = settlement.get_var(Var::Population);
        let target = (food / (FOOD_CONSUMPTION_PER_HEAD * TICK_OF_SLACK)).max(0.0);
        let next = population + (target - population) * POPULATION_CONVERGENCE;
        changes.push((settlement.id, next.max(0.0).round()));
    }

    for (agent, population) in changes {
        let agent = &mut sim.agents[agent];
        agent.vars_mut().with(Var::Population, population);
    }
}

fn prosperity_towards_equilibrium(sim: &mut Simulation, arena: &Bump) {
    // Simple baseline drift so prosperity doesn't stick forever at extremes.
    const PROSPERITY_EQUILIBRIUM: f64 = 0.5;
    const PROSPERITY_CONVERGENCE: f64 = 0.001;
    let mut changes = AVec::new_in(arena);

    for settlement in sim.agents.iter_set(agents::Set::Settlements) {
        let prosperity = settlement.get_var(Var::Prosperity);
        let next = prosperity + (PROSPERITY_EQUILIBRIUM - prosperity) * PROSPERITY_CONVERGENCE;
        changes.push((settlement.id, next.clamp(0.0, 1.0)));
    }

    for (agent, prosperity) in changes {
        let agent = &mut sim.agents[agent];
        agent.vars_mut().with(Var::Prosperity, prosperity);
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

// Agent tasking

fn agent_tasking(sim: &mut Simulation, arena: &Bump) {
    // Snapshot tasks so task selection can read and write without aliasing agents.
    let tasks = AVec::from_iter_in(
        sim.agents
            .iter_mut()
            .map(|agent| std::mem::take(&mut agent.task)),
        arena,
    );

    // Determine task, destination, and desired behavior for each agent.
    let mut results = AVec::with_capacity_in(tasks.len(), arena);
    for (subject, task) in sim.agents.iter().zip(tasks) {
        let (task, destination, behavior) = task_for_agent(sim, subject, task);
        results.push((subject.id, task, destination, behavior));
    }

    // Resolve task effects once agents are at their destinations.
    for (subject, task, destination, _) in &mut results {
        resolve_task_effects(sim, *subject, *destination, task);
    }

    // Commit task/behavior changes; fixed behavior overrides task-driven behavior.
    for (agent, (_, task, _, behavior)) in sim.agents.iter_mut().zip(results) {
        agent.task = task;
        agent.behavior = agent.fixed_behavior.unwrap_or(behavior);
    }
}

fn farmer_tasking(kind: TaskKind) -> Task {
    match kind {
        TaskKind::Init => Task {
            kind: TaskKind::ReturnHome,
            destination: TaskDestination::Home,
            ..Default::default()
        },
        TaskKind::ReturnHome => Task {
            kind: TaskKind::Load,
            destination: TaskDestination::Home,
            interaction: TaskInteraction::with(Interaction::LoadFood),
            ..Default::default()
        },
        TaskKind::Deliver => Task {
            kind: TaskKind::ReturnHome,
            destination: TaskDestination::Home,
            interaction: TaskInteraction::new(&[
                Interaction::IncreaseProsperity,
                Interaction::ResetProsperityBonus,
            ]),
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::MarketOfHome,
            // Deliver food to town and load a prosperity bonus for the return trip.
            interaction: TaskInteraction::new(&[
                Interaction::UnloadFood,
                Interaction::LoadProsperityBonus,
            ]),
            ..Default::default()
        },
    }
}

fn miner_tasking(kind: TaskKind) -> Task {
    match kind {
        TaskKind::Init => Task {
            kind: TaskKind::ReturnHome,
            destination: TaskDestination::Home,
            ..Default::default()
        },
        TaskKind::ReturnHome => Task {
            kind: TaskKind::Load,
            destination: TaskDestination::Home,
            interaction: TaskInteraction::default(),
            ..Default::default()
        },
        TaskKind::Deliver => Task {
            kind: TaskKind::ReturnHome,
            destination: TaskDestination::Home,
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::MarketOfHome,
            interaction: TaskInteraction::with(Interaction::IncreaseProsperity),
            ..Default::default()
        },
    }
}

fn task_for_agent(sim: &Simulation, subject: &Agent, mut task: Task) -> (Task, AgentId, Behavior) {
    // Retask when initializing or once the current task is complete.
    let retask = task.kind == TaskKind::Init || task.is_complete;
    if retask {
        let is_farmer = subject.flags.get(agents::Flag::IsFarmer);
        task = if is_farmer {
            farmer_tasking(task.kind)
        } else if subject.flags.get(agents::Flag::IsMiner) {
            miner_tasking(task.kind)
        } else {
            Task::default()
        };
    }

    // Resolve the concrete target for the task's symbolic destination.
    let destination = match task.destination {
        TaskDestination::Nothing => AgentId::null(),
        TaskDestination::Home => sim.agents.parent_of(Hierarchy::Attachment, subject.id),
        TaskDestination::MarketOfHome => {
            let home = sim.agents.parent_of(Hierarchy::Attachment, subject.id);
            sim.agents.parent_of(Hierarchy::LocalMarket, home)
        }
    };

    // Reset task if destination is not valid
    let is_valid = !destination.is_null();
    if !is_valid {
        return (Task::default(), AgentId::null(), Behavior::Idle);
    }

    // Behavior is driven by the task destination.
    let behavior = if destination.is_null() {
        subject.behavior
    } else {
        Behavior::GoTo(destination)
    };

    (task, destination, behavior)
}

fn resolve_task_effects(
    sim: &mut Simulation,
    subject: AgentId,
    destination: AgentId,
    task: &mut Task,
) {
    let at_destination =
        !destination.is_null() && sim.agents[subject].location == Location::AtAgent(destination);
    if !at_destination {
        return;
    }

    for interaction in task.interaction.iter_active() {
        let is_complete = handle_interaction(sim, interaction, subject, destination);
        if is_complete {
            task.interaction.set(interaction, false);
        }
    }

    task.is_complete = at_destination && !task.interaction.any();
}

fn handle_interaction(
    sim: &mut Simulation,
    interaction: Interaction,
    subject: AgentId,
    destination: AgentId,
) -> bool {
    match interaction {
        Interaction::UnloadFood => {
            let on_farmer = sim.agents[subject].get_var(Var::FoodStored) as i64;
            let on_settlement = sim.agents[destination].get_var(Var::FoodStored) as i64;
            let new_at_settlement = on_farmer + on_settlement;

            sim.agents[subject].vars_mut().with(Var::FoodStored, 0.);
            sim.agents[destination]
                .vars_mut()
                .with(Var::FoodStored, new_at_settlement as f64);
            true
        }
        Interaction::LoadFood => {
            const MIN_CARRIED_FOOD: i64 = 50;
            const MAX_CARRIED_FOOD: i64 = 500;
            const MAX_CARRIED_PROP: f64 = 0.5;

            let current_food = sim.agents[subject].get_var(Var::FoodStored) as i64;
            let mut at_settlement = sim.agents[destination].get_var(Var::FoodStored) as i64;
            let max_exportable = (at_settlement as f64 * MAX_CARRIED_PROP)
                .min(at_settlement as f64)
                .round() as i64;

            if current_food + max_exportable >= MIN_CARRIED_FOOD {
                let mut on_farmer = current_food + max_exportable;
                let put_down = (on_farmer - MAX_CARRIED_FOOD).max(0);
                on_farmer -= put_down;
                at_settlement = at_settlement - max_exportable + put_down;

                sim.agents[subject]
                    .vars_mut()
                    .with(Var::FoodStored, on_farmer as f64);
                sim.agents[destination]
                    .vars_mut()
                    .with(Var::FoodStored, at_settlement as f64);
                true
            } else {
                false
            }
        }
        Interaction::LoadProsperityBonus => {
            // Capture town prosperity as a bonus to be applied when returning home.
            const PROSPERITY_BONUS_SCALE: f64 = 0.025;
            let prosperity = sim.agents[destination].get_var(Var::Prosperity);
            let bonus = (prosperity * PROSPERITY_BONUS_SCALE).max(0.0);
            sim.agents[subject]
                .vars_mut()
                .with(Var::ProsperityBonus, bonus);
            true
        }
        Interaction::IncreaseProsperity => {
            let value = sim.agents[subject].get_var(Var::ProsperityBonus);
            sim.agents[destination]
                .vars_mut()
                .modify(Var::Prosperity, |x| x + value);
            true
        }
        Interaction::ResetProsperityBonus => {
            sim.agents[subject].vars_mut().set(Var::ProsperityBonus, 0.);
            true
        }
    }
}
