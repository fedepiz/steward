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
    view::{MapItems, MapTerrain},
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
                enter_on_arrival: false,
            };
        }
    }

    for _ in 0..request.advance_ticks {
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

        crate::agent_tasking::agent_tasking(sim, arena);

        const ECONOMY_TICK_RATE: usize = 200;
        if sim.turn_num % ECONOMY_TICK_RATE == 0 {
            food_production_and_consumption(sim, arena);
            // Food stock sets the target population; this nudges population toward that target.
            population_from_food(sim, arena);
            // Prosperity drifts toward a baseline over time.
            prosperity_towards_equilibrium(sim, arena);
        }

        let mut movement_elements = AVec::with_capacity_in(sim.parties.len(), arena);
        let mut desired_change_of_container = AVec::with_capacity_in(sim.parties.len(), arena);

        // Parties logical update
        for party in sim.parties.iter() {
            let goal = determine_party_goal(sim, party);

            let detection = sim.parties.detections.get_for(party.id);

            let movement_target = determine_party_movement_target(party, detection, goal);

            let desired_container = match goal {
                Goal::ToParty {
                    target,
                    enter_on_arrival: true,
                    ..
                } => target,
                _ => PartyId::null(),
            };
            if desired_container != party.inside_of {
                desired_change_of_container.push((party.id, desired_container))
            }

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
            let _span = tracing::info_span!("Detections").entered();
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

        // Resolve desired changes of party container
        for (party, container) in desired_change_of_container {
            if container.is_null() {
                sim.parties[party].inside_of = PartyId::null();
            } else {
                let pos = sim.parties[container].body.pos;
                let party = &mut sim.parties[party];
                party.inside_of = if party.body.pos == pos {
                    container
                } else {
                    PartyId::null()
                };
            }
        }

        // Determine agent location
        {
            let _span = tracing::info_span!("Agent Locations").entered();
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
    crate::view::view(sim, &request, &mut response);
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
        return Location::Proximate(agent.id);
    }

    // If an agent's party is inside another party, that the agent party is at the location
    if let Some(container) = sim.parties.get(party.inside_of) {
        return Location::Inside(container.agent);
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
                Location::Proximate(id)
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
    pub advance_ticks: u32,
    pub move_to_pos: Option<(f32, f32)>,
    pub move_to_item: Option<MapItemId>,
    pub highlighted_item: Option<MapItemId>,
    pub extract_terrain: bool,
    pub(crate) strings: StringPool,
    pub(crate) view_entities: Vec<ViewEntity>,
}

impl Request {
    pub(crate) fn entity_tag(&self, view: ViewEntity) -> &'_ str {
        self.strings.get(view.tag)
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MapItemId(pub u64);

#[derive(Clone, Copy)]
pub(crate) struct ViewEntity {
    tag: SpanHandle,
    pub(crate) entity: PartyId,
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

#[derive(Default)]
pub struct Response {
    pub objects: Objects,
    pub map_items: MapItems,
    pub map_terrain: Option<MapTerrain>,
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
    let (parent_party, parent_pos) = {
        let party = &sim.parties[sim.agents[spawn.parent].party];
        (party.id, party.body.pos)
    };
    let party = sim.parties.spawn_with_type(spawn.party_type);
    party.body.pos = parent_pos;
    party.inside_of = parent_party;

    let agent = sim.agents.spawn();
    agent.name = party.name;

    // Tie agent and party together
    agent.party = party.id;
    // NOTE: This should ideally be auto-computed, but that means moving spawns around.
    // Spawning should ideally not happen here anyways.
    agent.location = Location::Inside(spawn.parent);
    agent.flags = spawn.flags;
    agent.vars_mut().set_many(spawn.vars);

    party.agent = agent.id;

    let agent = agent.id;
    sim.agents
        .set_parent(Hierarchy::Attachment, spawn.parent, agent);
}

// Units of food consumed per unit population per economic tick.
const FOOD_CONSUMPTION_PER_HEAD: f64 = 0.1;

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
    const TICK_OF_SLACK: f64 = 12.;
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
    const PROSPERITY_CONVERGENCE: f64 = 0.05;
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
        Behavior::GoTo {
            target,
            enter_on_arrival,
        } => sim
            .agents
            .get(target)
            .and_then(|agent| sim.parties.get(agent.party))
            .map(|party| Goal::ToParty {
                target: party.id,
                distance: std::f32::NEG_INFINITY,
                enter_on_arrival,
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
        Goal::ToParty {
            target, distance, ..
        } => {
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
