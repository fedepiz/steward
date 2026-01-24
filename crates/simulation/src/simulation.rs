use bumpalo::Bump;
use slotmap::{Key, KeyData, SecondaryMap};
use util::string_pool::*;

use crate::{
    agents::{self, *},
    geom::V2,
    movement::{self, SpatialMap},
    names::{Name, NamePart, Names},
    objects::*,
    parties::{self, *},
    terrain_map::TerrainMap,
    view::{MapItems, MapTerrain},
};

// GAME PARAMETERS
pub(crate) const FARMER_OPPORTUNITY_CHANGE_PER_TICK: f64 = 1.;
pub(crate) const FARMER_OPPORTUNITY_MIN: f64 = -10000.0;
pub(crate) const FARMER_OPPORTUNITY_VISIT_CHANGE: f64 = -2000.;
pub(crate) const FARMER_OPPORTUNITY_SPAWN_CHANGE: f64 = -2000.;

pub(crate) const MINER_OPPORTUNITY_CHANGE_PER_TICK: f64 = 1.;
pub(crate) const MINER_OPPORTUNITY_MIN: f64 = -10000.0;
pub(crate) const MINER_OPPORTUNITY_SPAWN_CHANGE: f64 = MINER_OPPORTUNITY_MIN;

pub(crate) const CARAVAN_OPPORTUNITY_MIN: f64 = -10000.0;
pub(crate) const CARAVAN_OPPORTUNITY_MAX: f64 = 0.0;
pub(crate) const CARAVAN_OPPORTUNITY_SPAWN_CHANGE: f64 = CARAVAN_OPPORTUNITY_MIN;

type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;
pub(crate) type Color = (u8, u8, u8);

#[derive(Default)]
pub(crate) struct Simulation {
    pub turn_num: usize,
    pub terrain_map: TerrainMap,
    pub names: Names,
    pub parties: Parties,
    movement_cache: movement::MovementCache,
    pub agents: Agents,
    pub faction_colors: SecondaryMap<AgentId, Color>,
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

        tick_opportunities(sim);
        spawn_with_opportunity(sim, arena);

        crate::agent_tasking::agent_tasking(sim, arena);

        const ECONOMY_TICK_RATE: usize = 200;
        if sim.turn_num % ECONOMY_TICK_RATE == 0 {
            let _span = tracing::info_span!("Economic tick").entered();
            food_production_and_consumption(sim, arena);
            // Food stock sets the target population; this nudges population toward that target.
            population_from_food(sim, arena);
            // Consumables are converted into prosperity in small increments.
            consumables_into_prosperity(sim, arena);
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
            .filter(|det| det.is_location)
            .map(|det| (det.agent, det.distance)),
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

            scratch.push(Detection {
                target: target.id,
                agent: target.agent,
                is_location: target.is_location,
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

fn tick_opportunities(sim: &mut Simulation) {
    struct Rule<'a> {
        variable: Var,
        change: f64,
        min: f64,
        max: f64,
        var_gt: &'a [(Var, f64)],
    }

    const RULES: &[Rule] = &[
        Rule {
            variable: Var::FarmerOpportunity,
            change: FARMER_OPPORTUNITY_CHANGE_PER_TICK,
            min: FARMER_OPPORTUNITY_MIN,
            max: 0.,
            var_gt: &[],
        },
        Rule {
            variable: Var::MinerOpportunity,
            change: MINER_OPPORTUNITY_CHANGE_PER_TICK,
            min: MINER_OPPORTUNITY_MIN,
            max: 0.,
            var_gt: &[],
        },
        Rule {
            variable: Var::MinerOpportunity,
            change: MINER_OPPORTUNITY_CHANGE_PER_TICK * -2.,
            min: MINER_OPPORTUNITY_MIN,
            max: 0.,
            var_gt: &[(Var::Minerals, 10.)],
        },
    ];

    for agent in sim.agents.iter_mut() {
        // Only applies to settlements?
        if !agent.in_set(Set::Settlements) {
            continue;
        }
        for rule in RULES {
            let passes_thresholds = rule
                .var_gt
                .iter()
                .all(|&(var, threshold)| agent.get_var(var) >= threshold);
            if !passes_thresholds {
                continue;
            }

            let current = agent.get_var(rule.variable);
            let next = (current + rule.change).clamp(rule.min, rule.max);
            agent.vars_mut().with(rule.variable, next);
        }
    }
}

fn spawn_with_opportunity(sim: &mut Simulation, arena: &Bump) {
    let mut spawns = AVec::new_in(arena);
    let mut updates = AVec::new_in(arena);

    #[derive(Clone, Copy)]
    struct SpawnInfo {
        party_type: &'static str,
        flags: Flags,
        name_contains_of_parent: bool,
    }

    const FARMER_SPAWN_INFO: SpawnInfo = SpawnInfo {
        party_type: "farmers",
        flags: Flags::new().with(Flag::IsFarmer),
        name_contains_of_parent: false,
    };
    const MINER_SPAWN_INFO: SpawnInfo = SpawnInfo {
        party_type: "miners",
        flags: Flags::new().with(Flag::IsMiner),
        name_contains_of_parent: false,
    };
    const CARAVAN_SPAWN_INFO: SpawnInfo = SpawnInfo {
        party_type: "caravan",
        flags: Flags::new().with(Flag::IsCaravan),
        name_contains_of_parent: true,
    };

    #[derive(Clone, Copy, PartialEq, PartialOrd)]
    struct OpportunityInfo {
        variable: Var,
        threshold: f64,
        on_spawn_change: f64,
        min_value: f64,
        max_value: f64,
        workplace_set: Option<Set>,
    }

    const FARMER_OPPORTUNITY_INFO: OpportunityInfo = OpportunityInfo {
        variable: Var::FarmerOpportunity,
        threshold: 0.,
        on_spawn_change: FARMER_OPPORTUNITY_SPAWN_CHANGE,
        min_value: FARMER_OPPORTUNITY_MIN,
        max_value: 0.,
        workplace_set: None,
    };
    const MINER_OPPORTUNITY_INFO: OpportunityInfo = OpportunityInfo {
        variable: Var::MinerOpportunity,
        threshold: 0.,
        on_spawn_change: MINER_OPPORTUNITY_SPAWN_CHANGE,
        min_value: MINER_OPPORTUNITY_MIN,
        max_value: 0.,
        workplace_set: Some(Set::Mines),
    };
    const CARAVAN_OPPORTUNITY_INFO: OpportunityInfo = OpportunityInfo {
        variable: Var::CaravanOpportunity,
        threshold: CARAVAN_OPPORTUNITY_MAX,
        on_spawn_change: CARAVAN_OPPORTUNITY_SPAWN_CHANGE,
        min_value: CARAVAN_OPPORTUNITY_MIN,
        max_value: CARAVAN_OPPORTUNITY_MAX,
        workplace_set: None,
    };

    #[derive(Default)]
    struct Eligeable<'a> {
        sets: &'a [Set],
    }

    let table = [
        (
            Eligeable {
                sets: &[Set::Villages],
            },
            FARMER_OPPORTUNITY_INFO,
            FARMER_SPAWN_INFO,
        ),
        (
            Eligeable {
                sets: &[Set::Towns],
            },
            MINER_OPPORTUNITY_INFO,
            MINER_SPAWN_INFO,
        ),
        (
            Eligeable {
                sets: &[Set::Towns],
            },
            CARAVAN_OPPORTUNITY_INFO,
            CARAVAN_SPAWN_INFO,
        ),
    ];

    for agent in sim.agents.iter() {
        for (filter, opportunity, spawn) in &table {
            // Check if opportunity value is in trigger range (a cheap check)
            let opportunity_value = agent.get_var(opportunity.variable);
            if opportunity_value < opportunity.threshold {
                continue;
            }

            // Check if filter applis
            if filter.sets.iter().any(|&set| !agent.in_set(set)) {
                continue;
            }

            let mut parents = AVec::new_in(arena);

            // Look for a target workplace in the party detections
            let workplace = opportunity.workplace_set.and_then(|set| {
                let detections = sim.parties.detections.get_for(agent.party);
                detections
                    .iter()
                    .find(|entry| sim.agents.in_set(entry.agent, set))
                    .map(|entry| (Hierarchy::WorkArea, entry.agent))
            });

            if opportunity.workplace_set.is_some() && workplace.is_none() {
                continue;
            }

            parents.extend(workplace);

            // Update opportunity value
            let next_value = (opportunity_value + opportunity.on_spawn_change)
                .clamp(opportunity.min_value, opportunity.max_value);

            // Record variable change
            updates.push((agent.id, opportunity.variable, next_value));

            // Produce spawn
            let party_type = sim.parties.find_type_by_tag(spawn.party_type).unwrap();
            let mut name = party_type.name;
            if spawn.name_contains_of_parent {
                name.set(NamePart::OfX, agent.name.get(NamePart::Main));
            }
            spawns.push(SpawnSubagent {
                party_type: party_type.id,
                parent: agent.id,
                name,
                flags: spawn.flags,
                vars: &[],
                parents: parents.into_bump_slice(),
            });
        }
    }

    for spawn in spawns {
        let parent = sim.names.resolve(sim.agents[spawn.parent].name);
        let child = sim.names.resolve(sim.parties[spawn.party_type].name);
        println!("Spawning {child} at {parent}");
        spawn_subagent(sim, spawn);
    }

    for (agent_id, variable, value) in updates {
        let agent = &mut sim.agents[agent_id];
        agent.vars_mut().set(variable, value);
    }
}

struct SpawnSubagent<'a> {
    party_type: PartyTypeId,
    parent: AgentId,
    name: Name,
    flags: Flags,
    vars: &'a [(Var, f64)],
    parents: &'a [(Hierarchy, AgentId)],
}

fn spawn_subagent(sim: &mut Simulation, spawn: SpawnSubagent) {
    // Spawn agent (now ihere, later floated out)
    struct ParentData {
        party: PartyId,
        pos: V2,
        faction: AgentId,
    }

    let parent = {
        let agent = &sim.agents[spawn.parent];
        let party = &sim.parties[agent.party];
        let faction = sim.agents.parent_of(Hierarchy::FactionMembership, agent.id);
        ParentData {
            party: party.id,
            pos: party.body.pos,
            faction,
        }
    };
    let party = sim.parties.spawn_with_type(spawn.party_type);
    party.body.pos = parent.pos;
    party.inside_of = parent.party;

    let agent = sim.agents.spawn();
    agent.name = spawn.name;
    party.name = spawn.name;

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
    sim.agents
        .set_parent(Hierarchy::FactionMembership, parent.faction, agent);

    for &(hierarchy, id) in spawn.parents {
        sim.agents.set_parent(hierarchy, id, agent);
    }
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

fn consumables_into_prosperity(sim: &mut Simulation, arena: &Bump) {
    #[derive(Clone, Copy)]
    struct ConsumptionType {
        variable: Var,
        consumption_proportion: f64,
        prosperity_per_unit: f64,
    }

    const TABLE: &[ConsumptionType] = &[
        ConsumptionType {
            variable: Var::Goods,
            consumption_proportion: 0.1,
            prosperity_per_unit: 0.05 * 0.01, // This is meant as a percent
        },
        ConsumptionType {
            variable: Var::Minerals,
            consumption_proportion: 0.1,
            prosperity_per_unit: 0.05 * 0.01, // This is meant as a percent
        },
    ];

    let mut var_changes = AVec::new_in(arena);
    let mut prosperity_changes = AVec::new_in(arena);

    for settlement in sim.agents.iter_set(agents::Set::Settlements) {
        let mut prosperity_delta = 0.0;

        for entry in TABLE {
            let available = settlement.get_var(entry.variable).max(0.0);
            if available <= 0.0 {
                continue;
            }

            let consumed = (available * entry.consumption_proportion).round();
            let remaining = (available - consumed).max(0.0);
            prosperity_delta += consumed * entry.prosperity_per_unit;

            var_changes.push((settlement.id, entry.variable, remaining));
        }

        if prosperity_delta > 0.0 {
            let prosperity = settlement.get_var(Var::Prosperity);
            let next = (prosperity + prosperity_delta).clamp(0.0, 1.0);
            prosperity_changes.push((settlement.id, next));
        }
    }

    for (agent, variable, value) in var_changes {
        let agent = &mut sim.agents[agent];
        agent.vars_mut().with(variable, value);
    }

    for (agent, prosperity) in prosperity_changes {
        let agent = &mut sim.agents[agent];
        agent.vars_mut().with(Var::Prosperity, prosperity);
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
        Behavior::GoTo {
            target,
            enter_on_arrival,
        } => {
            let distance = if enter_on_arrival {
                std::f32::NEG_INFINITY
            } else {
                0.
            };
            sim.agents
                .get(target)
                .and_then(|agent| sim.parties.get(agent.party))
                .map(|party| Goal::ToParty {
                    target: party.id,
                    distance,
                    enter_on_arrival,
                })
                .unwrap_or_default()
        }
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
