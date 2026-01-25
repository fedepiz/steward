use bumpalo::Bump;
use rand::{Rng, SeedableRng, rngs::SmallRng};
use slotmap::{Key, KeyData, SecondaryMap};
use util::{span::Span, string_pool::*};

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
    pub turn_num: u64,
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
                on_arrival: OnArrival::Attack,
            };
        }
    }

    for _ in 0..request.advance_ticks {
        sim.turn_num = sim.turn_num.wrapping_add(1);

        tick_opportunities(sim);
        spawn_with_opportunity(sim, arena);

        crate::agent_tasking::agent_tasking(sim, arena);

        const ECONOMY_TICK_RATE: u64 = 200;
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

        const ACTIVITY_TICK_RATE: u64 = 100;
        if sim.turn_num % ACTIVITY_TICK_RATE == 0 {
            let _span = tracing::info_span!("Activity tick").entered();
            tick_activities(arena, sim);
        }

        let mut movement_elements = AVec::with_capacity_in(sim.parties.len(), arena);

        let mut desire_exit = AVec::with_capacity_in(sim.parties.len(), arena);
        let mut desire_entry = AVec::with_capacity_in(sim.parties.len(), arena);

        let mut activity_start_intent = AVec::with_capacity_in(sim.parties.len(), arena);

        // Collect agent facts
        let facts = &self::collect_facts(arena, &sim.agents);

        // Parties logical update
        for party in sim.parties.iter() {
            let (goal, avoid_area) = determine_party_goal_and_avoidance(sim, party);

            let detection = sim.parties.detections.get_for(party.id);

            let movement_target = determine_party_movement_target(party, detection, goal);

            {
                // Enqueue desire to enter a container
                let target = match goal {
                    Goal::Idle => party.inside_of,
                    Goal::ToParty {
                        target,
                        on_arrival: OnArrival::Enter,
                        ..
                    } => target,
                    _ => PartyId::null(),
                };

                if target != party.inside_of {
                    if target.is_null() {
                        desire_exit.push(party.id);
                    } else {
                        desire_entry.push((party.id, target, true));
                    }
                }
            }

            {
                // Enqueue desire to begin a battle
                match goal {
                    Goal::ToParty {
                        target,
                        on_arrival: OnArrival::Attack,
                        ..
                    } => {
                        activity_start_intent.push((ActivityType::Battle, party.id, target));
                    }
                    _ => {}
                }
            }

            // Resolve movement target
            let (destination, direct) = match movement_target {
                MovementTarget::Immobile => (party.body.pos, false),
                MovementTarget::FixedPos { pos, direct } => (pos, direct),
                MovementTarget::Party(id) => (sim.parties[id].body.pos, false),
            };

            // If the party is inside another party, their speed is 0
            let speed = if party.inside_of.is_null() {
                party.speed
            } else {
                0.
            };

            // Actual movement element
            movement_elements.push(movement::Element {
                id: party.id,
                speed,
                pos: party.body.pos,
                destination,
                direct,
                avoid_area,
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
            self::detections(&mut detections, &sim.parties, spatial_map, facts);
            sim.parties.detections = detections;
        }

        // Iterate writeback
        {
            let _span = tracing::info_span!("Update positions").entered();
            let mut positions = movement_result.positions.into_iter();

            for party in sim.parties.iter_mut() {
                let (id, pos) = positions.next().unwrap();
                assert!(id == party.id);
                party.body.pos = pos;
            }
        }

        // Resolve desired beging of battle
        {
            let _span = tracing::info_span!("Post Move State Changes").entered();
            resolve_activity_starts(sim, &activity_start_intent);

            // Resolve desired changes of party container
            for party in desire_exit {
                sim.parties[party].inside_of = PartyId::null();
            }

            for (party, target, check) in desire_entry {
                let allowed = {
                    let party = &sim.parties[party];
                    let target = &sim.parties[target];
                    !check || party.body.pos == target.body.pos
                };

                sim.parties[party].inside_of = if allowed { target } else { PartyId::null() };
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
                    let location = location_of_agent(sim, agent, facts, &mut scratch);
                    (agent.id, location)
                })
                .collect();

            // Writeback
            for (id, location) in output {
                sim.agents[id].location = location;
            }
        }

        sim.parties.garbage_collect();
    }

    let mut response = Response::default();
    crate::view::view(sim, &request, &mut response);
    response
}

fn location_of_agent(
    sim: &Simulation,
    agent: &Agent,
    facts: &Facts,
    scratch: &mut Vec<(AgentId, f32)>,
) -> Location {
    // Disembodied agent has no location
    if agent.party.is_null() {
        return Location::Nowhere;
    }

    let facts = facts.get_for(agent.id);

    // If the agent is a settlement, then it's at a settlement!
    let is_location = facts.any_with_kind(FactKind::IsLocation);

    if is_location {
        return Location::Proximate(agent.id);
    }

    let party = &sim.parties[agent.party];
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

fn detections(
    detections: &mut Detections,
    parties: &Parties,
    spatial_map: &SpatialMap,
    facts: &Facts,
) {
    detections.clear();

    let mut scratch = Vec::with_capacity(100);
    for party in parties.iter() {
        const DETECTION_RANGE: f32 = 20.;
        let neighbours = spatial_map.search(party.body.pos, DETECTION_RANGE);
        for target in neighbours {
            // Do not look at yourself
            if target == party.id {
                continue;
            }
            let target = &parties[target];
            // Distance net of sizes. 0 or lower means collision
            let distance = body_distance(party.body, target.body);

            let facts = facts.get_for(target.agent);
            let is_location = facts.any_with_kind(FactKind::IsLocation);
            let threat = calculate_power(&facts);

            scratch.push(Detection {
                target: target.id,
                agent: target.agent,
                is_location,
                distance,
                threat,
            });
        }
        detections.set(party.id, &scratch);
        scratch.clear();
    }
}

fn body_distance(b1: Body, b2: Body) -> f32 {
    let center_to_center_distance = V2::distance(b1.pos, b2.pos);
    let collision_range = (b1.size + b2.size) / 2.;

    // Distance net of sizes. 0 or lower means collision
    center_to_center_distance - collision_range
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

fn determine_party_goal_and_avoidance(sim: &Simulation, party: &Party) -> (Goal, movement::Area) {
    if party.speed == 0.0 {
        return Default::default();
    }

    let agent = match sim.agents.get(party.agent) {
        Some(agent) => agent,
        None => return Default::default(),
    };

    if agent.flags.get(Flag::IsActivityPartecipant) {
        return Default::default();
    }

    let goal = match agent.behavior {
        Behavior::Idle => Goal::Idle,
        Behavior::Player => sim.player_party_goal,
        Behavior::GoTo { target, on_arrival } => {
            let must_get_same_position = match on_arrival {
                OnArrival::Enter => true,
                OnArrival::Nothing => false,
                OnArrival::Attack => false,
            };

            let distance = if must_get_same_position {
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
                    on_arrival,
                })
                .unwrap_or_default()
        }
    };

    // Inner avoid range is the range of actual avoidance
    const INNER_AVOID_RANGE: f32 = 6.;
    // Outer avoid range is the range of consideration
    const OUTER_AVOID_RANGE: f32 = INNER_AVOID_RANGE * 1.2;

    // Do we detect a threat?
    let detections = sim.parties.detections.get_for(party.id);
    let primary_threat = detections
        .iter()
        .filter(|det| det.threat > 0. && det.distance <= OUTER_AVOID_RANGE)
        .min_by_key(|det| (det.distance * 10.).round().max(0.) as u32);

    let avoidance = primary_threat
        .map(|det| {
            let pos = sim.parties[det.target].body.pos;
            movement::Area {
                pos,
                range: INNER_AVOID_RANGE,
            }
        })
        .unwrap_or_default();

    (goal, avoidance)
}

pub(crate) fn determine_party_movement_target(
    _: &Party,
    detections: &[Detection],
    goal: Goal,
) -> MovementTarget {
    match goal {
        Goal::Idle => MovementTarget::Immobile,
        Goal::MoveTo(pos) => MovementTarget::FixedPos { pos, direct: false },
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

pub(crate) struct Facts<'a> {
    buffer: AVec<'a, Fact>,
    by_agent: SecondaryMap<AgentId, Span>,
}

impl<'a> Facts<'a> {
    fn new(arena: &'a Bump, capacity: usize) -> Self {
        Self {
            buffer: AVec::new_in(arena),
            by_agent: SecondaryMap::with_capacity(capacity),
        }
    }

    fn insert(&mut self, id: AgentId, facts: impl IntoIterator<Item = Fact>) {
        let start = self.buffer.len();
        self.buffer.extend(facts);
        let end = self.buffer.len();
        let span = Span::between(start, end);
        self.by_agent.insert(id, span);
    }

    fn get_for(&self, id: AgentId) -> AgentFacts<'_> {
        let span = self.by_agent.get(id).copied().unwrap_or_default();
        AgentFacts(span.view(&self.buffer))
    }
}

#[derive(Clone, Copy)]
struct AgentFacts<'a>(&'a [Fact]);

impl AgentFacts<'_> {
    fn any_with_kind(&self, kind: FactKind) -> bool {
        self.0.iter().any(|fact| fact.kind == kind)
    }

    fn accumulate_with_kind(&self, kinds: &[(FactKind, f32)]) -> f32 {
        let mut sum = 0.;
        for fact in self.0 {
            for &(tgt, accum) in kinds {
                if fact.kind == tgt {
                    sum += accum;
                }
            }
        }
        sum
    }
}

#[derive(Clone, Copy, Default)]
struct Fact {
    kind: FactKind,
}

impl From<FactKind> for Fact {
    fn from(kind: FactKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum FactKind {
    Unknown,
    IsLocation,
    IsPlayer,
}

impl Default for FactKind {
    fn default() -> Self {
        Self::Unknown
    }
}

fn collect_facts<'a>(arena: &'a Bump, agents: &Agents) -> Facts<'a> {
    let _span = tracing::info_span!("Collect Facts").entered();
    let mut out = Facts::new(arena, agents.capacity());
    let mut scratch = AVec::new_in(arena);
    for agent in agents.iter() {
        if agent.in_set(Set::Locations) {
            scratch.push(Fact::from(FactKind::IsLocation));
        }
        if agent.is_player {
            scratch.push(Fact::from(FactKind::IsPlayer));
        }

        out.insert(agent.id, scratch.drain(..));
    }
    out
}

fn calculate_power(facts: &AgentFacts) -> f32 {
    facts.accumulate_with_kind(&[(FactKind::IsPlayer, 10.)])
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ActivityType {
    Battle,
}

fn resolve_activity_starts<'a>(
    sim: &mut Simulation,
    incipits: &[(ActivityType, PartyId, PartyId)],
) {
    for &(activity_type, subject, target) in incipits {
        let (
            activity_party_type,
            activity_set,
            participant_flag,
            subject_hierarchy,
            target_hierarchy,
        ) = match activity_type {
            ActivityType::Battle => (
                "battle",
                Set::Battle,
                Flag::IsActivityPartecipant,
                Hierarchy::ActivitySubject,
                Hierarchy::ActivityTarget,
            ),
        };

        let collides = sim
            .parties
            .detections
            .get_for(subject)
            .iter()
            .any(|entry| entry.target == target && entry.distance <= 0.0);

        if !collides {
            continue;
        }

        // Get information necessary to spawn the battle
        let pos = {
            let subject = &sim.parties[subject];
            let target = &sim.parties[target];
            // An activitity start only works if both subject and target are not in an activity
            let already_in_activity = [subject.agent, target.agent]
                .iter()
                .any(|&id| sim.agents[id].flags.get(Flag::IsActivityPartecipant));
            if already_in_activity {
                continue;
            }
            (subject.body.pos + target.body.pos) / 2.
        };

        // Spawn the battle
        let activity_agent = {
            let party_type = sim.parties.find_type_by_tag(activity_party_type).unwrap();
            let agent = sim.agents.spawn();
            let party = sim.parties.spawn_with_type(party_type.id);
            agent.name = party.name;
            party.agent = agent.id;
            party.body.pos = pos;
            agent.party = party.id;
            let agent = agent.id;
            sim.agents.add_to_set(activity_set, agent);
            agent
        };

        for (id, hierarchy) in [(subject, subject_hierarchy), (target, target_hierarchy)] {
            let agent = sim.parties[id].agent;
            sim.agents.set_parent(hierarchy, activity_agent, agent);
            sim.agents[agent].flags.set(participant_flag, true);
        }
    }
}

fn tick_activities(arena: &Bump, sim: &mut Simulation) {
    let activities = AVec::from_iter_in(sim.agents.iter_set_ids(Set::Battle), arena);
    let mut partecipants = AVec::new_in(arena);
    for activity_id in activities {
        // Get the members out
        for hierarchy in [Hierarchy::ActivitySubject, Hierarchy::ActivityTarget] {
            partecipants.extend(
                sim.agents
                    .children_of(hierarchy, activity_id)
                    .map(|id| (id, hierarchy)),
            );
        }

        // Make decisions
        let rng = &mut rng_from_multi_seed(&[sim.turn_num, activity_id.data().as_ffi()]);
        let is_attacker_victory = rng.gen_bool(0.5);
        let winning_role = if is_attacker_victory {
            Hierarchy::ActivitySubject
        } else {
            Hierarchy::ActivityTarget
        };

        // Despawn the activity
        let activity = sim.agents.despawn(activity_id).unwrap();
        // Set the party to have no agent
        sim.parties[activity.party].agent = AgentId::null();

        // Process partecipants
        for (agent, role) in partecipants.drain(..) {
            let agent = &mut sim.agents[agent];
            agent.flags.set(Flag::IsActivityPartecipant, false);
            if role == winning_role {
                println!("{} is a winner", sim.names.resolve(agent.name));
            }
        }
    }
}

fn rng_from_multi_seed(seeds: &[u64]) -> SmallRng {
    let mut seed = 0u64;
    for &i in seeds {
        seed = seed.wrapping_mul(13).wrapping_add(i).wrapping_mul(17);
    }
    SmallRng::seed_from_u64(seed)
}
