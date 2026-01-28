use bumpalo::Bump;
use rand::{Rng, SeedableRng, rngs::SmallRng};
use slotmap::{Key, KeyData, SecondaryMap};
use strum::IntoEnumIterator;
use util::{span::Span, string_pool::*};

use crate::{
    entities::{self, *},
    geom::V2,
    interaction::Interactions,
    movement::{self, SpatialMap},
    names::{Name, NamePart, Names},
    objects::*,
    simulation::detection::{Detection, Detections},
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
    movement_cache: movement::MovementCache,
    pub entities: Entities,
    pub faction_colors: SecondaryMap<EntityId, Color>,
    player_goal: Goal,
    detections: Detections,
    pub(crate) interactions: Interactions,
}

impl Simulation {
    pub(crate) fn tick(&mut self, request: Request, arena: &Bump) -> Response {
        let _span = tracing::info_span!("Tick").entered();
        tick(self, request, arena)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum OnArrival {
    Nothing,
    Enter,
    Attack,
    Interact,
}

impl Default for OnArrival {
    fn default() -> Self {
        Self::Nothing
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum MovementTarget {
    Immobile,
    FixedPos { pos: V2, direct: bool },
    Entity(EntityId),
}

impl Default for MovementTarget {
    fn default() -> Self {
        Self::Immobile
    }
}

fn tick(sim: &mut Simulation, mut request: Request, arena: &Bump) -> Response {
    if let Some(req) = request.init.take() {
        *sim = Simulation::default();
        sim.interactions = Interactions::new();
        crate::init::init(sim, req);
    }

    let process_input = !sim.interactions.has_interaction();
    let mut num_ticks = request.advance_ticks;

    if sim.interactions.has_interaction() {
        num_ticks = 0;
    }

    if process_input {
        // Input handling
        {
            // From move pos
            if let Some((x, y)) = request.move_to_pos.take() {
                sim.player_goal = Goal::MoveTo(V2::new(x, y));
            }

            // From move to target
            if let Some(item_id) = request.move_to_item.take() {
                let target = EntityId::from(KeyData::from_ffi(item_id.0));
                sim.player_goal = Goal::ToEntity {
                    target,
                    distance: 0.,
                    on_arrival: OnArrival::Interact,
                };
            }
        }
    }

    for _ in 0..num_ticks {
        sim.turn_num = sim.turn_num.wrapping_add(1);

        // Tick timers
        tick_timers(sim);
        tick_opportunities(sim);
        spawn_with_opportunity(sim, arena);

        crate::tasking::entity_tasking(sim, arena);

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
            // Recruiting for settlements that do that
            increase_recruits(sim);
        }

        const ACTIVITY_TICK_RATE: u64 = 100;
        if sim.turn_num % ACTIVITY_TICK_RATE == 0 {
            let _span = tracing::info_span!("Activity tick").entered();
            activities::tick_activities(arena, sim);
        }

        {
            const HEADCOUNT_VARS: &[Var] = &[Var::Civilians, Var::Soldiers];
            let iter = sim
                .entities
                .iter()
                .filter(|entity| {
                    entity.get_flag(Flag::IsEphemeral)
                        && HEADCOUNT_VARS.iter().all(|&var| entity.get_var(var) <= 0.)
                })
                .map(|entity| entity.id);

            for id in AVec::from_iter_in(iter, arena) {
                sim.entities.despawn(id);
            }
        }

        let mut movement_elements = AVec::with_capacity_in(sim.entities.len(), arena);

        let mut desire_exit = AVec::with_capacity_in(sim.entities.len(), arena);
        let mut desire_entry = AVec::with_capacity_in(sim.entities.len(), arena);

        let mut activity_start_intent = AVec::with_capacity_in(sim.entities.len(), arena);

        let mut interaction_start_intent: Option<(EntityId, EntityId)> = None;

        let diplo_map = DiploMap::calculate(arena, sim);

        // Parties logical update
        for subject in sim.entities.iter() {
            #[derive(Default)]
            struct Intent {
                movement_target: MovementTarget,
                avoid_area: movement::Area,
                target: EntityId,
                desire_exit: bool,
                desire_entry: bool,
                start_activity: Option<activities::Type>,
                start_interaction: bool,
            }

            let mut intent = Intent::default();
            {
                let (goal, avoid_area) =
                    determine_party_goal_and_avoidance(sim, &diplo_map, subject);

                let detection = sim.detections.get_for(subject.id);

                let movement_target = determine_party_movement_target(detection, goal);

                intent.movement_target = movement_target;
                intent.avoid_area = avoid_area;

                {
                    let container = sim.entities.parent_of(Hierarchy::Container, subject.id);
                    // Enqueue desire to enter a container
                    let target = match goal {
                        Goal::Idle => container,
                        Goal::ToEntity {
                            target,
                            on_arrival: OnArrival::Enter,
                            ..
                        } => target,
                        _ => EntityId::null(),
                    };

                    if target != container {
                        if target.is_null() {
                            intent.desire_exit = true;
                        } else {
                            intent.desire_entry = true;
                        }
                    }
                    intent.target = target;
                }

                {
                    // Enqueue desire to begin a battle
                    match goal {
                        Goal::ToEntity {
                            target, on_arrival, ..
                        } => {
                            intent.target = target;
                            match on_arrival {
                                OnArrival::Attack => {
                                    intent.start_activity = Some(activities::Type::Battle);
                                }
                                OnArrival::Interact => {
                                    intent.start_interaction = true;
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            };

            let is_frozen = subject.get_var(Var::FrozenTimer) > 0.;
            if is_frozen {
                intent = Intent::default();
            }

            // Resolve movement target
            let (destination, direct) = match intent.movement_target {
                MovementTarget::Immobile => (subject.body.pos, false),
                MovementTarget::FixedPos { pos, direct } => (pos, direct),
                MovementTarget::Entity(id) => (
                    sim.entities
                        .get(id)
                        .map(|entity| entity.body.pos)
                        .unwrap_or(subject.body.pos),
                    false,
                ),
            };

            // If the party is inside another party, their speed is 0
            let speed = if !subject.get_flag(Flag::IsInside) {
                subject.speed
            } else {
                0.
            };

            // Actual movement element
            let element = movement::Element {
                id: subject.id,
                speed,
                pos: subject.body.pos,
                destination,
                direct,
                avoid_area: intent.avoid_area,
            };

            if intent.desire_exit {
                desire_exit.push(subject.id);
            }

            if !intent.target.is_null() {
                if intent.desire_entry {
                    desire_entry.push((subject.id, intent.target, true));
                }
                if let Some(activity_type) = intent.start_activity {
                    activity_start_intent.push((activity_type, subject.id, intent.target));
                }
            }

            if intent.start_interaction {
                // Only players begin interactions
                assert!(subject.is_player);
                interaction_start_intent = Some((subject.id, intent.target));
            }

            movement_elements.push(element);
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
            let mut detections = std::mem::take(&mut sim.detections);
            self::detection::calculate(&mut detections, sim, spatial_map, &diplo_map);
            sim.detections = detections;
        }

        // Iterate writeback
        {
            let _span = tracing::info_span!("Update positions").entered();
            let positions = movement_result.positions.into_iter();

            for (id, pos) in positions {
                sim.entities[id].body.pos = pos;
            }
        }

        {
            // Resolve desired beging of battle
            let _span = tracing::info_span!("Post Move State Changes").entered();
            activities::begin_activities(sim, &activity_start_intent);

            // Resolve desired changes of party container
            for subject in desire_exit {
                sim.entities.remove_parent(Hierarchy::Container, subject);
                sim.entities[subject].flags.set(Flag::IsInside, false);
            }

            for (subject, target, check) in desire_entry {
                let allowed = {
                    let subject = &sim.entities[subject];
                    let target = &sim.entities[target];
                    !check || subject.body.pos == target.body.pos
                };

                let container = if allowed { target } else { EntityId::null() };

                sim.entities
                    .set_parent(Hierarchy::Container, container, subject);
                sim.entities[subject].flags.set(Flag::IsInside, allowed);
            }
        }

        // Resolve interacton beginning
        if let Some((initiator, receiver)) = interaction_start_intent {
            // Check for collision
            if sim.detections.collides(initiator, receiver) {
                let mut interactions = std::mem::take(&mut sim.interactions);
                // We flip the order, as the initiator is usually the player..
                interactions.begin_interaction(arena, sim, receiver, initiator);
                sim.interactions = interactions;
                // This is started by the player, so let's clear their goal
                sim.player_goal = Goal::Idle;
            }
        }

        // Determine entity location
        {
            let _span = tracing::info_span!("Entity Locations").entered();
            let mut scratch = vec![];

            let output: Vec<_> = sim
                .entities
                .iter()
                .map(|entity| {
                    let location = location_of_entity(sim, entity, &mut scratch);
                    (entity.id, location)
                })
                .collect();

            // Writeback
            for (id, location) in output {
                sim.entities[id].location = location;
            }
        }

        sim.entities.garbage_collect();
    }
    let mut response = Response::default();
    crate::view::view(sim, &request, &mut response);
    response
}

fn location_of_entity(
    sim: &Simulation,
    entity: &Entity,
    scratch: &mut Vec<(EntityId, f32)>,
) -> Location {
    // Disembodied entity has no location
    if entity.get_flag(Flag::IsDisembodied) {
        return Location::Nowhere;
    }

    // If the entity is a location, then it's at a settlement!
    if entity.in_set(Set::Locations) {
        return Location::Proximate(entity.id);
    }

    // If an entity's party is inside another party, that the entity party is at the location
    if entity.get_flag(Flag::IsInside) {
        let container = sim.entities.parent_of(Hierarchy::Container, entity.id);
        assert!(!container.is_null());
        return Location::Inside(container);
    }

    // Otherwise, look at the detections, collecting into scratch space
    let detections = sim.detections.get_for(entity.id);

    scratch.clear();

    const NEAR_DISTANCE: f32 = 10.;

    // Only consider parties in the target set
    scratch.extend(
        detections
            .iter()
            .filter(|det| det.is_location && det.distance <= NEAR_DISTANCE)
            .map(|det| (det.id, det.distance)),
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
    pub highlighted_item: MapItemId,
    pub extract_terrain: bool,
    pub(crate) strings: StringPool,
    pub(crate) view_entities: Vec<ViewEntity>,
}

impl Request {
    pub(crate) fn entity_tag(&self, view: ViewEntity) -> &'_ str {
        self.strings.get(view.tag)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MapItemId(pub u64);

impl Default for MapItemId {
    fn default() -> Self {
        Self(EntityId::default().data().as_ffi())
    }
}

#[derive(Clone, Copy)]
pub(crate) struct ViewEntity {
    tag: SpanHandle,
    pub(crate) entity: EntityId,
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
        let entity = EntityId::from(KeyData::from_ffi(id.0));
        self.view_entity(key, entity);
    }

    fn view_entity(&mut self, key: &str, entity: EntityId) {
        let key = self.strings.push_str(key);
        self.view_entities.push(ViewEntity { tag: key, entity });
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
        var_gte: &'a [(Var, f64)],
    }

    const RULES: &[Rule] = &[
        Rule {
            variable: Var::FarmerOpportunity,
            change: FARMER_OPPORTUNITY_CHANGE_PER_TICK,
            min: FARMER_OPPORTUNITY_MIN,
            max: 0.,
            var_gte: &[],
        },
        Rule {
            variable: Var::MinerOpportunity,
            change: MINER_OPPORTUNITY_CHANGE_PER_TICK,
            min: MINER_OPPORTUNITY_MIN,
            max: 0.,
            var_gte: &[],
        },
        Rule {
            variable: Var::MinerOpportunity,
            change: MINER_OPPORTUNITY_CHANGE_PER_TICK * -2.,
            min: MINER_OPPORTUNITY_MIN,
            max: 0.,
            var_gte: &[(Var::Minerals, 10.)],
        },
    ];

    for entity in sim.entities.iter_mut() {
        // Only applies to settlements?
        if !entity.in_set(Set::Settlements) {
            continue;
        }
        for rule in RULES {
            let passes_thresholds = rule
                .var_gte
                .iter()
                .all(|&(var, threshold)| entity.get_var(var) >= threshold);
            if !passes_thresholds {
                continue;
            }

            let current = entity.get_var(rule.variable);
            let next = (current + rule.change).clamp(rule.min, rule.max);
            entity.vars_mut().with(rule.variable, next);
        }
    }
}

fn spawn_with_opportunity(sim: &mut Simulation, arena: &Bump) {
    let mut spawns = AVec::new_in(arena);
    let mut updates = AVec::new_in(arena);

    #[derive(Clone, Copy)]
    struct SpawnInfo<'a> {
        party_type: &'a str,
        flags: Flags,
        name_contains_of_parent: bool,
        vars: &'a [(Var, f64)],
    }

    const FARMER_SPAWN_INFO: SpawnInfo = SpawnInfo {
        party_type: "farmers",
        flags: Flags::new().with(Flag::IsFarmer),
        name_contains_of_parent: false,
        vars: &[(Var::Civilians, 10.)],
    };
    const MINER_SPAWN_INFO: SpawnInfo = SpawnInfo {
        party_type: "miners",
        flags: Flags::new().with(Flag::IsMiner),
        name_contains_of_parent: false,
        vars: &[(Var::Civilians, 10.)],
    };

    const CARAVAN_SPAWN_INFO: SpawnInfo = SpawnInfo {
        party_type: "caravan",
        flags: Flags::new().with(Flag::IsCaravan),
        name_contains_of_parent: true,
        vars: &[(Var::Civilians, 10.), (Var::Soldiers, 20.)],
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

    for entity in sim.entities.iter() {
        for (filter, opportunity, spawn) in &table {
            // Check if opportunity value is in trigger range (a cheap check)
            let opportunity_value = entity.get_var(opportunity.variable);
            if opportunity_value < opportunity.threshold {
                continue;
            }

            // Check if filter applis
            if filter.sets.iter().any(|&set| !entity.in_set(set)) {
                continue;
            }

            let mut parents = AVec::new_in(arena);

            // Look for a target workplace in the party detections
            let workplace = opportunity.workplace_set.and_then(|set| {
                let detections = sim.detections.get_for(entity.id);
                detections
                    .iter()
                    .find(|entry| sim.entities.in_set(entry.id, set))
                    .map(|entry| (Hierarchy::WorkArea, entry.id))
            });

            if opportunity.workplace_set.is_some() && workplace.is_none() {
                continue;
            }

            parents.extend(workplace);

            // Update opportunity value
            let next_value = (opportunity_value + opportunity.on_spawn_change)
                .clamp(opportunity.min_value, opportunity.max_value);

            // Record variable change
            updates.push((entity.id, opportunity.variable, next_value));

            // Produce spawn
            let party_type = sim.entities.find_type_by_tag(spawn.party_type).unwrap();
            let mut name = party_type.name;
            if spawn.name_contains_of_parent {
                name.set(NamePart::OfX, entity.name.get(NamePart::Main));
            }
            let flags = spawn.flags.with(Flag::IsEphemeral);
            spawns.push(SpawnSubentity {
                typ: party_type.id,
                parent: entity.id,
                name,
                flags,
                vars: spawn.vars,
                parents: parents.into_bump_slice(),
            });
        }
    }

    for spawn in spawns {
        let parent = sim.names.resolve(sim.entities[spawn.parent].name);
        let child = sim.names.resolve(sim.entities[spawn.typ].name);
        println!("Spawning {child} at {parent}");
        spawn_subentity(sim, spawn);
    }

    for (entity_id, variable, value) in updates {
        let entity = &mut sim.entities[entity_id];
        entity.vars_mut().set(variable, value);
    }
}

struct SpawnSubentity<'a> {
    typ: ArchetypeId,
    parent: EntityId,
    name: Name,
    flags: Flags,
    vars: &'a [(Var, f64)],
    parents: &'a [(Hierarchy, EntityId)],
}

fn spawn_subentity(sim: &mut Simulation, spawn: SpawnSubentity) {
    // Spawn entity (now ihere, later floated out)
    struct ParentData {
        pos: V2,
        faction: EntityId,
    }

    let parent = {
        let entity = &sim.entities[spawn.parent];
        let faction = sim
            .entities
            .parent_of(Hierarchy::FactionMembership, entity.id);
        ParentData {
            pos: entity.body.pos,
            faction,
        }
    };
    let child = sim.entities.spawn_with_type(spawn.typ);
    child.name = spawn.name;

    child.body.pos = parent.pos;

    child.location = Location::Inside(spawn.parent);
    child.flags = spawn.flags;
    child.flags.set(Flag::IsInside, true);

    child.vars_mut().set_many(spawn.vars);

    let child = child.id;
    sim.entities
        .set_parent(Hierarchy::Attachment, spawn.parent, child);
    sim.entities
        .set_parent(Hierarchy::FactionMembership, parent.faction, child);
    sim.entities
        .set_parent(Hierarchy::Container, spawn.parent, child);

    for &(hierarchy, id) in spawn.parents {
        sim.entities.set_parent(hierarchy, id, child);
    }
}

// Units of food consumed per unit population per economic tick.
const FOOD_CONSUMPTION_PER_HEAD: f64 = 0.01;

fn food_production_and_consumption(sim: &mut Simulation, arena: &Bump) {
    // Prosperity scales the base food output from villages and towns.
    const BASE_FOOD_PRODUCTION: f64 = 100.0;
    const PROSPERITY_IMPACT_ON_FOOD_PRODUCTION: f64 = 0.1;
    let mut changes = AVec::new_in(arena);
    for settlement in sim.entities.iter_set(entities::Set::Settlements) {
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

    for (entity, food) in changes {
        let entity = &mut sim.entities[entity];
        entity.vars_mut().with(Var::FoodStored, food);
    }
}

fn population_from_food(sim: &mut Simulation, arena: &Bump) {
    // Convert food stock into a target population and converge gradually.
    const POPULATION_CONVERGENCE: f64 = 0.01;
    // The target is some kind of multiple of the food consumption
    const TICK_OF_SLACK: f64 = 12.;
    let mut changes = AVec::new_in(arena);

    for settlement in sim.entities.iter_set(entities::Set::Settlements) {
        let food = settlement.get_var(Var::FoodStored);
        let population = settlement.get_var(Var::Population);
        let target = (food / (FOOD_CONSUMPTION_PER_HEAD * TICK_OF_SLACK)).max(0.0);
        let next = population + (target - population) * POPULATION_CONVERGENCE;
        changes.push((settlement.id, next.max(0.0).round()));
    }

    for (entity, population) in changes {
        let entity = &mut sim.entities[entity];
        entity.vars_mut().with(Var::Population, population);
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

    for settlement in sim.entities.iter_set(entities::Set::Settlements) {
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

    for (entity, variable, value) in var_changes {
        let entity = &mut sim.entities[entity];
        entity.vars_mut().with(variable, value);
    }

    for (entity, prosperity) in prosperity_changes {
        let entity = &mut sim.entities[entity];
        entity.vars_mut().with(Var::Prosperity, prosperity);
    }
}

fn prosperity_towards_equilibrium(sim: &mut Simulation, arena: &Bump) {
    // Simple baseline drift so prosperity doesn't stick forever at extremes.
    const PROSPERITY_EQUILIBRIUM: f64 = 0.5;
    const PROSPERITY_CONVERGENCE: f64 = 0.05;
    let mut changes = AVec::new_in(arena);

    for settlement in sim.entities.iter_set(entities::Set::Settlements) {
        let prosperity = settlement.get_var(Var::Prosperity);
        let next = prosperity + (PROSPERITY_EQUILIBRIUM - prosperity) * PROSPERITY_CONVERGENCE;
        changes.push((settlement.id, next.clamp(0.0, 1.0)));
    }

    for (entity, prosperity) in changes {
        let entity = &mut sim.entities[entity];
        entity.vars_mut().with(Var::Prosperity, prosperity);
    }
}

fn determine_party_goal_and_avoidance(
    sim: &Simulation,
    diplo_map: &DiploMap,
    entity: &Entity,
) -> (Goal, movement::Area) {
    if entity.speed == 0.0 {
        return Default::default();
    }

    if entity.flags.get(Flag::IsActivityPartecipant) {
        return Default::default();
    }

    let mut goal = match entity.behavior {
        Behavior::Idle => Goal::Idle,
        Behavior::ToPos(pos) => Goal::MoveTo(pos),
        Behavior::Player => sim.player_goal,
        Behavior::ToEntity { target, on_arrival } => {
            let must_get_same_position = match on_arrival {
                OnArrival::Enter => true,
                OnArrival::Nothing => false,
                OnArrival::Attack => false,
                OnArrival::Interact => false,
            };

            let distance = if must_get_same_position {
                std::f32::NEG_INFINITY
            } else {
                0.
            };

            sim.entities
                .get(target)
                .map(|target| Goal::ToEntity {
                    target: target.id,
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

    let is_player = matches!(entity.behavior, Behavior::Player);
    if is_player {
        return (goal, movement::Area::default());
    }

    // Do we detect a threat?
    let detections = sim.detections.get_for(entity.id);
    let own_power = calculate_power(entity);
    let courage = entity.get_var(Var::Courage).max(0.0);
    let aggressiveness = entity.get_var(Var::Aggressiveness).max(0.0);

    let mut primary_threat = EntityId::null();
    let mut primary_target = EntityId::null();
    let mut primary_threat_distance = f32::INFINITY;
    let mut primary_target_distance = f32::INFINITY;

    for det in detections {
        if det.distance > OUTER_AVOID_RANGE {
            continue;
        }

        if det.threat > 0. && det.threat > own_power * courage {
            if det.distance < primary_threat_distance {
                primary_threat_distance = det.distance;
                primary_threat = det.id;
            }
        }

        if !det.is_location
            && diplo_map.is_hostile(entity.id, det.id)
            && own_power * aggressiveness > calculate_power(&sim.entities[det.id])
            && det.distance < primary_target_distance
        {
            primary_target_distance = det.distance;
            primary_target = det.id;
        }
    }

    let avoidance = if !primary_threat.is_null() {
        let pos = sim.entities[primary_threat].body.pos;
        movement::Area {
            pos,
            range: INNER_AVOID_RANGE,
        }
    } else {
        movement::Area::default()
    };

    let is_fleeing = !primary_threat.is_null();

    if !is_fleeing && !primary_target.is_null() {
        goal = Goal::ToEntity {
            target: primary_target,
            distance: 0.,
            on_arrival: OnArrival::Attack,
        };
    }

    (goal, avoidance)
}

fn determine_party_movement_target(detections: &[Detection], goal: Goal) -> MovementTarget {
    match goal {
        Goal::Idle => MovementTarget::Immobile,
        Goal::MoveTo(pos) => MovementTarget::FixedPos { pos, direct: false },
        Goal::ToEntity {
            target, distance, ..
        } => {
            let close_to_target = detections
                .iter()
                .find(|d| target == d.id)
                .map(|d| d.distance <= distance)
                .unwrap_or(false);

            if !close_to_target {
                MovementTarget::Entity(target)
            } else {
                MovementTarget::Immobile
            }
        }
    }
}

fn calculate_power(entity: &Entity) -> f64 {
    entity.get_var(Var::Soldiers)
}

mod activities {
    use strum::{EnumCount, EnumIter};

    use super::*;

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub(crate) enum Type {
        Battle,
    }

    pub(crate) enum Resolution {
        Ongoing,
        Resolved { winning_role: Option<Hierarchy> },
    }

    pub(crate) fn begin_activities<'a>(
        sim: &mut Simulation,
        incipits: &[(Type, EntityId, EntityId)],
    ) {
        for &(activity_type, subject, target) in incipits {
            let (
                activity_party_type,
                activity_set,
                participant_flag,
                subject_hierarchy,
                target_hierarchy,
            ) = match activity_type {
                Type::Battle => (
                    "battle",
                    Set::Battle,
                    Flag::IsActivityPartecipant,
                    Hierarchy::ActivitySubject,
                    Hierarchy::ActivityTarget,
                ),
            };

            let collides = sim.detections.collides(subject, target);

            if !collides {
                continue;
            }

            // Get information necessary to spawn the battle
            let pos = {
                let subject = &sim.entities[subject];
                let target = &sim.entities[target];
                // An activitity start only works if both subject and target are not in an activity
                let already_in_activity = [subject, target]
                    .iter()
                    .any(|entity| entity.flags.get(Flag::IsActivityPartecipant));
                if already_in_activity {
                    continue;
                }
                (subject.body.pos + target.body.pos) / 2.
            };

            // Spawn the battle
            let activity_entity = {
                let archetype = sim.entities.find_type_by_tag(activity_party_type).unwrap();
                let entity = sim.entities.spawn_with_type(archetype.id);
                entity.body.pos = pos;
                let entity = entity.id;
                sim.entities.add_to_set(activity_set, entity);
                entity
            };

            for (entity, hierarchy) in [(subject, subject_hierarchy), (target, target_hierarchy)] {
                sim.entities.set_parent(hierarchy, activity_entity, entity);
                sim.entities[entity].flags.set(participant_flag, true);
            }
        }
    }

    pub(super) fn tick_activities(arena: &Bump, sim: &mut Simulation) {
        for activity_set in [Set::Battle] {
            let activities = AVec::from_iter_in(sim.entities.iter_set_ids(activity_set), arena);

            let mut subjects = AVec::new_in(arena);
            let mut targets = AVec::new_in(arena);

            for activity_id in activities {
                subjects.clear();
                targets.clear();

                // Get the members out
                for (partecipants, role) in [
                    (&mut subjects, Hierarchy::ActivitySubject),
                    (&mut targets, Hierarchy::ActivityTarget),
                ] {
                    partecipants.extend(sim.entities.children_of(role, activity_id));
                }

                let rng = &mut rng_from_multi_seed(&[sim.turn_num, activity_id.data().as_ffi()]);

                // Specialise behavior for the specific activity
                let resolution = match activity_set {
                    Set::Battle => tick_battle(arena, sim, rng, &subjects, &targets),
                    _ => Resolution::Ongoing,
                };

                let winning_role = match resolution {
                    Resolution::Ongoing => {
                        continue;
                    }
                    Resolution::Resolved { winning_role } => winning_role,
                };

                // Despawn the activity
                sim.entities.despawn(activity_id).unwrap();

                // Process partecipants
                for (entities, role) in [
                    (subjects.as_slice(), Hierarchy::ActivitySubject),
                    (targets.as_slice(), Hierarchy::ActivityTarget),
                ] {
                    for &entity in entities {
                        let entity = &mut sim.entities[entity];
                        entity.flags.set(Flag::IsActivityPartecipant, false);
                        if Some(role) == winning_role {
                            entity.inc_var(Var::FrozenTimer, 100.);
                        }
                    }
                }
            }
        }
    }

    fn tick_battle(
        arena: &Bump,
        sim: &mut Simulation,
        rng: &mut SmallRng,
        subjects: &[EntityId],
        targets: &[EntityId],
    ) -> activities::Resolution {
        struct TroopKind<'a> {
            /// Variable from which the troop value is extracted/written to
            var: Var,
            stats: &'a [(Stat, f64)],
        }

        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter, EnumCount)]
        enum Stat {
            /// Shuld always be 1.
            Heads,
            /// Scales the amount of combat power provided by this type of troop
            Power,
            /// Scales the share of casualties taken by this type of troop
            CasualtyWeight,
        }

        #[derive(Clone, Copy, Default)]
        struct Stats([f64; Stat::COUNT]);

        impl Stats {
            fn get(&self, s: Stat) -> f64 {
                self.0[s as usize]
            }

            fn set(&mut self, s: Stat, v: f64) {
                self.0[s as usize] = v;
            }

            fn sum(it: impl Iterator<Item = Stats>) -> Self {
                let mut accum = Self::default();
                for item in it {
                    for stat in Stat::iter() {
                        let x = accum.get(stat) + item.get(stat);
                        accum.set(stat, x);
                    }
                }
                accum
            }
        }

        const SOLDIERS: TroopKind = TroopKind {
            var: Var::Soldiers,
            stats: &[
                (Stat::Heads, 1.),
                (Stat::Power, 1.),
                (Stat::CasualtyWeight, 1.),
            ],
        };

        const CIVILIANS: TroopKind = TroopKind {
            var: Var::Civilians,
            stats: &[
                (Stat::Heads, 1.),
                (Stat::Power, 0.1),
                (Stat::CasualtyWeight, 0.5),
            ],
        };

        const ALL_TROOPS_KINDS: [TroopKind; 2] = [SOLDIERS, CIVILIANS];

        const BASE_CASUALTY_RATE: f64 = 0.04;
        const MAX_CASUALTY_RATE: f64 = 0.18;

        const ROLES: [Hierarchy; 2] = [Hierarchy::ActivitySubject, Hierarchy::ActivityTarget];

        struct Troop<'a> {
            entity: EntityId,
            kind: &'a TroopKind<'a>,
            stats: Stats,
        }

        struct Side<'a> {
            troops: AVec<'a, Troop<'a>>,
            totals: Stats,
        }

        let sides = [subjects, targets].map(|entities| {
            let mut troops = AVec::new_in(arena);
            for &id in entities {
                let entity = &sim.entities[id];
                for kind in &ALL_TROOPS_KINDS {
                    let amount = entity.get_var(kind.var);
                    if amount > 0.0 {
                        let mut stats = Stats::default();
                        for &(stat, k) in kind.stats {
                            stats.set(stat, k * amount);
                        }
                        troops.push(Troop {
                            entity: id,
                            kind,
                            stats,
                        });
                    }
                }
            }

            let totals = Stats::sum(troops.iter().map(|t| t.stats));

            Side { troops, totals }
        });

        let overall = Stats::sum(sides.iter().map(|x| x.totals));
        let total_power = overall.get(Stat::Power);

        let mut losses = |headcount: f64, opponent_strength: f64| {
            let ratio = if total_power > 0.0 {
                opponent_strength / total_power
            } else {
                0.0
            };
            let ratio = ratio.clamp(0.0, 1.0);
            let variability = rng.gen_range(0.85..1.15);
            let proportion =
                (BASE_CASUALTY_RATE * (1.0 + ratio) * variability).clamp(0.0, MAX_CASUALTY_RATE);

            // The proportional part is good when the parties are large. However, when the parties are small, we would like
            // an acceleration of the casualties. For this, we add 1/4 of each size as casualties, capped to 20.

            let fixed_part = (opponent_strength / 4.).min(20.);

            (fixed_part + headcount * proportion).clamp(0., headcount)
        };

        let losses = [0, 1].map(|sidx| {
            losses(
                sides[sidx].totals.get(Stat::Heads),
                sides[1 - sidx].totals.get(Stat::Power),
            )
        });

        for side_idx in 0..2 {
            let side_total = sides[side_idx].totals.get(Stat::CasualtyWeight);
            let total_loss = losses[side_idx];

            if side_total <= 0.0 || total_loss <= 0.0 {
                continue;
            }

            for entry in &sides[side_idx].troops {
                let entry_weight = entry.stats.get(Stat::CasualtyWeight);
                if entry_weight <= 0.0 {
                    continue;
                }
                let share = entry_weight / side_total;
                let entity = &mut sim.entities[entry.entity];
                let next = {
                    let loss_amount = total_loss * share;
                    let next = (entity.get_var(entry.kind.var) - loss_amount).max(0.0);
                    let fractional = next.fract();
                    let adjusted = if rng.gen_bool(fractional) { 1. } else { 0. };
                    next - next.fract() + adjusted
                };
                entity.set_var(entry.kind.var, next);
            }
        }

        for side_idx in 0..2 {
            let remaining_headcount = sides[side_idx]
                .troops
                .iter()
                .map(|troop| sim.entities[troop.entity].get_var(troop.kind.var))
                .sum::<f64>();
            if remaining_headcount <= 0.0 {
                return Resolution::Resolved {
                    winning_role: Some(ROLES[1 - side_idx]),
                };
            }
        }

        Resolution::Ongoing
    }
}

fn rng_from_multi_seed(seeds: &[u64]) -> SmallRng {
    let mut seed = 0u64;
    for &i in seeds {
        seed = seed.wrapping_mul(13).wrapping_add(i).wrapping_mul(17);
    }
    SmallRng::seed_from_u64(seed)
}

struct DiploMap<'a> {
    alloc: AVec<'a, EntityId>,
    entries: SecondaryMap<EntityId, DiploEntry>,
}

#[derive(Clone, Copy, Default)]
struct DiploEntry {
    parent: EntityId,
    // Span of entities that are fixed to be hostile
    hostile_entities: Span,
    generally_hostile: bool,
}

impl<'a> DiploMap<'a> {
    fn new(arena: &'a Bump, capacity: usize) -> Self {
        Self {
            alloc: AVec::with_capacity_in(capacity, arena),
            entries: SecondaryMap::with_capacity(capacity),
        }
    }

    pub fn calculate(arena: &'a Bump, sim: &Simulation) -> Self {
        let _span = tracing::info_span!("Diplo Map").entered();
        let mut diplo_map = Self::new(arena, sim.entities.capacity());

        for entity in sim.entities.iter() {
            let generally_hostile = entity.flags.get(Flag::IsGenerallyHostile);
            if entity.in_set(Set::Factions) {
                let related = sim
                    .entities
                    .relationships_from(Relationship::Diplo, entity.id);
                let hostile = related.filter(|&(_, x)| x < 0.).map(|(id, _)| id);
                diplo_map.insert(entity.id, EntityId::null(), hostile, generally_hostile);
            } else {
                let faction = sim
                    .entities
                    .parent_of(Hierarchy::FactionMembership, entity.id);
                diplo_map.insert(entity.id, faction, std::iter::empty(), generally_hostile);
            }
        }

        diplo_map
    }

    fn insert(
        &mut self,
        id: EntityId,
        parent: EntityId,
        hostile: impl Iterator<Item = EntityId>,
        generally_hostile: bool,
    ) {
        let hostile_entities = {
            let start = self.alloc.len();
            self.alloc.extend(hostile);
            let end = self.alloc.len();
            Span::between(start, end)
        };
        self.entries.insert(
            id,
            DiploEntry {
                parent,
                hostile_entities,
                generally_hostile,
            },
        );
    }

    pub fn is_hostile(&self, id: EntityId, target: EntityId) -> bool {
        // If id is target, or if either is null, then there is no hostility.
        if id == target || id.is_null() || target.is_null() {
            return false;
        }

        // Extract entries
        let my_entry = self.entries.get(id).copied().unwrap_or_default();
        let target_entry = self.entries.get(target).copied().unwrap_or_default();

        // An entity that is "generally hostile", will be hostile unless parents match
        if my_entry.generally_hostile {
            return my_entry.parent.is_null() || my_entry.parent != target_entry.parent;
        }
        // Otherwise, first we check if the target is directly hostile to me
        let directly_hostile = my_entry
            .hostile_entities
            .view(&self.alloc)
            .contains(&target);
        if directly_hostile {
            return true;
        }

        // Otherwise, if we have a parent, we could still be hostile because of the parent
        if !my_entry.parent.is_null() {
            return self.is_hostile(my_entry.parent, target_entry.parent);
        }

        // All failed: we are not hostile
        false
    }
}

mod detection {
    use super::*;

    #[derive(Default)]
    pub(crate) struct Detections {
        data: Vec<Detection>,
        spans: SecondaryMap<EntityId, Span>,
    }

    impl Detections {
        pub(crate) fn clear(&mut self) {
            self.data.clear();
            self.spans.clear();
        }

        pub(crate) fn set(&mut self, id: EntityId, detections: &[Detection]) {
            let detections = detections.iter().copied();
            let span = Span::of_extension(&mut self.data, detections);
            self.spans.insert(id, span);
        }

        pub(crate) fn get_for(&self, id: EntityId) -> &[Detection] {
            self.spans
                .get(id)
                .map(|span| span.view(&self.data))
                .unwrap_or_default()
        }

        pub(crate) fn collides(&self, id: EntityId, target: EntityId) -> bool {
            self.get_for(id)
                .iter()
                .any(|entry| entry.id == target && entry.distance <= 0.0)
        }
    }

    #[derive(Clone, Copy)]
    pub(crate) struct Detection {
        pub id: EntityId,
        pub distance: f32,
        pub is_location: bool,
        pub threat: f64,
    }

    pub(crate) fn calculate(
        detections: &mut Detections,
        sim: &Simulation,
        spatial_map: &SpatialMap,
        diplo_map: &DiploMap,
    ) {
        detections.clear();

        let mut scratch = Vec::with_capacity(100);
        for subject in sim.entities.iter() {
            if subject.get_flag(Flag::IsDisembodied) {
                continue;
            }

            const DETECTION_RANGE: f32 = 20.;
            let neighbours = spatial_map.search(subject.body.pos, DETECTION_RANGE);

            for target in neighbours {
                // Do not look at yourself
                if target == subject.id {
                    continue;
                }
                let target = &sim.entities[target];
                // Distance net of sizes. 0 or lower means collision
                let distance = body_distance(subject.body, target.body);

                let is_location = target.in_set(Set::Locations);

                let threat = if diplo_map.is_hostile(target.id, subject.id) {
                    calculate_power(target)
                } else {
                    0.
                };

                scratch.push(Detection {
                    id: target.id,
                    is_location,
                    distance,
                    threat,
                });
            }
            detections.set(subject.id, &scratch);
            scratch.clear();
        }
    }

    fn body_distance(b1: Body, b2: Body) -> f32 {
        let center_to_center_distance = V2::distance(b1.pos, b2.pos);
        let collision_range = (b1.size + b2.size) / 2.;

        // Distance net of sizes. 0 or lower means collision
        center_to_center_distance - collision_range
    }
}

fn tick_timers(sim: &mut Simulation) {
    const TIMERS: [Var; 1] = [Var::FrozenTimer];
    for entity in sim.entities.iter_mut() {
        for var in TIMERS {
            let value = entity.get_var(var);
            if value > 0.0 {
                entity.set_var(var, (value - 1.).max(0.));
            }
        }
    }
}

fn increase_recruits(sim: &mut Simulation) {
    const AUTO_RECRUIT_AMOUNT: f64 = 5.;
    for entity in sim.entities.iter_mut() {
        let target = entity.get_var(Var::AutoRecruitBase);
        let soldiers = entity.get_var(Var::Soldiers);
        let recruited = (target - soldiers).clamp(0., AUTO_RECRUIT_AMOUNT);
        entity.set_var(Var::Soldiers, soldiers + recruited);
    }
}
