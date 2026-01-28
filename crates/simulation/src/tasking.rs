use bumpalo::Bump;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use slotmap::Key;

use crate::entities::{self, *};
use crate::geom::V2;
use crate::simulation::*;

type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;

pub(crate) fn entity_tasking(sim: &mut Simulation, arena: &Bump) {
    let _span = tracing::info_span!("Entity tasking").entered();
    // Snapshot tasks so task selection can read and write without aliasing entities.
    let tasks = AVec::from_iter_in(
        sim.entities
            .iter_mut()
            .map(|entity| std::mem::take(&mut entity.task)),
        arena,
    );

    // Determine task, destination, and desired behavior for each entity.
    let mut results = AVec::with_capacity_in(tasks.len(), arena);
    let mut despawns = AVec::new_in(arena);
    for (subject, task) in sim.entities.iter().zip(tasks) {
        let (task, destination, behavior, despawn) = task_for_entity(sim, subject, task);
        results.push((subject.id, task, destination, behavior));
        if despawn {
            despawns.push(subject.id);
        }
    }

    // Resolve task effects once entities are at their destinations.
    for (subject, task, destination, _) in &mut results {
        resolve_task_effects(sim, *subject, *destination, task);
    }

    // Commit task/behavior changes; fixed behavior overrides task-driven behavior.
    for (entity, (_, task, _, behavior)) in sim.entities.iter_mut().zip(results) {
        entity.task = task;
        entity.behavior = entity.fixed_behavior.unwrap_or(behavior);
    }

    for id in despawns {
        sim.entities.despawn(id);
    }
}

const SHORT_WAIT: u32 = 40;
const LONG_WAIT: u32 = 10 * SHORT_WAIT;
const MINERALS_PER_LOAD: f64 = 100.0;
const GOODS_PER_FOOD_DELIVERED: f64 = 0.1;
const GOODS_PER_CARAVAN_LOAD: f64 = 100.;

fn farmer_tasking(kind: TaskKind) -> Task {
    match kind {
        TaskKind::Init => Task {
            kind: TaskKind::ReturnToBase,
            destination: TaskDestination::Base,
            ..Default::default()
        },
        TaskKind::ReturnToBase => Task {
            kind: TaskKind::Load,
            destination: TaskDestination::Base,
            interaction: TaskInteraction::new(&[
                InteractionKind::LoadFood,
                InteractionKind::UnloadGoods,
            ]),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::MarketOfHome,
            interaction: TaskInteraction::with(InteractionKind::SellFood),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        },
        TaskKind::Deliver => Task {
            kind: TaskKind::ReturnToBase,
            destination: TaskDestination::Base,
            ..Default::default()
        },
        _ => Default::default(),
    }
}

fn miner_tasking(kind: TaskKind) -> Task {
    match kind {
        TaskKind::Init => Task {
            kind: TaskKind::ReturnToBase,
            destination: TaskDestination::Base,
            ..Default::default()
        },
        TaskKind::ReturnToBase => Task {
            kind: TaskKind::Load,
            destination: TaskDestination::WorkArea,
            interaction: TaskInteraction::with(InteractionKind::LoadMinerals),
            arrival_wait: LONG_WAIT,
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::Base,
            interaction: TaskInteraction::with(InteractionKind::UnloadMinerals),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        },
        TaskKind::Deliver => Task {
            kind: TaskKind::ReturnToBase,
            destination: TaskDestination::Base,
            ..Default::default()
        },
        _ => Default::default(),
    }
}

fn caravan_tasking(kind: TaskKind) -> Task {
    match kind {
        TaskKind::Init => Task {
            kind: TaskKind::ReturnToBase,
            destination: TaskDestination::Base,
            interaction: TaskInteraction::default(),
            ..Default::default()
        },
        TaskKind::ReturnToBase => Task {
            kind: TaskKind::Load,
            destination: TaskDestination::Base,
            interaction: TaskInteraction::with(InteractionKind::CaravanVisit),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::TradingDestination,
            interaction: TaskInteraction::with(InteractionKind::CaravanVisit),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        },
        TaskKind::Deliver => Task {
            kind: TaskKind::ReturnToBase,
            destination: TaskDestination::Base,
            interaction: TaskInteraction::default(),
            ..Default::default()
        },
        _ => Default::default(),
    }
}

fn bandit_tasking(sim: &Simulation, subject: &Entity) -> Task {
    // If homeless, suicide
    let home = sim.entities.parent_of(Hierarchy::Attachment, subject.id);
    if home.is_null() {
        return Task {
            despawn_on_choice: true,
            ..Default::default()
        };
    }

    let soldiers = subject.get_var(Var::Soldiers);
    let desired_soldiers = subject.get_var(Var::DesiredSoldiers);
    // If not enough soldiers, go home and pick up more
    if desired_soldiers > 0.0 && soldiers < desired_soldiers {
        Task {
            kind: TaskKind::ReturnToBase,
            destination: TaskDestination::Base,
            interaction: TaskInteraction::with(InteractionKind::LoadSoldiers),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        }
    } else {
        // Roam and hunt
        let rng = &mut SmallRng::seed_from_u64(
            sim.turn_num
                .wrapping_mul(13)
                .wrapping_add(subject.id.data().as_ffi()),
        );

        let radius = 20.;
        let dx = rng.gen_range(-radius..radius);
        let dy = rng.gen_range(-radius..radius);

        let designated_pos = sim.entities[home].body.pos + V2::new(dx, dy);

        let destination = if sim.terrain_map.is_pos_traversable(designated_pos) {
            TaskDestination::DesignatedPos
        } else {
            TaskDestination::Nothing
        };

        Task {
            kind: TaskKind::Hunt,
            destination,
            designated_pos,
            interaction: TaskInteraction::default(),
            ..Default::default()
        }
    }
}

fn person_tasking() -> Task {
    Task {
        kind: TaskKind::Generic,
        destination: TaskDestination::Home,
        arrival_wait: LONG_WAIT,
        ..Default::default()
    }
}

#[derive(Default, Clone, Copy)]
struct Destination {
    target: EntityId,
    enter: bool,
    has_pos: bool,
    pos: V2,
}

fn task_for_entity(
    sim: &Simulation,
    subject: &Entity,
    mut task: Task,
) -> (Task, Destination, Behavior, bool) {
    // Retask when initializing or once the current task is complete.
    let retask = task.kind == TaskKind::Init || task.is_complete;

    if retask {
        task = if subject.in_set(Set::People) {
            person_tasking()
        } else if subject.get_flag(Flag::IsFarmer) {
            farmer_tasking(task.kind)
        } else if subject.get_flag(Flag::IsMiner) {
            miner_tasking(task.kind)
        } else if subject.get_flag(Flag::IsCaravan) {
            caravan_tasking(task.kind)
        } else if subject.get_flag(Flag::IsBandit) {
            bandit_tasking(sim, subject)
        } else {
            Task::default()
        };

        if task.despawn_on_choice {
            return (task, Destination::default(), Behavior::default(), true);
        }
    }

    // Resolve the concrete target for the task's symbolic destination.
    let destination = resolve_task_destination(sim, subject, &task);

    // Reset task if destination is not valid
    let is_valid = !destination.target.is_null() || destination.has_pos;
    if !is_valid {
        return (Task::default(), destination, Behavior::Idle, false);
    }

    // Behavior is driven by the task destination.
    let mut behavior = subject.behavior;
    if !destination.target.is_null() {
        behavior = Behavior::ToEntity {
            target: destination.target,
            on_arrival: if destination.enter {
                OnArrival::Enter
            } else {
                OnArrival::Nothing
            },
        };
    }

    if destination.has_pos {
        behavior = Behavior::ToPos(destination.pos);
    }

    (task, destination, behavior, false)
}

fn resolve_task_destination(sim: &Simulation, subject: &Entity, task: &Task) -> Destination {
    match task.destination {
        TaskDestination::Nothing => Destination::default(),
        TaskDestination::DesignatedPos => Destination {
            pos: task.designated_pos,
            has_pos: true,
            ..Default::default()
        },
        TaskDestination::Base => {
            let target = sim.entities.parent_of(Hierarchy::Attachment, subject.id);
            Destination {
                target,
                enter: true,
                ..Default::default()
            }
        }
        TaskDestination::Home => {
            let target = sim.entities.parent_of(Hierarchy::HomeOf, subject.id);
            Destination {
                target,
                enter: true,
                ..Default::default()
            }
        }
        TaskDestination::MarketOfHome => {
            let home = sim.entities.parent_of(Hierarchy::Attachment, subject.id);
            let target = sim.entities.parent_of(Hierarchy::LocalMarket, home);
            Destination {
                target,
                enter: true,
                ..Default::default()
            }
        }
        TaskDestination::WorkArea => {
            let target = sim.entities.parent_of(Hierarchy::WorkArea, subject.id);
            Destination {
                target,
                enter: false,
                ..Default::default()
            }
        }
        TaskDestination::TradingDestination => {
            let home = sim.entities.parent_of(Hierarchy::Attachment, subject.id);
            let market_of_home = sim.entities.parent_of(Hierarchy::LocalMarket, home);
            let excluded = [home, market_of_home];
            let target = sim
                .entities
                .iter_set_ids(entities::Set::Towns)
                .find(|target| !excluded.contains(target))
                .unwrap_or_default();
            Destination {
                target,
                enter: true,
                ..Default::default()
            }
        }
    }
}

fn resolve_task_effects(
    sim: &mut Simulation,
    subject: EntityId,
    destination: Destination,
    task: &mut Task,
) {
    {
        let subject = &sim.entities[subject];
        let mut at_destination = false;
        if !destination.target.is_null() {
            let location = subject.location;
            at_destination = if destination.enter {
                location == Location::Inside(destination.target)
            } else {
                location.is_at(destination.target)
            };
        }

        if destination.has_pos {
            at_destination = subject.body.pos == destination.pos;
        }

        if !at_destination {
            return;
        }
    }

    if task.arrival_wait > 0 {
        task.arrival_wait = task.arrival_wait.saturating_sub(1);
        return;
    }

    for interaction in task.interaction.iter_active() {
        let is_complete = handle_interaction(sim, interaction, subject, destination.target);
        if is_complete {
            task.interaction.set(interaction, false);
        }
    }

    task.is_complete = !task.interaction.any();
}

fn handle_interaction(
    sim: &mut Simulation,
    interaction: InteractionKind,
    subject_id: EntityId,
    target_id: EntityId,
) -> bool {
    match interaction {
        InteractionKind::SellFood => {
            let subject = &sim.entities[subject_id];
            let target = &sim.entities[target_id];
            let on_farmer = subject.get_var(Var::FoodStored) as i64;
            let on_settlement = target.get_var(Var::FoodStored) as i64;
            let new_at_settlement = on_farmer + on_settlement;
            let prosperity = target.get_var(Var::Prosperity).clamp(0.0, 1.0);
            let goods_created =
                ((on_farmer as f64) * prosperity * GOODS_PER_FOOD_DELIVERED).round() as i64;
            let goods_on_farmer = subject.get_var(Var::Goods) as i64 + goods_created;

            sim.entities[subject_id]
                .vars_mut()
                .with(Var::FoodStored, 0.)
                .with(Var::Goods, goods_on_farmer as f64);
            sim.entities[target_id]
                .vars_mut()
                .with(Var::FoodStored, new_at_settlement as f64);
            true
        }
        InteractionKind::LoadFood => {
            const MIN_CARRIED_FOOD: i64 = 50;
            const MAX_CARRIED_FOOD: i64 = 500;
            const MAX_CARRIED_PROP: f64 = 0.5;

            let subject = &sim.entities[subject_id];
            let target = &sim.entities[target_id];

            let current_food = subject.get_var(Var::FoodStored) as i64;
            let mut at_settlement = target.get_var(Var::FoodStored) as i64;
            let max_exportable = (at_settlement as f64 * MAX_CARRIED_PROP)
                .min(at_settlement as f64)
                .round() as i64;

            if current_food + max_exportable >= MIN_CARRIED_FOOD {
                let mut on_farmer = current_food + max_exportable;
                let put_down = (on_farmer - MAX_CARRIED_FOOD).max(0);
                on_farmer -= put_down;
                at_settlement = at_settlement - max_exportable + put_down;

                let opportunity = (target.get_var(Var::FarmerOpportunity)
                    + FARMER_OPPORTUNITY_VISIT_CHANGE)
                    .clamp(FARMER_OPPORTUNITY_MIN, 0.);

                sim.entities[subject_id]
                    .vars_mut()
                    .with(Var::FoodStored, on_farmer as f64);

                sim.entities[target_id]
                    .vars_mut()
                    .with(Var::FoodStored, at_settlement as f64)
                    .with(Var::FarmerOpportunity, opportunity);

                true
            } else {
                false
            }
        }
        InteractionKind::UnloadMinerals => {
            let subject = &sim.entities[subject_id];
            let target = &sim.entities[target_id];
            let on_miner = subject.get_var(Var::Minerals) as i64;
            let on_settlement = target.get_var(Var::Minerals) as i64;
            let new_at_settlement = on_miner + on_settlement;

            sim.entities[target_id]
                .vars_mut()
                .with(Var::Minerals, new_at_settlement as f64);

            sim.entities[subject_id].vars_mut().with(Var::Minerals, 0.);

            true
        }
        InteractionKind::LoadMinerals => {
            sim.entities[subject_id]
                .vars_mut()
                .modify(Var::Minerals, |x| x + MINERALS_PER_LOAD);
            true
        }
        InteractionKind::UnloadGoods => {
            let subject = &sim.entities[subject_id];
            let target = &sim.entities[target_id];
            let on_farmer = subject.get_var(Var::Goods) as i64;
            let on_settlement = target.get_var(Var::Goods) as i64;
            let new_at_settlement = on_farmer + on_settlement;

            sim.entities[target_id]
                .vars_mut()
                .with(Var::Goods, new_at_settlement as f64);

            sim.entities[subject_id].vars_mut().with(Var::Goods, 0.);

            true
        }
        InteractionKind::CaravanVisit => {
            let mut on_caravan = sim.entities[subject_id].get_var(Var::Goods);
            let target = &sim.entities[target_id];
            let mut in_town = target.get_var(Var::Goods);
            let prosperity = target.get_var(Var::Prosperity);

            in_town += on_caravan;

            let new_goods = (GOODS_PER_CARAVAN_LOAD * (1.0 + 2. * prosperity)).round();
            on_caravan = new_goods;

            sim.entities[subject_id]
                .vars_mut()
                .set(Var::Goods, on_caravan);

            sim.entities[target_id].vars_mut().set(Var::Goods, in_town);

            true
        }
        InteractionKind::LoadSoldiers => {
            let subject = &sim.entities[subject_id];
            let target = &sim.entities[target_id];
            let desired = subject.get_var(Var::DesiredSoldiers);
            if desired <= 0.0 {
                return true;
            }

            let on_subject = subject.get_var(Var::Soldiers);
            if on_subject >= desired {
                return true;
            }

            let at_base = target.get_var(Var::Soldiers);
            let needed = (desired - on_subject).max(0.0);
            let transferred = needed.min(at_base);

            sim.entities[subject_id]
                .vars_mut()
                .with(Var::Soldiers, on_subject + transferred);
            sim.entities[target_id]
                .vars_mut()
                .with(Var::Soldiers, at_base - transferred);

            true
        }
    }
}
