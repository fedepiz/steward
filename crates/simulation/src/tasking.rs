use bumpalo::Bump;
use slotmap::Key;

use crate::entities::{
    self, Entity, EntityId, Behavior, Flag, Hierarchy, Interaction, Location, Set, Task,
    TaskDestination, TaskInteraction, TaskKind, Var,
};
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
    for (subject, task) in sim.entities.iter().zip(tasks) {
        let (task, destination, behavior) = task_for_entity(sim, subject, task);
        results.push((subject.id, task, destination, behavior));
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
            interaction: TaskInteraction::new(&[Interaction::LoadFood, Interaction::UnloadGoods]),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::MarketOfHome,
            interaction: TaskInteraction::with(Interaction::SellFood),
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
            interaction: TaskInteraction::with(Interaction::LoadMinerals),
            arrival_wait: LONG_WAIT,
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::Base,
            interaction: TaskInteraction::with(Interaction::UnloadMinerals),
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
            interaction: TaskInteraction::with(Interaction::CaravanVisit),
            arrival_wait: SHORT_WAIT,
            ..Default::default()
        },
        TaskKind::Load => Task {
            kind: TaskKind::Deliver,
            destination: TaskDestination::TradingDestination,
            interaction: TaskInteraction::with(Interaction::CaravanVisit),
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
}

fn task_for_entity(
    sim: &Simulation,
    subject: &Entity,
    mut task: Task,
) -> (Task, Destination, Behavior) {
    // Retask when initializing or once the current task is complete.
    let retask = task.kind == TaskKind::Init || task.is_complete;
    if retask {
        task = if subject.in_set(Set::People) {
            person_tasking()
        } else if subject.flags.get(Flag::IsFarmer) {
            farmer_tasking(task.kind)
        } else if subject.flags.get(Flag::IsMiner) {
            miner_tasking(task.kind)
        } else if subject.flags.get(Flag::IsCaravan) {
            caravan_tasking(task.kind)
        } else {
            Task::default()
        };
    }

    // Resolve the concrete target for the task's symbolic destination.
    let destination = match task.destination {
        TaskDestination::Nothing => Destination::default(),
        TaskDestination::Base => {
            let target = sim.entities.parent_of(Hierarchy::Attachment, subject.id);
            Destination {
                target,
                enter: true,
            }
        }
        TaskDestination::Home => {
            let target = sim.entities.parent_of(Hierarchy::HomeOf, subject.id);
            Destination {
                target,
                enter: true,
            }
        }
        TaskDestination::MarketOfHome => {
            let home = sim.entities.parent_of(Hierarchy::Attachment, subject.id);
            let target = sim.entities.parent_of(Hierarchy::LocalMarket, home);
            Destination {
                target,
                enter: true,
            }
        }
        TaskDestination::WorkArea => {
            let target = sim.entities.parent_of(Hierarchy::WorkArea, subject.id);
            Destination {
                target,
                enter: false,
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
            }
        }
    };

    // Reset task if destination is not valid
    let is_valid = !destination.target.is_null();
    if !is_valid {
        return (Task::default(), destination, Behavior::Idle);
    }

    // Behavior is driven by the task destination.
    let behavior = if destination.target.is_null() {
        subject.behavior
    } else {
        Behavior::GoTo {
            target: destination.target,
            on_arrival: if destination.enter {
                OnArrival::Enter
            } else {
                OnArrival::Nothing
            },
        }
    };

    (task, destination, behavior)
}

fn resolve_task_effects(
    sim: &mut Simulation,
    subject: EntityId,
    destination: Destination,
    task: &mut Task,
) {
    let target = destination.target;
    let at_destination = !target.is_null() && {
        let location = sim.entities[subject].location;
        if destination.enter {
            location == Location::Inside(target)
        } else {
            location.is_at(target)
        }
    };

    if !at_destination {
        return;
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

    task.is_complete = at_destination && !task.interaction.any();
}

fn handle_interaction(
    sim: &mut Simulation,
    interaction: Interaction,
    subject_id: EntityId,
    target_id: EntityId,
) -> bool {
    match interaction {
        Interaction::SellFood => {
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
        Interaction::LoadFood => {
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
        Interaction::UnloadMinerals => {
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
        Interaction::LoadMinerals => {
            sim.entities[subject_id]
                .vars_mut()
                .modify(Var::Minerals, |x| x + MINERALS_PER_LOAD);
            true
        }
        Interaction::UnloadGoods => {
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
        Interaction::CaravanVisit => {
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
    }
}
