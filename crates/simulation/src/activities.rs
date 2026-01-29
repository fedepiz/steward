use bumpalo::Bump;
use rand::rngs::SmallRng;
use rand::*;
use slotmap::Key;
use strum::{EnumCount, EnumIter, IntoEnumIterator};

use crate::entities::*;
use crate::simulation::*;

type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Type {
    Battle,
}

pub(crate) enum Resolution {
    Ongoing,
    Resolved { winning_role: Option<Hierarchy> },
}

pub(crate) fn begin_activities<'a>(sim: &mut Simulation, incipits: &[(Type, EntityId, EntityId)]) {
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
) -> Resolution {
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

fn rng_from_multi_seed(seeds: &[u64]) -> SmallRng {
    let mut seed = 0u64;
    for &i in seeds {
        seed = seed.wrapping_mul(13).wrapping_add(i).wrapping_mul(17);
    }
    SmallRng::seed_from_u64(seed)
}
