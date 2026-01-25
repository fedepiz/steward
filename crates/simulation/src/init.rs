use std::collections::HashMap;

use crate::entities::{Behavior, Flag, Hierarchy, Set, Var};
use crate::names::Name;
use crate::simulation::*;
use crate::{geom::*, terrain_map};

pub(crate) fn init(sim: &mut Simulation, req: InitRequest) {
    sim.terrain_map = terrain_map::init(&req.elevations, req.map_width, req.map_height);

    const BASE_SPEED: f32 = 2.;
    const WALK_SPEED: f32 = 1.;

    const LOCATION_SETS: &[Set] = &[Set::Settlements, Set::Mines];

    const SIZE_TINY: f32 = 1.;
    const SIZE_SMALL: f32 = 2.;

    {
        let typ = sim.entities.add_type();
        typ.tag = "battle";
        typ.image = "battle";
        typ.size = SIZE_TINY;
        typ.layer = 2;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "person";
        typ.image = "person";
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = BASE_SPEED;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "farmers";
        typ.image = "farmers";
        typ.name = Name::simple(sim.names.define("Farmers"));
        typ.size = SIZE_SMALL;
        typ.speed = 1.;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "miners";
        typ.image = "miners";
        typ.name = Name::simple(sim.names.define("Miners"));
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "caravan";
        typ.image = "caravan";
        typ.name = Name::simple(sim.names.define("Caravan"));
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "mine";
        typ.image = "mine";
        typ.name = Name::simple(sim.names.define("Mine"));
        typ.speed = 0.;
        typ.size = 2.5;
        typ.layer = 0;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "village";
        typ.image = "village";
        typ.name = Name::simple(sim.names.define("Village"));
        typ.size = 3.0;
        typ.always_show_name = true;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "hillfort";
        typ.image = "hillfort";
        typ.name = Name::simple(sim.names.define("Hillfort"));
        typ.size = 3.;
        typ.always_show_name = true;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "town";
        typ.image = "town";
        typ.name = Name::simple(sim.names.define("Town"));
        typ.size = 4.;
        typ.always_show_name = true;
    }

    {
        let typ = sim.entities.add_type();
        typ.tag = "faction";
        typ.is_disembodied = true;
    }

    #[derive(Default, Clone, Copy)]
    struct FactionDesc<'a> {
        key: &'a str,
        name: &'a str,
        color: (u8, u8, u8),
    }

    let factions = [
        FactionDesc {
            key: "rheged",
            name: "Rheged",
            color: (200, 40, 40),
        },
        FactionDesc {
            key: "crafu",
            name: "Crafu",
            color: (130, 70, 180),
        },
    ];

    #[derive(Default, Clone, Copy)]
    struct EntityDesc<'a> {
        key: &'a str,
        name: &'a str,
        pos: (f32, f32),
        party_typ: &'a str,
        vars: &'a [(Var, f64)],
        flags: &'a [Flag],
        sets: &'a [Set],
        parents: &'a [(Hierarchy, &'a str)],
        children: &'a [(Hierarchy, &'a str)],
        is_player: bool,
    }

    const SETTLEMENT_PROSPERITY: f64 = 0.5;
    const TOWN_POPULATION: f64 = 5000.;
    const VILLAGE_POPULATION: f64 = 1000.;
    const HILLFORT_POPULATION: f64 = 1000.;
    const VILLAGE_FOOD_CAPACITY: f64 = 1000.;
    const HILLFORT_FOOD_CAPACITY: f64 = 10_000.;
    const TOWN_FOOD_CAPACITY: f64 = 10_000.;
    const PERSON_RENOWN: f64 = 10.;

    let entities = [
        EntityDesc {
            pos: (615., 540.),
            party_typ: "person",
            name: "Ambrosius Aurelianus",
            sets: &[Set::People],
            vars: &[(Var::Renown, PERSON_RENOWN)],
            flags: &[Flag::IsGenerallyHostile],
            is_player: true,
            // parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        EntityDesc {
            pos: (610., 520.),
            party_typ: "person",
            name: "Rhoedd map Rhun",
            sets: &[Set::People],
            vars: &[(Var::Renown, PERSON_RENOWN)],
            parents: &[
                (Hierarchy::FactionMembership, "rheged"),
                // TODO: Make this deduced, not given as init
                (Hierarchy::HomeOf, "caer_ligualid"),
            ],
            children: &[
                (Hierarchy::RulerOf, "rheged"),
                (Hierarchy::Lordship, "caer_ligualid"),
                (Hierarchy::Lordship, "llan_heledd"),
            ],
            ..Default::default()
        },
        EntityDesc {
            pos: (600., 520.),
            party_typ: "person",
            name: "Gwaith map Elffin",
            sets: &[Set::People],
            vars: &[(Var::Renown, PERSON_RENOWN)],
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            children: &[
                (Hierarchy::Lordship, "caer_wenddoleu"),
                (Hierarchy::Lordship, "anava"),
            ],
            ..Default::default()
        },
        EntityDesc {
            pos: (600., 500.),
            party_typ: "person",
            name: "Eadwine map Owain",
            sets: &[Set::People],
            vars: &[(Var::Renown, PERSON_RENOWN)],
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        EntityDesc {
            key: "caer_ligualid",
            name: "Caer Ligualid",
            pos: (592., 514.),
            party_typ: "town",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, TOWN_POPULATION),
                (Var::FoodCapacity, TOWN_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Towns],
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        EntityDesc {
            key: "llan_heledd",
            name: "Llan Heledd",
            pos: (570., 540.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_ligualid"),
                (Hierarchy::FactionMembership, "rheged"),
            ],
            ..Default::default()
        },
        EntityDesc {
            name: "Din Drust",
            pos: (570., 490.),
            party_typ: "hillfort",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, HILLFORT_POPULATION),
                (Var::FoodCapacity, HILLFORT_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Hillforts],
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        EntityDesc {
            name: "Din Reghed",
            pos: (530., 500.),
            party_typ: "hillfort",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, HILLFORT_POPULATION),
                (Var::FoodCapacity, HILLFORT_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Hillforts],
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        EntityDesc {
            name: "Ad Candidam Casam",
            pos: (530., 520.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_ligualid"),
                (Hierarchy::FactionMembership, "rheged"),
            ],
            ..Default::default()
        },
        EntityDesc {
            name: "Isura",
            pos: (560., 500.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_ligualid"),
                (Hierarchy::FactionMembership, "rheged"),
            ],
            ..Default::default()
        },
        EntityDesc {
            key: "anava",
            name: "Anava",
            pos: (603., 500.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_ligualid"),
                (Hierarchy::FactionMembership, "rheged"),
            ],
            ..Default::default()
        },
        EntityDesc {
            key: "caer_wenddoleu",
            name: "Caer Wenddoleu",
            pos: (625., 505.),
            party_typ: "hillfort",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, HILLFORT_POPULATION),
                (Var::FoodCapacity, HILLFORT_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements],
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        EntityDesc {
            name: "Lligwy Mine",
            pos: (600., 530.),
            party_typ: "mine",
            sets: &[Set::Mines],
            ..Default::default()
        },
        EntityDesc {
            key: "caer_maunguid",
            name: "Caer Maunguid",
            pos: (630., 665.),
            party_typ: "town",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, TOWN_POPULATION),
                (Var::FoodCapacity, TOWN_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Towns],
            parents: &[(Hierarchy::FactionMembership, "crafu")],
            ..Default::default()
        },
        EntityDesc {
            name: "Maes Cogwy",
            pos: (605., 625.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_maunguid"),
                (Hierarchy::FactionMembership, "crafu"),
            ],
            ..Default::default()
        },
        EntityDesc {
            name: "Ecclesia Hyll",
            pos: (600., 645.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_maunguid"),
                (Hierarchy::FactionMembership, "crafu"),
            ],
            ..Default::default()
        },
        EntityDesc {
            name: "Ced",
            pos: (600., 675.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_maunguid"),
                (Hierarchy::FactionMembership, "crafu"),
            ],
            ..Default::default()
        },
        EntityDesc {
            name: "Dwfr",
            pos: (640., 690.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[
                (Hierarchy::LocalMarket, "caer_maunguid"),
                (Hierarchy::FactionMembership, "crafu"),
            ],
            ..Default::default()
        },
    ];

    let player_name = Name::simple(sim.names.define("Player"));

    let mut keys = HashMap::new();
    let mut pass2 = vec![];

    for faction in factions {
        let archetype = sim.entities.find_type_by_tag("faction").unwrap().id;
        let entity = sim.entities.spawn_with_type(archetype);
        entity.name = Name::simple(sim.names.define(faction.name));
        let entity = entity.id;
        sim.entities.add_to_set(Set::Factions, entity);
        sim.faction_colors.insert(entity, faction.color);
        keys.insert(faction.key, entity);
    }

    for desc in entities {
        let typ = sim
            .entities
            .find_type_by_tag(desc.party_typ)
            .copied()
            .unwrap();
        let entity = sim.entities.spawn_with_type(typ.id);
        let name = if desc.is_player {
            player_name
        } else if desc.name.is_empty() {
            typ.name
        } else {
            Name::simple(sim.names.define(desc.name))
        };
        entity.name = name;
        if desc.is_player {
            entity.speed = 5.;
        }
        entity.body.pos = V2::new(desc.pos.0, desc.pos.1);

        entity.name = name;
        entity.is_player = desc.is_player;
        entity.fixed_behavior = if desc.is_player {
            Some(Behavior::Player)
        } else {
            None
        };

        entity.vars_mut().set_many(desc.vars);
        for &flag in desc.flags {
            entity.flags.set(flag, true);
        }

        let entity = entity.id;

        for &set in desc.sets {
            sim.entities.add_to_set(set, entity);
        }

        let is_location = LOCATION_SETS.iter().any(|set| desc.sets.contains(set));
        if is_location {
            sim.entities.add_to_set(Set::Locations, entity);
        }

        if !desc.key.is_empty() {
            keys.insert(desc.key, entity);
        }
        pass2.push((desc, entity));
    }

    for (desc, entity) in pass2 {
        for &(hierarchy, key) in desc.parents {
            let parent = keys.get(key).copied().unwrap();
            sim.entities.set_parent(hierarchy, parent, entity);
        }

        for &(hierarchy, key) in desc.children {
            let child = keys.get(key).copied().unwrap();
            sim.entities.set_parent(hierarchy, entity, child);
        }
    }
}
