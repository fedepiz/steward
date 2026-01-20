use std::collections::HashMap;

use crate::agents::{Behavior, Hierarchy, Set, Var};
use crate::names::Name;
use crate::simulation::*;
use crate::{geom::*, terrain_map};

pub(crate) fn init(sim: &mut Simulation, req: InitRequest) {
    sim.terrain_map = terrain_map::init(&req.elevations, req.map_width, req.map_height);

    const BASE_SPEED: f32 = 2.;
    const WALK_SPEED: f32 = 1.;

    const LOCATION_SETS: &[Set] = &[Set::Settlements, Set::Mines];

    const SIZE_SMALL: f32 = 2.;

    {
        let typ = sim.parties.add_type();
        typ.tag = "person";
        typ.image = "pawns/person";
        typ.name = Name::simple(sim.names.define("Person"));
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = BASE_SPEED;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "farmers";
        typ.image = "pawns/farmers";
        typ.name = Name::simple(sim.names.define("Farmers"));
        typ.size = SIZE_SMALL;
        typ.speed = 1.;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "miners";
        typ.image = "pawns/miners";
        typ.name = Name::simple(sim.names.define("Miners"));
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "mine";
        typ.image = "pawns/mine";
        typ.name = Name::simple(sim.names.define("Mine"));
        typ.speed = 0.;
        typ.size = 2.5;
        typ.layer = 0;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "village";
        typ.image = "pawns/village";
        typ.name = Name::simple(sim.names.define("Village"));
        typ.size = 3.0;
        typ.always_show_name = true;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "hillfort";
        typ.image = "pawns/hillfort";
        typ.name = Name::simple(sim.names.define("Hillfort"));
        typ.size = 3.;
        typ.always_show_name = true;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "town";
        typ.image = "pawns/town";
        typ.name = Name::simple(sim.names.define("Town"));
        typ.size = 4.;
        typ.always_show_name = true;
    }

    #[derive(Default, Clone, Copy)]
    struct Desc<'a> {
        key: &'a str,
        name: &'a str,
        pos: (f32, f32),
        party_typ: &'a str,
        vars: &'a [(Var, f64)],
        sets: &'a [Set],
        parents: &'a [(Hierarchy, &'a str)],
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

    let descs = [
        Desc {
            pos: (590., 520.),
            party_typ: "person",
            vars: &[(Var::Renown, PERSON_RENOWN)],
            is_player: true,
            ..Default::default()
        },
        Desc {
            pos: (610., 520.),
            party_typ: "person",
            vars: &[(Var::Renown, PERSON_RENOWN)],
            ..Default::default()
        },
        Desc {
            pos: (600., 520.),
            party_typ: "person",
            vars: &[(Var::Renown, PERSON_RENOWN)],
            ..Default::default()
        },
        Desc {
            pos: (600., 500.),
            party_typ: "person",
            vars: &[(Var::Renown, PERSON_RENOWN)],
            ..Default::default()
        },
        Desc {
            key: "caer_ligualid",
            name: "Caer Ligualid",
            pos: (597., 509.),
            party_typ: "town",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, TOWN_POPULATION),
                (Var::FoodCapacity, TOWN_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Towns],
            ..Default::default()
        },
        Desc {
            name: "Llan Heledd",
            pos: (580., 520.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[(Hierarchy::LocalMarket, "caer_ligualid")],
            ..Default::default()
        },
        Desc {
            name: "Din Drust",
            pos: (570., 490.),
            party_typ: "hillfort",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, HILLFORT_POPULATION),
                (Var::FoodCapacity, HILLFORT_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Hillforts],
            ..Default::default()
        },
        Desc {
            name: "Din Reghed",
            pos: (530., 500.),
            party_typ: "hillfort",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, HILLFORT_POPULATION),
                (Var::FoodCapacity, HILLFORT_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Hillforts],
            ..Default::default()
        },
        Desc {
            name: "Ad Candidam Casam",
            pos: (530., 520.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[(Hierarchy::LocalMarket, "caer_ligualid")],
            ..Default::default()
        },
        Desc {
            name: "Isura",
            pos: (560., 500.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[(Hierarchy::LocalMarket, "caer_ligualid")],
            ..Default::default()
        },
        Desc {
            name: "Anava",
            pos: (603., 500.),
            party_typ: "village",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, VILLAGE_POPULATION),
                (Var::FoodCapacity, VILLAGE_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements, Set::Villages],
            parents: &[(Hierarchy::LocalMarket, "caer_ligualid")],
            ..Default::default()
        },
        Desc {
            name: "Caer Wenddoleu",
            pos: (625., 505.),
            party_typ: "hillfort",
            vars: &[
                (Var::Prosperity, SETTLEMENT_PROSPERITY),
                (Var::Population, HILLFORT_POPULATION),
                (Var::FoodCapacity, HILLFORT_FOOD_CAPACITY),
            ],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Lligwy Mine",
            pos: (600., 522.),
            party_typ: "mine",
            sets: &[Set::Mines],
            parents: &[(Hierarchy::LocalMarket, "caer_ligualid")],
            ..Default::default()
        },
    ];

    let player_name = Name::simple(sim.names.define("Player"));

    let mut keys = HashMap::new();
    let mut pass2 = vec![];

    for desc in descs {
        let typ = sim.parties.find_type_by_tag(desc.party_typ).unwrap();
        let party = sim.parties.spawn_with_type(typ.id);
        let name = if desc.is_player {
            player_name
        } else if desc.name.is_empty() {
            typ.name
        } else {
            Name::simple(sim.names.define(desc.name))
        };
        party.name = name;
        if desc.is_player {
            party.speed = 5.;
        }
        party.body.pos = V2::new(desc.pos.0, desc.pos.1);

        let agent = sim.agents.spawn();
        agent.name = name;
        agent.is_player = desc.is_player;
        agent.fixed_behavior = Some(if desc.is_player {
            Behavior::Player
        } else {
            Behavior::Test
        });
        agent.party = party.id;
        party.agent = agent.id;

        // Settlements are locations
        party.is_location = LOCATION_SETS.iter().any(|set| desc.sets.contains(set));

        let agent = agent.id;

        sim.agents.vars_mut(agent).set_many(desc.vars);

        for &set in desc.sets {
            sim.agents.add_to_set(set, agent);
        }
        if party.is_location {
            sim.agents.add_to_set(Set::Locations, agent);
        }

        party.agent = agent;

        if !desc.key.is_empty() {
            keys.insert(desc.key, agent);
        }
        pass2.push((desc, agent));
    }

    for (desc, agent) in pass2 {
        for &(hierarchy, key) in desc.parents {
            let parent = keys.get(key).copied().unwrap();
            sim.agents.set_parent(hierarchy, parent, agent);
        }
    }
}
