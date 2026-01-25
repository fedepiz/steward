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
        typ.image = "person";
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = BASE_SPEED;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "farmers";
        typ.image = "farmers";
        typ.name = Name::simple(sim.names.define("Farmers"));
        typ.size = SIZE_SMALL;
        typ.speed = 1.;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "miners";
        typ.image = "miners";
        typ.name = Name::simple(sim.names.define("Miners"));
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "caravan";
        typ.image = "caravan";
        typ.name = Name::simple(sim.names.define("Caravan"));
        typ.size = SIZE_SMALL;
        typ.layer = 1;
        typ.speed = WALK_SPEED;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "mine";
        typ.image = "mine";
        typ.name = Name::simple(sim.names.define("Mine"));
        typ.speed = 0.;
        typ.size = 2.5;
        typ.layer = 0;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "village";
        typ.image = "village";
        typ.name = Name::simple(sim.names.define("Village"));
        typ.size = 3.0;
        typ.always_show_name = true;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "hillfort";
        typ.image = "hillfort";
        typ.name = Name::simple(sim.names.define("Hillfort"));
        typ.size = 3.;
        typ.always_show_name = true;
    }

    {
        let typ = sim.parties.add_type();
        typ.tag = "town";
        typ.image = "town";
        typ.name = Name::simple(sim.names.define("Town"));
        typ.size = 4.;
        typ.always_show_name = true;
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
    struct AgentDesc<'a> {
        key: &'a str,
        name: &'a str,
        pos: (f32, f32),
        party_typ: &'a str,
        vars: &'a [(Var, f64)],
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

    let agents = [
        AgentDesc {
            pos: (590., 520.),
            party_typ: "person",
            name: "Ambrosius Aurelianus",
            sets: &[Set::People],
            vars: &[(Var::Renown, PERSON_RENOWN)],
            is_player: true,
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
            pos: (600., 500.),
            party_typ: "person",
            name: "Eadwine map Owain",
            sets: &[Set::People],
            vars: &[(Var::Renown, PERSON_RENOWN)],
            parents: &[(Hierarchy::FactionMembership, "rheged")],
            ..Default::default()
        },
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
            name: "Lligwy Mine",
            pos: (600., 530.),
            party_typ: "mine",
            sets: &[Set::Mines],
            ..Default::default()
        },
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        AgentDesc {
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
        let agent = sim.agents.spawn();
        agent.name = Name::simple(sim.names.define(faction.name));
        let agent = agent.id;
        sim.agents.add_to_set(Set::Factions, agent);
        sim.faction_colors.insert(agent, faction.color);
        keys.insert(faction.key, agent);
    }

    for desc in agents {
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
        agent.fixed_behavior = if desc.is_player {
            Some(Behavior::Player)
        } else {
            None
        };
        agent.party = party.id;
        party.agent = agent.id;

        // Settlements are locations

        let agent = agent.id;

        sim.agents.vars_mut(agent).set_many(desc.vars);

        for &set in desc.sets {
            sim.agents.add_to_set(set, agent);
        }

        let is_location = LOCATION_SETS.iter().any(|set| desc.sets.contains(set));
        if is_location {
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

        for &(hierarchy, key) in desc.children {
            let child = keys.get(key).copied().unwrap();
            sim.agents.set_parent(hierarchy, agent, child);
        }
    }
}
