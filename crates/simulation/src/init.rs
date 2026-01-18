use crate::agents::{Set, Var};
use crate::names::Name;
use crate::simulation::*;
use crate::{geom::*, terrain_map};

pub(crate) fn init(sim: &mut Simulation, req: InitRequest) {
    sim.terrain_map = terrain_map::init(&req.elevations, req.map_width, req.map_height);

    {
        let typ = sim.parties.add_type();
        typ.tag = "person";
        typ.image = "pawns/person";
        typ.name = Name::simple(sim.names.define("Person"));
        typ.size = 2.0;
        typ.layer = 1;
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

    #[derive(Default)]
    struct Desc<'a> {
        name: &'a str,
        pos: (f32, f32),
        party_typ: &'a str,
        speed: f32,
        vars: &'a [(Var, f64)],
        sets: &'a [Set],
        is_player: bool,
    }

    let descs = [
        Desc {
            pos: (590., 520.),
            party_typ: "person",
            speed: 5.,
            vars: &[(Var::Renown, 10.)],
            is_player: true,
            ..Default::default()
        },
        Desc {
            pos: (610., 520.),
            party_typ: "person",
            speed: 1.,
            vars: &[(Var::Renown, 10.)],
            ..Default::default()
        },
        Desc {
            pos: (600., 520.),
            party_typ: "person",
            speed: 1.,
            vars: &[(Var::Renown, 10.)],
            ..Default::default()
        },
        Desc {
            pos: (600., 500.),
            party_typ: "person",
            speed: 1.,
            vars: &[(Var::Renown, 10.)],
            ..Default::default()
        },
        Desc {
            name: "Caer Ligualid",
            pos: (597., 509.),
            party_typ: "town",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Llan Heledd",
            pos: (580., 520.),
            party_typ: "village",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Din Drust",
            pos: (570., 490.),
            party_typ: "hillfort",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Din Reghed",
            pos: (530., 500.),
            party_typ: "hillfort",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Ad Candidam Casam",
            pos: (530., 520.),
            party_typ: "village",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Isura",
            pos: (560., 500.),
            party_typ: "village",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Anava",
            pos: (603., 500.),
            party_typ: "village",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
        Desc {
            name: "Caer Wenddoleu",
            pos: (625., 505.),
            party_typ: "hillfort",
            speed: 0.,
            vars: &[(Var::Prosperity, 0.5)],
            sets: &[Set::Settlements],
            ..Default::default()
        },
    ];

    let player_name = Name::simple(sim.names.define("Player"));
    for desc in descs {
        let typ = sim.parties.find_type_by_tag(desc.party_typ).unwrap();
        let party = sim.parties.spawn_with_type(typ.id);
        party.is_player = desc.is_player;
        party.name = if desc.is_player {
            player_name
        } else if desc.name.is_empty() {
            typ.name
        } else {
            Name::simple(sim.names.define(desc.name))
        };
        party.speed = desc.speed;
        party.body.pos = V2::new(desc.pos.0, desc.pos.1);

        let agent = sim.agents.spawn();
        agent.name = party.name;
        agent.party = party.id;
        party.agent = agent.id;
        let agent = agent.id;

        sim.agents.vars_mut(agent).set_many(desc.vars);

        for &set in desc.sets {
            sim.agents.add_to_set(set, agent);
        }

        party.agent = agent;
    }
}
