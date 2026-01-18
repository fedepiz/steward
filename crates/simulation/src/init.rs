use crate::agents::Var;
use crate::names::Name;
use crate::simulation::*;
use crate::{geom::*, terrain_map};

pub(crate) fn init(sim: &mut Simulation, req: InitRequest) {
    sim.terrain_map = terrain_map::init(&req.elevations, req.map_width, req.map_height);

    {
        let person = sim.parties.add_type();
        person.tag = "person";
        person.name = Name::simple(sim.names.define("Person"));
        person.size = 1.0;
    }

    let descs = [
        V2::new(580., 520.),
        V2::new(610., 520.),
        V2::new(600., 520.),
        V2::new(600., 500.),
    ];

    let player_name = Name::simple(sim.names.define("Player"));

    for (idx, pos) in descs.into_iter().enumerate() {
        let typ = sim.parties.find_type_by_tag("person").unwrap();
        let party = sim.parties.spawn_with_type(typ.id);
        let is_player = idx == 0;
        party.is_player = is_player;

        party.name = if is_player { player_name } else { typ.name };
        party.speed = if is_player { 5. } else { 1. };
        party.body.pos = pos;

        let agent = sim.agents.spawn();
        agent.name = party.name;
        agent.party = party.id;
        party.agent = agent.id;
        let agent = agent.id;

        sim.agents.vars_mut(agent).with(Var::Renown, 10.);

        party.agent = agent
    }
}
