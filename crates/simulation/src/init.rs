use crate::names::Name;
use crate::simulation::*;
use crate::{geom::*, terrain_map};

pub(crate) fn init(sim: &mut Simulation, req: InitRequest) {
    sim.terrain_map = terrain_map::init(&req.elevations, req.map_width, req.map_height);

    {
        let person = sim.entities.add_type();
        person.tag = "person";
        person.name = Name::simple(sim.names.define("Person"));
        person.size = V2::splat(1.0);
    }

    let descs = [
        V2::new(580., 520.),
        V2::new(610., 520.),
        V2::new(600., 520.),
        V2::new(600., 500.),
    ];

    let player_name = Name::simple(sim.names.define("Player"));

    for (idx, pos) in descs.into_iter().enumerate() {
        let typ = sim.entities.find_type_by_tag("person").unwrap();
        let entity = sim.entities.spawn_with_type(typ.id);
        let is_player = idx == 0;
        entity.is_player = is_player;

        entity.name = if is_player { player_name } else { typ.name };
        entity.body.pos = pos;
    }
}
