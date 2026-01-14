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

    for pos in [
        V2::new(580., 520.),
        V2::new(610., 520.),
        V2::new(600., 520.),
        V2::new(600., 500.),
    ] {
        let typ = sim.entities.find_type_by_tag("person").unwrap();
        let entity = sim.entities.spawn_with_type(typ.id);
        entity.name = typ.name;
        entity.body.pos = pos;
        entity.destination = entity.body.pos;
    }

    if let Some(entity) = sim.entities.iter_mut().next() {
        entity.is_player = true
    }
}
