use crate::names::Name;
use crate::simulation::*;
use crate::{geom::*, terrain_map};

pub(crate) fn init(sim: &mut Simulation, req: InitRequest) {
    sim.terrain_map = terrain_map::init(&req.elevations, req.map_width, req.map_height);

    {
        let person = sim.entities.add_type();
        person.tag = "person";
        person.name = Name::simple(sim.names.define("Person"));
        person.size = V2::splat(2.0);
    }

    for pos in [V2::splat(0.), V2::new(-5., -6.)] {
        let typ = sim.entities.find_type_by_tag("person").unwrap();
        let entity = sim.entities.spawn_with_type(typ.id);
        entity.name = typ.name;
        entity.body.pos = pos;
    }
}
