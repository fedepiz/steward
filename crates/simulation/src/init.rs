use crate::geom::*;
use crate::names::Name;
use crate::simulation::*;

pub(crate) fn init(sim: &mut Simulation) {
    {
        let person = sim.entities.add_type();
        person.tag = "person";
        person.name = Name::simple(sim.names.define("Person"));
        person.size = V2::splat(1.0);
    }

    for pos in [V2::splat(0.), V2::new(-5., -6.)] {
        let typ = sim.entities.find_type_by_tag("person").unwrap();
        let entity = sim.entities.spawn_with_type(typ.id);
        entity.name = typ.name;
        entity.body.pos = pos;
    }
}
