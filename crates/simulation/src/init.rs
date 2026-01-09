use crate::geom::*;
use crate::simulation::*;

pub(crate) fn init(sim: &mut Simulation) {
    const MAP_WIDTH: usize = 3200;
    const MAP_HEIGHT: usize = 3200;

    sim.movement = crate::movement::MovementSystem::new(MAP_WIDTH, MAP_HEIGHT);

    {
        let person = sim.entities.add_type();
        person.tag = "person";
        person.name = "Person";
        person.size = V2::splat(1.0);
    }

    for pos in [V2::splat(0.), V2::new(-5., -6.)] {
        let type_id = sim.entities.find_type_by_tag("person").unwrap();
        let entity = sim.entities.spawn_with_type(type_id);
        entity.body.pos = pos;
    }

    sim.movement.insert(
        &sim.entities
            .iter()
            .map(|entity| (entity.id, entity.body.pos, 1.0))
            .collect::<Vec<_>>(),
    );
}
