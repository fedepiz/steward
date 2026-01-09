use slotmap::Key;
use util::string_pool::{SpanHandle, StringPool};

use crate::{
    entities::{Body, Entities},
    geom::V2,
    objects::{Objects, ObjectsBuilder},
};

#[derive(Default)]
pub(crate) struct Simulation {
    pub turn_num: usize,
    pub entities: Entities,
}

impl Simulation {
    pub(crate) fn tick(&mut self, request: Request) -> Response {
        if request.init {
            *self = Self::default();
            init(self);
        }

        if request.advance_time {
            self.turn_num = self.turn_num.wrapping_add(1);
        }

        let mut response = Response::default();
        extract(self, &request, &mut response);

        response
    }
}

#[derive(Default)]
pub struct Request {
    pub init: bool,
    pub advance_time: bool,
}

fn init(sim: &mut Simulation) {
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
}

fn extract(sim: &Simulation, _: &Request, response: &mut Response) {
    let mut ctx = ObjectsBuilder::default();
    // Create a default zero object, so that ObjectId::default() is valid but unused
    ctx.spawn(|_| {});
    // Create the 'global' object
    ctx.spawn(|ctx| {
        ctx.tag("root");
        ctx.fmt("tick_num", format_args!("Turn number: {}", sim.turn_num));
    });
    response.objects = ctx.build();

    let map_items = &mut response.map_items;
    for entity in sim.entities.iter() {
        // TODO: Filter here for being in view
        let typ = sim.entities.get_type(entity.type_id);

        map_items.entries.push(MapItemData {
            id: entity.id.data().as_ffi(),
            name: SpanHandle::default(),
            image: typ.tag,
            body: entity.body,
        });
    }
}

#[derive(Default)]
pub struct Response {
    pub objects: Objects,
    pub map_items: MapItems,
}

#[derive(Default)]
pub struct MapItems {
    names: StringPool,
    entries: Vec<MapItemData>,
}

impl MapItems {
    fn get(&self, data: &MapItemData) -> MapItem<'_> {
        MapItem {
            id: MapItemId(data.id),
            name: self.names.get(data.name),
            image: data.image,
            x: data.body.pos.x - data.body.size.x / 2.,
            y: data.body.pos.y - data.body.size.y / 2.,
            width: data.body.size.x,
            height: data.body.size.y,
        }
    }

    pub fn get_by_index(&self, index: usize) -> MapItem<'_> {
        let data = self.entries.get(index).copied().unwrap_or_default();
        self.get(&data)
    }

    pub fn iter(&self) -> impl Iterator<Item = MapItem<'_>> + use<'_> {
        self.entries.iter().map(|data| self.get(data))
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MapItemId(pub u64);

#[derive(Default, Clone, Copy)]
struct MapItemData {
    id: u64,
    name: SpanHandle,
    image: &'static str,
    body: Body,
}

pub struct MapItem<'a> {
    pub id: MapItemId,
    pub name: &'a str,
    pub image: &'a str,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

mod csr_map {}
