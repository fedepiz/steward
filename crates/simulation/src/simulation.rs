use bumpalo::Bump;
use slotmap::{Key, KeyData};
use util::string_pool::{SpanHandle, StringPool};

use crate::{
    entities::{Body, Entities, EntityId},
    geom::V2,
    movement,
    names::{Name, Names},
    objects::{Objects, ObjectsBuilder},
};

#[derive(Default)]
pub(crate) struct Simulation {
    pub turn_num: usize,
    pub names: Names,
    pub entities: Entities,
    movement_cache: movement::MovementCache,
}

impl Simulation {
    pub(crate) fn tick(&mut self, request: Request, arena: &Bump) -> Response {
        if request.init {
            *self = Self::default();
            crate::init::init(self);
        }

        let advance_time = request.advance_time;

        if advance_time {
            self.turn_num = self.turn_num.wrapping_add(1);

            let elements = {
                let it = self.entities.iter().map(|entity| movement::Element {
                    id: entity.id,
                    speed: 1.,
                    pos: entity.body.pos,
                    destination: V2::splat(0.),
                });
                arena.alloc_slice_fill_iter(it)
            };

            let map_def = movement::MapInfo {
                width: 1000,
                height: 1000,
            };

            let result = movement::tick_movement(&mut self.movement_cache, &elements, &map_def);

            for (id, pos) in result.positions {
                self.entities[id].body.pos = pos;
            }
        }

        let mut response = Response::default();
        view(self, &request, &mut response);

        response
    }
}

#[derive(Default)]
pub struct Request {
    pub init: bool,
    pub advance_time: bool,
    strings: StringPool,
    view_entities: Vec<ViewEntity>,
}

impl Request {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn view_map_item(&mut self, key: &str, id: MapItemId) {
        let entity = EntityId::from(KeyData::from_ffi(id.0));
        self.view_entity(key, entity);
    }

    fn view_entity(&mut self, key: &str, entity: EntityId) {
        let key = self.strings.push_str(key);
        self.view_entities.push(ViewEntity { tag: key, entity });
    }
}

#[derive(Clone, Copy)]
struct ViewEntity {
    tag: SpanHandle,
    entity: EntityId,
}

fn view(sim: &Simulation, req: &Request, response: &mut Response) {
    let mut ctx = ObjectsBuilder::default();
    // Create a default zero object, so that ObjectId::default() is valid but unused
    ctx.spawn(|_| {});
    // Create the 'global' object
    ctx.spawn(|ctx| {
        ctx.tag("root");
        ctx.fmt("tick_num", format_args!("Turn number: {}", sim.turn_num));
    });

    // Create requested objects
    for view_entity in &req.view_entities {
        let entity = &sim.entities[view_entity.entity];
        ctx.spawn(|ctx| {
            let tag = req.strings.get(view_entity.tag);
            ctx.tag(tag);
            ctx.display("name", sim.names.resolve(entity.name));
        });
    }

    response.objects = ctx.build();

    let map_items = &mut response.map_items;
    for entity in sim.entities.iter() {
        // TODO: Filter here for being in view
        let typ = sim.entities.get_type(entity.type_id);

        map_items.entries.push(MapItemData {
            id: entity.id.data().as_ffi(),
            name: Default::default(),
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
