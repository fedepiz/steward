use bumpalo::Bump;
use slotmap::{Key, KeyData};
use util::string_pool::{SpanHandle, StringPool};

use crate::{
    entities::{Body, Entities, EntityId},
    geom::V2,
    movement,
    names::Names,
    objects::{Objects, ObjectsBuilder},
    terrain_map::TerrainMap,
};

#[derive(Default)]
pub(crate) struct Simulation {
    pub turn_num: usize,
    pub names: Names,
    pub entities: Entities,
    pub terrain_map: TerrainMap,
    movement_cache: movement::MovementCache,
}

impl Simulation {
    pub(crate) fn tick(&mut self, mut request: Request, arena: &Bump) -> Response {
        if let Some(req) = request.init.take() {
            *self = Self::default();
            crate::init::init(self, req);
        }

        let advance_time = request.advance_time;

        if advance_time {
            self.turn_num = self.turn_num.wrapping_add(1);

            for entity in self.entities.iter_mut() {
                entity.destination = V2::new(500., 500.);
            }
            if let Some(third_entity) = self.entities.iter().nth(2).map(|entity| entity.id) {
                let target = self.entities.iter().next().map(|x| x.body.pos).unwrap();
                self.entities[third_entity].destination = target;
            }

            let elements = {
                let it = self.entities.iter().map(|entity| movement::Element {
                    id: entity.id,
                    speed: 1.,
                    pos: entity.body.pos,
                    destination: entity.destination,
                });
                arena.alloc_slice_fill_iter(it)
            };

            let result =
                movement::tick_movement(&mut self.movement_cache, &elements, &self.terrain_map);

            for (id, pos) in result.positions {
                self.entities[id].body.pos = pos;
            }
        }

        let mut response = Response::default();
        view(self, &request, &mut response);

        response
    }
}

impl movement::MovementGraph for TerrainMap {
    fn size(&self) -> (usize, usize) {
        self.size()
    }

    fn get_speed_at(&self, x: i64, y: i64) -> f32 {
        let x = x.max(0) as usize;
        let y = y.max(0) as usize;
        let terrain = self.terrain_at(x, y);
        terrain.movement_speed_multiplier
    }
}

#[derive(Default)]
pub struct Request {
    pub init: Option<InitRequest>,
    pub advance_time: bool,
    strings: StringPool,
    view_entities: Vec<ViewEntity>,
}

#[derive(Default)]
pub struct InitRequest {
    pub map_width: usize,
    pub map_height: usize,
    pub elevations: Vec<u8>,
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

    response.map_terrain.hash = sim.terrain_map.hash();
    response.map_terrain.size = sim.terrain_map.size();
    response.map_terrain.tiles = sim
        .terrain_map
        .iter_terrains()
        .map(|typ| typ.color)
        .collect();
}

#[derive(Default)]
pub struct Response {
    pub objects: Objects,
    pub map_items: MapItems,
    pub map_terrain: MapTerrain,
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
            x: data.body.pos.x,
            y: data.body.pos.y,
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

#[derive(Default)]
pub struct MapTerrain {
    pub hash: u64,
    pub size: (usize, usize),
    pub tiles: Vec<(u8, u8, u8)>,
}
