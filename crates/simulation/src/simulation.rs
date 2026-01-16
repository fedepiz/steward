use bumpalo::Bump;
use slotmap::{Key, KeyData};
use util::string_pool::{SpanHandle, StringPool};

use crate::{
    entities::{Body, Detection, Detections, Entities, EntityId, MovementTarget},
    geom::V2,
    movement::{self, SpatialMap},
    names::Names,
    objects::{Objects, ObjectsBuilder},
    terrain_map::TerrainMap,
};

#[derive(Default)]
pub(crate) struct Simulation {
    pub turn_num: usize,
    pub terrain_map: TerrainMap,
    pub names: Names,
    pub entities: Entities,
    movement_cache: movement::MovementCache,
}

impl Simulation {
    pub(crate) fn tick(&mut self, request: Request, _: &Bump) -> Response {
        let _span = tracing::info_span!("Tick").entered();
        tick(self, request)
    }
}

fn tick(sim: &mut Simulation, mut request: Request) -> Response {
    if let Some(req) = request.init.take() {
        *sim = Simulation::default();
        crate::init::init(sim, req);
    }

    // Input handling
    {
        let mut command = None;

        // From move pos
        if let Some((x, y)) = request.move_to_pos.take() {
            command = Some(MovementTarget::FixedPos(V2::new(x, y)));
        }

        // From move to target
        if let Some(item_id) = request.move_to_item.take() {
            let id = EntityId::from(KeyData::from_ffi(item_id.0));
            command = Some(MovementTarget::Follow(id));
        }

        // Update entity positions
        for entity in sim.entities.iter_mut() {
            entity.movement_target = if entity.is_player {
                command.unwrap_or(entity.movement_target)
            } else {
                MovementTarget::FixedPos(V2::new(500., 500.))
            }
        }
    }

    if request.advance_time {
        sim.turn_num = sim.turn_num.wrapping_add(1);

        let mut movement_elements = Vec::with_capacity(sim.entities.len());

        // Extract data from entities in linear pass
        for entity in sim.entities.iter() {
            let destination = match entity.movement_target {
                MovementTarget::Immobile => entity.body.pos,
                MovementTarget::FixedPos(pos) => pos,
                MovementTarget::Follow(id) => sim.entities[id].body.pos,
            };

            movement_elements.push(movement::Element {
                id: entity.id,
                speed: entity.speed,
                pos: entity.body.pos,
                destination,
            });
        }

        // Apply movement results back to entities
        let movement_result = movement::tick_movement(
            &mut sim.movement_cache,
            &movement_elements,
            &sim.terrain_map,
        );

        // Detection check
        {
            let spatial_map = &movement_result.spatial_map;
            let mut detections = std::mem::take(&mut sim.entities.detections);
            self::detections(&mut detections, &sim.entities, spatial_map);
            sim.entities.detections = detections;
        }

        // Iterate writeback
        {
            let mut positions = movement_result.positions.into_iter();

            for entity in sim.entities.iter_mut() {
                let (id, pos) = positions.next().unwrap();
                assert!(id == entity.id);
                entity.body.pos = pos;
            }
        }
    }

    let mut response = Response::default();
    view(sim, &request, &mut response);
    response
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
    pub move_to_pos: Option<(f32, f32)>,
    pub move_to_item: Option<MapItemId>,
    pub higlighted_item: Option<MapItemId>,
    pub extract_terrain: bool,
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

fn detections(detections: &mut Detections, entities: &Entities, spatial_map: &SpatialMap) {
    detections.clear();

    let mut scratch = Vec::with_capacity(100);
    for entity in entities.iter() {
        let detection_range = 1.;
        let neighbours = spatial_map.search(entity.body.pos, detection_range);
        for target in neighbours {
            // Do not look at yourself, skip "disembodied" entities
            if target == entity.id {
                continue;
            }
            let target = &entities[target];
            let distance = V2::distance(entity.body.pos, target.body.pos);
            let range = (entity.body.size + target.body.size) / 2.;

            // Non-colliding
            let collides = distance <= range;

            scratch.push(Detection {
                target: target.id,
                distance,
                collides,
            });
        }
        detections.set(entity.id, &scratch);
        scratch.clear();
    }
}

#[derive(Clone, Copy)]
struct ViewEntity {
    tag: SpanHandle,
    entity: EntityId,
}

fn view(sim: &Simulation, req: &Request, response: &mut Response) {
    let _span = tracing::info_span!("View").entered();

    {
        let _span = tracing::info_span!("Objects").entered();
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
    }

    {
        let _span = tracing::info_span!("Map Items").entered();
        let ctx = &mut response.map_items;
        let highlighted_entity = req
            .higlighted_item
            .map(|id| id.as_entity())
            .unwrap_or_default();

        for entity in sim.entities.iter() {
            // TODO: Filter here for being in view
            let typ = sim.entities.get_type(entity.type_id);

            let is_highlighted = highlighted_entity == entity.id;

            let name = if is_highlighted {
                let name = sim.names.resolve(entity.name);
                ctx.names.push(name)
            } else {
                Default::default()
            };

            ctx.entries.push(MapItemData {
                id: entity.id.data().as_ffi(),
                name,
                image: typ.tag,
                body: entity.body,
            });
        }
    }

    if req.extract_terrain {
        let _span = tracing::info_span!("Map Terrain").entered();
        let mut map_terrain = MapTerrain::default();
        map_terrain.hash = sim.terrain_map.hash();
        map_terrain.size = sim.terrain_map.size();
        map_terrain.tiles = sim
            .terrain_map
            .iter_terrains()
            .map(|typ| typ.color)
            .collect();
        response.map_terrain = Some(map_terrain);
    }
}

#[derive(Default)]
pub struct Response {
    pub objects: Objects,
    pub map_items: MapItems,
    pub map_terrain: Option<MapTerrain>,
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
            width: data.body.size,
            height: data.body.size,
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

impl MapItemId {
    pub(crate) fn as_entity(self) -> EntityId {
        EntityId::from(KeyData::from_ffi(self.0))
    }
}

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
