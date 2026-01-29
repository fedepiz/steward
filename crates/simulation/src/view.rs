use slotmap::Key;
use slotmap::KeyData;
use util::string_pool::{SpanHandle, StringPool};

use crate::entities::{self, *};
use crate::names::Name;
use crate::objects::*;
use crate::simulation::*;

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
            x: data.body.pos.x - data.body.size / 2.,
            y: data.body.pos.y - data.body.size / 2.,
            width: data.body.size,
            height: data.body.size,
            color: data.color,
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
    color: Color,
}

pub struct MapItem<'a> {
    pub id: MapItemId,
    pub name: &'a str,
    pub image: &'static str,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub color: Color,
}

#[derive(Default)]
pub struct MapTerrain {
    pub hash: u64,
    pub size: (usize, usize),
    pub tiles: Vec<(u8, u8, u8)>,
}
pub(crate) fn view(sim: &Simulation, req: &Request, response: &mut Response) {
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

        if sim.dialog.has_interaction() {
            sim.dialog.as_object(&mut ctx);
        }

        // Create requested objects
        for &view_entity in &req.view_entities {
            let entity = match sim.entities.get(view_entity.entity) {
                Some(x) => x,
                None => {
                    continue;
                }
            };
            ctx.spawn(|ctx| {
                let tag = req.entity_tag(view_entity);
                ctx.tag(tag);
                ctx.display("name", sim.names.resolve(entity.name));

                {
                    // Display food
                    let stored = entity.get_var(Var::FoodStored);
                    if entity.in_set(Set::Settlements) {
                        let capacity = entity.get_var(Var::FoodCapacity);
                        ctx.display("food", format_args!("{stored}/{capacity}"));
                    } else {
                        ctx.display("food", stored);
                    }
                }

                for (key, var) in [
                    ("minerals", Var::Minerals),
                    ("goods", Var::Goods),
                    ("civilians", Var::Civilians),
                    ("soldiers", Var::Soldiers),
                ] {
                    let value = entity.get_var(var);
                    if value == 0. {
                        continue;
                    }
                    ctx.display(key, entity.get_var(var));
                }

                {
                    let (prefix, target) = match entity.location {
                        Location::Nowhere => ("Nowhere", EntityId::null()),
                        Location::FarOut => ("Far Out", EntityId::null()),
                        Location::Near(x) => ("Near ", x),
                        Location::Proximate(x) => ("Proximate ", x),
                        Location::Inside(x) => ("Inside ", x),
                    };
                    let name = if target.is_null() {
                        Name::default()
                    } else {
                        sim.entities[target].name
                    };
                    let name = sim.names.resolve(name);
                    ctx.fmt("location", format_args!("{prefix}{name}"));
                }

                if entity.in_set(entities::Set::People) {
                    ctx.display("renown", entity.get_var(Var::Renown));
                }

                if entity.in_set(entities::Set::Settlements) {
                    ctx.display("population", entity.get_var(Var::Population));
                    ctx.fmt(
                        "prosperity",
                        format_args!("{:1.2}%", entity.get_var(Var::Prosperity) * 100.),
                    );

                    ctx.display(
                        "farmer_opportunity",
                        format_args!("{:1.2}", entity.get_var(Var::FarmerOpportunity)),
                    );

                    ctx.display(
                        "miner_opportunity",
                        format_args!("{:1.2}", entity.get_var(Var::MinerOpportunity)),
                    );

                    ctx.display(
                        "caravan_opportunity",
                        format_args!("{:1.2}", entity.get_var(Var::CaravanOpportunity)),
                    );

                    ctx.list("contents", |ctx| {
                        for child in sim.entities.children_of(Hierarchy::Container, entity.id) {
                            ctx.spawn(|ctx| {
                                let child = &sim.entities[child];
                                ctx.handle("id", child.id.data().as_ffi());
                                ctx.display("name", sim.names.resolve(child.name));
                            });
                        }
                    });
                }
            });
        }

        response.objects = ctx.build();
    }

    {
        let _span = tracing::info_span!("Map Items").entered();
        let ctx = &mut response.map_items;
        let highlighted_entity = req.highlighted_item.as_entity();

        // Get parties and types, sorted by layer
        let mut entities: Vec<_> = sim
            .entities
            .iter()
            .filter(|entity| {
                !entity.get_flag(Flag::IsDisembodied) && !entity.get_flag(Flag::IsInside)
            })
            .map(|entity| (entity, sim.entities.get_type(entity.type_id)))
            .collect();

        entities.sort_by_key(|(_, typ)| typ.layer);

        for (entity, typ) in entities {
            // TODO: Filter here for being in view

            let is_highlighted = highlighted_entity == entity.id;
            let show_name = is_highlighted || typ.always_show_name;

            let name = if show_name {
                let name = sim.names.resolve(entity.name);
                ctx.names.push(name)
            } else {
                Default::default()
            };

            let faction = sim
                .entities
                .parent_of(Hierarchy::FactionMembership, entity.id);

            let color = sim
                .faction_colors
                .get(faction)
                .copied()
                .unwrap_or((200, 200, 200));

            ctx.entries.push(MapItemData {
                id: entity.id.data().as_ffi(),
                name,
                image: typ.image,
                body: entity.body,
                color,
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
