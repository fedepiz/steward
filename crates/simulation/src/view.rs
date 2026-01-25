use slotmap::Key;
use slotmap::KeyData;
use util::string_pool::{SpanHandle, StringPool};

use crate::agents::{self, *};
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
    pub(crate) fn as_entity(self) -> AgentId {
        AgentId::from(KeyData::from_ffi(self.0))
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

        // Create requested objects
        for &view_entity in &req.view_entities {
            let agent = match sim.agents.get(view_entity.entity) {
                Some(x) => x,
                None => {
                    continue;
                }
            };
            ctx.spawn(|ctx| {
                let tag = req.entity_tag(view_entity);
                ctx.tag(tag);
                ctx.display("name", sim.names.resolve(agent.name));

                {
                    // Display food
                    let stored = agent.get_var(Var::FoodStored);
                    if agent.in_set(Set::Settlements) {
                        let capacity = agent.get_var(Var::FoodCapacity);
                        ctx.display("food", format_args!("{stored}/{capacity}"));
                    } else {
                        ctx.display("food", stored);
                    }
                }

                ctx.display("minerals", agent.get_var(Var::Minerals));
                ctx.display("goods", agent.get_var(Var::Goods));

                if agent.in_set(agents::Set::People) {
                    ctx.display("renown", agent.get_var(Var::Renown));
                }

                if agent.in_set(agents::Set::Settlements) {
                    ctx.display("population", agent.get_var(Var::Population));
                    ctx.fmt(
                        "prosperity",
                        format_args!("{:1.2}%", agent.get_var(Var::Prosperity) * 100.),
                    );

                    ctx.display(
                        "farmer_opportunity",
                        format_args!("{:1.2}", agent.get_var(Var::FarmerOpportunity)),
                    );

                    ctx.display(
                        "miner_opportunity",
                        format_args!("{:1.2}", agent.get_var(Var::MinerOpportunity)),
                    );

                    ctx.display(
                        "caravan_opportunity",
                        format_args!("{:1.2}", agent.get_var(Var::CaravanOpportunity)),
                    );
                }
            });
        }

        response.objects = ctx.build();
    }

    {
        let _span = tracing::info_span!("Map Items").entered();
        let ctx = &mut response.map_items;
        let highlighted_entity = req
            .highlighted_item
            .map(|id| id.as_entity())
            .unwrap_or_default();

        // Get parties and types, sorted by layer
        let mut agents: Vec<_> = sim
            .agents
            .iter()
            .filter(|agent| !agent.get_flag(Flag::IsDisembodied) && !agent.get_flag(Flag::IsInside))
            .map(|agent| (agent, sim.agents.get_type(agent.type_id)))
            .collect();

        agents.sort_by_key(|(_, typ)| typ.layer);

        for (agent, typ) in agents {
            // TODO: Filter here for being in view

            let is_highlighted = highlighted_entity == agent.id;
            let show_name = is_highlighted || typ.always_show_name;

            let name = if show_name {
                let name = sim.names.resolve(agent.name);
                ctx.names.push(name)
            } else {
                Default::default()
            };

            let faction = sim.agents.parent_of(Hierarchy::FactionMembership, agent.id);

            let color = sim
                .faction_colors
                .get(faction)
                .copied()
                .unwrap_or((200, 200, 200));

            ctx.entries.push(MapItemData {
                id: agent.id.data().as_ffi(),
                name,
                image: typ.image,
                body: agent.body,
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
