use slotmap::{SlotMap, new_key_type};

use crate::geom::V2;

new_key_type! { pub(crate) struct EntityId; }
new_key_type! { pub(crate) struct EntityTypeId; }

#[derive(Default)]
pub(crate) struct Entities {
    types: SlotMap<EntityTypeId, EntityType>,
    entities: SlotMap<EntityId, Entity>,
}

impl Entities {
    pub(crate) fn spawn(&mut self) -> &mut Entity {
        let id = self.entities.insert(Entity::default());
        let data = &mut self.entities[id];
        data.id = id;
        data
    }

    pub(crate) fn spawn_with_type(&mut self, type_id: EntityTypeId) -> &mut Entity {
        let id = self.entities.insert(Entity::default());
        self.set_type(id, type_id);
        let data = &mut self.entities[id];
        data.id = id;
        data
    }

    pub(crate) fn set_type(&mut self, id: EntityId, type_id: EntityTypeId) {
        let typ = self.get_type(type_id);

        let entity = match self.entities.get_mut(id) {
            Some(data) => data,
            None => {
                return;
            }
        };

        entity.type_id = typ.id;
        entity.body.size = typ.size;
    }

    pub(crate) fn add_type(&mut self) -> &mut EntityType {
        let id = self.types.insert(EntityType::default());
        let data = &mut self.types[id];
        data.id = id;
        data
    }

    pub(crate) fn find_type_by_tag(&self, tag: &str) -> Option<EntityTypeId> {
        self.types.values().find(|typ| typ.tag == tag).map(|x| x.id)
    }

    pub(crate) fn get_type(&self, id: EntityTypeId) -> EntityType {
        self.types.get(id).copied().unwrap_or_default()
    }

    pub(crate) fn iter(&self) -> slotmap::basic::Values<'_, EntityId, Entity> {
        self.entities.values()
    }
}

impl std::ops::Index<EntityId> for Entities {
    type Output = Entity;

    fn index(&self, index: EntityId) -> &Self::Output {
        &self.entities[index]
    }
}

impl std::ops::IndexMut<EntityId> for Entities {
    fn index_mut(&mut self, index: EntityId) -> &mut Self::Output {
        &mut self.entities[index]
    }
}

impl std::ops::Index<EntityTypeId> for Entities {
    type Output = EntityType;

    fn index(&self, index: EntityTypeId) -> &Self::Output {
        &self.types[index]
    }
}

impl std::ops::IndexMut<EntityTypeId> for Entities {
    fn index_mut(&mut self, index: EntityTypeId) -> &mut Self::Output {
        &mut self.types[index]
    }
}

#[derive(Default, Clone, Copy)]
pub(crate) struct EntityType {
    pub id: EntityTypeId,
    pub tag: &'static str,
    pub name: &'static str,
    pub size: V2,
}

#[derive(Default, Clone, Copy)]
pub(crate) struct Entity {
    pub id: EntityId,
    pub type_id: EntityTypeId,
    pub body: Body,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Body {
    pub pos: V2,
    pub size: V2,
}
