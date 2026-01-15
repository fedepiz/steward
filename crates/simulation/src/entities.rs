use std::rc::Rc;

use slotmap::{SecondaryMap, SlotMap, new_key_type};
use util::span::Span;

use crate::{geom::V2, names::Name};

new_key_type! { pub(crate) struct EntityId; }
new_key_type! { pub(crate) struct EntityTypeId; }

#[derive(Default)]
pub(crate) struct Entities {
    types: SlotMap<EntityTypeId, EntityType>,
    entities: SlotMap<EntityId, Entity>,
    /// Retains a 'synched' copy of all the ids, in order, in the structure, behind a reference counted pointer.
    /// Used to iterate whithout needing mutable access to the underlying structure (as a Rc clone is cheap).
    /// Spawning a new entity while holding a clone of EntityIds is illegal: it will result in a panic
    /// (though mabye the panci can be avoided if we are ok with a copy-on-write)
    iterable_ids: EntityIds,
    pub(crate) detections: Detections,
}

#[derive(Default, Clone)]
pub(crate) struct EntityIds(Rc<SecondaryMap<EntityId, ()>>);

impl EntityIds {
    #[must_use]
    fn insert(&mut self, id: EntityId) -> Option<()> {
        Rc::get_mut(&mut self.0)?.insert(id, ());
        Some(())
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = EntityId> + use<'_> {
        self.0.keys()
    }
}

#[derive(Default)]
pub(crate) struct Detections {
    data: Vec<Detection>,
    spans: SecondaryMap<EntityId, Span>,
}

impl Detections {
    pub(crate) fn clear(&mut self) {
        self.data.clear();
        self.spans.clear();
    }

    pub(crate) fn set(&mut self, id: EntityId, detections: &[Detection]) {
        let detections = detections.iter().copied();
        let span = Span::of_extension(&mut self.data, detections);
        self.spans.insert(id, span);
    }

    pub(crate) fn get(&self, id: EntityId) -> &[Detection] {
        self.spans
            .get(id)
            .map(|span| span.view(&self.data))
            .unwrap_or_default()
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Detection {
    pub target: EntityId,
    pub distance: f32,
    pub collides: bool,
}

impl Entities {
    pub(crate) fn spawn(&mut self) -> &mut Entity {
        let id = self.entities.insert(Entity::default());
        let data = &mut self.entities[id];
        self.iterable_ids.insert(id).unwrap();
        data.id = id;
        data
    }

    pub(crate) fn spawn_with_type(&mut self, type_id: EntityTypeId) -> &mut Entity {
        let typ = self.types.get(type_id).copied().unwrap();
        let entity = self.spawn();
        Self::set_type(entity, &typ);
        entity
    }

    pub(crate) fn acquire_ids(&self) -> EntityIds {
        self.iterable_ids.clone()
    }

    fn set_type(entity: &mut Entity, typ: &EntityType) {
        entity.type_id = typ.id;
        entity.body.size = typ.size;
    }

    pub(crate) fn add_type(&mut self) -> &mut EntityType {
        let id = self.types.insert(EntityType::default());
        let data = &mut self.types[id];
        data.id = id;
        data
    }

    pub(crate) fn find_type_by_tag(&self, tag: &str) -> Option<EntityType> {
        self.types.values().find(|typ| typ.tag == tag).copied()
    }

    pub(crate) fn get_type(&self, id: EntityTypeId) -> EntityType {
        self.types.get(id).copied().unwrap_or_default()
    }

    pub(crate) fn iter(&self) -> slotmap::basic::Values<'_, EntityId, Entity> {
        self.entities.values()
    }

    pub(crate) fn iter_mut(&mut self) -> slotmap::basic::ValuesMut<'_, EntityId, Entity> {
        self.entities.values_mut()
    }

    pub(crate) fn len(&self) -> usize {
        self.entities.len()
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
    pub name: Name,
    pub size: f32,
}

#[derive(Default, Clone, Copy)]
pub(crate) struct Entity {
    pub id: EntityId,
    pub name: Name,
    pub type_id: EntityTypeId,
    pub body: Body,
    pub speed: f32,
    pub movement_target: MovementTarget,
    pub is_player: bool,
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum MovementTarget {
    Immobile,
    FixedPos(V2),
    Follow(EntityId),
}

impl Default for MovementTarget {
    fn default() -> Self {
        Self::Immobile
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Body {
    pub pos: V2,
    pub size: f32,
}
