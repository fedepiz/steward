use std::collections::{BTreeMap, BTreeSet};

use crate::{OnArrival, geom::V2, names::Name};
use slotmap::*;
use strum::*;
use util::bitset::BitSet;

new_key_type! { pub struct EntityId; }
new_key_type! { pub struct ArchetypeId; }

#[derive(Default, Clone, Copy)]
pub(crate) struct Archetype {
    pub id: ArchetypeId,
    pub tag: &'static str,
    pub image: &'static str,
    pub name: Name,
    pub size: f32,
    pub speed: f32,
    pub vars: &'static [(Var, f64)],
    pub always_show_name: bool,
    pub layer: usize,
    pub is_disembodied: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter, Debug)]
pub(crate) enum Var {
    // Generic Party
    Civilians,
    Soldiers,
    Courage,
    Aggressiveness,
    // Person
    Money,
    Renown,
    // Timers
    FrozenTimer,
    // Settlement
    Population,
    Prosperity,
    FoodStored,
    FoodCapacity,
    Goods,
    Minerals,
    // Opportunities
    FarmerOpportunity,
    MinerOpportunity,
    CaravanOpportunity,
    // Autorecruiting
    AutoRecruitBase,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Set {
    // Places
    Locations,
    Settlements,
    Villages,
    Hillforts,
    Towns,
    Mines,
    // Factions
    Factions,
    // People
    People,
    // Activities
    Battle,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Flag {
    // Type flags
    IsFarmer,
    IsMiner,
    IsCaravan,
    IsBandit,
    // An ephemeral agent is an agent that despawns if
    // it becomes "empty"
    IsEphemeral,
    // Status flag
    IsDisembodied,
    IsInside,
    IsActivityPartecipant,
    // Diplo flags
    IsGenerallyHostile,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter, Debug)]
pub(crate) enum Hierarchy {
    Container,
    // Economy
    Attachment,
    LocalMarket,
    WorkArea,
    // Politics
    FactionMembership,
    // The relationship between a lord (or lord-like person) and its various possessions
    Lordship,
    // The relationship between the ruling-lord and the ruled faction
    RulerOf,
    // The relationship between a location and the people that reside there
    HomeOf,
    // Battles
    ActivitySubject,
    ActivityTarget,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Relationship {
    Diplo,
    Placeholder1,
    Placeholder2,
}

#[derive(Default)]
pub(crate) struct Entities {
    null_dummy: Entity,
    types: SlotMap<ArchetypeId, Archetype>,
    entries: SlotMap<EntityId, Entity>,
    hierarchies: [HierarchyData; Hierarchy::COUNT],
    relationships: [RelationshipData; Relationship::COUNT],
    sets: [BTreeSet<EntityId>; Set::COUNT],
    despawn_queue: Vec<EntityId>,
}

impl Entities {
    pub(crate) fn spawn(&mut self) -> &mut Entity {
        let id = self.entries.insert(Entity::default());
        let entity = Entity {
            id,
            ..Default::default()
        };
        let entry = &mut self.entries[id];
        *entry = entity;
        entry
    }

    pub(crate) fn despawn(&mut self, id: EntityId) -> Option<Entity> {
        let entity = match self.entries.get_mut(id) {
            Some(entity) => std::mem::take(entity),
            None => {
                return None;
            }
        };

        for set_idx in entity.sets.iter() {
            self.sets[set_idx].remove(&entity.id);
        }

        for hierarchy in &mut self.hierarchies {
            hierarchy.remove_entity(entity.id);
        }

        for relationship in &mut self.relationships {
            relationship.remove_entity(entity.id);
        }

        self.despawn_queue.push(entity.id);

        Some(entity)
    }

    pub(crate) fn garbage_collect(&mut self) {
        for id in self.despawn_queue.drain(..) {
            self.entries.remove(id);
        }
    }

    pub(crate) fn spawn_with_type(&mut self, type_id: ArchetypeId) -> &mut Entity {
        let typ = self.types.get(type_id).copied().unwrap();
        let entity = self.spawn();
        Self::set_type(entity, &typ);
        entity
    }

    fn set_type(entity: &mut Entity, typ: &Archetype) {
        entity.type_id = typ.id;
        entity.name = typ.name;
        entity.body.size = typ.size;
        entity.speed = typ.speed;
        entity.flags.set(Flag::IsDisembodied, typ.is_disembodied);
        for &(var, value) in typ.vars {
            entity.set_var(var, value);
        }
    }

    pub(crate) fn add_type(&mut self) -> &mut Archetype {
        let id = self.types.insert(Archetype::default());
        let data = &mut self.types[id];
        data.id = id;
        data
    }

    pub(crate) fn find_type_by_tag(&self, tag: &str) -> Option<&Archetype> {
        self.types.values().find(|typ| typ.tag == tag)
    }

    pub(crate) fn get_type(&self, id: ArchetypeId) -> Archetype {
        self.types.get(id).copied().unwrap_or_default()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Entity> + ExactSizeIterator {
        self.entries.values()
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut Entity> + ExactSizeIterator {
        self.entries.values_mut()
    }

    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    pub(crate) fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    pub(crate) fn ids(&self) -> impl Iterator<Item = EntityId> + ExactSizeIterator {
        self.entries.keys()
    }

    pub(crate) fn get(&self, id: EntityId) -> Option<&Entity> {
        self.entries.get(id)
    }

    pub(crate) fn get_mut(&mut self, id: EntityId) -> Option<&mut Entity> {
        self.entries.get_mut(id)
    }

    pub fn vars_mut(&mut self, id: EntityId) -> VarsMut<'_> {
        self.entries
            .get_mut(id)
            .map(Entity::vars_mut)
            .unwrap_or_default()
    }

    pub fn add_to_set(&mut self, set: Set, entity: EntityId) -> bool {
        let idx = set as usize;
        self.entries[entity].sets.set(idx, true);
        self.sets[idx].insert(entity)
    }

    pub fn remove_from_set(&mut self, set: Set, entity: EntityId) -> bool {
        let idx = set as usize;
        self.entries[entity].sets.set(idx, false);
        self.sets[idx].remove(&entity)
    }

    pub fn iter_set_ids(&self, set: Set) -> impl Iterator<Item = EntityId> + use<'_> {
        self.sets[set as usize].iter().copied()
    }

    pub fn iter_set(&self, set: Set) -> impl Iterator<Item = &Entity> + use<'_> {
        self.iter_set_ids(set).map(|id| &self.entries[id])
    }

    pub fn in_set(&self, id: EntityId, set: Set) -> bool {
        self.entries[id].in_set(set)
    }

    pub fn set_parent(&mut self, hierarchy: Hierarchy, parent: EntityId, child: EntityId) {
        let h_data = &mut self.hierarchies[hierarchy as usize];
        h_data.change(parent, child);
    }

    pub fn remove_parent(&mut self, hierarchy: Hierarchy, child: EntityId) {
        let h_data = &mut self.hierarchies[hierarchy as usize];
        h_data.change(EntityId::null(), child);
    }

    pub fn children_of(
        &self,
        hierarchy: Hierarchy,
        parent: EntityId,
    ) -> impl Iterator<Item = EntityId> + use<'_> {
        let h_data = &self.hierarchies[hierarchy as usize];
        h_data.children_of(parent)
    }

    pub fn parent_of(&self, hierarchy: Hierarchy, child: EntityId) -> EntityId {
        let h_data = &self.hierarchies[hierarchy as usize];
        h_data.parent_of(child)
    }

    pub fn set_relationship(
        &mut self,
        relationship: Relationship,
        from: EntityId,
        to: EntityId,
        value: f32,
    ) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.set(from, to, value);
    }

    pub fn remove_relationship(
        &mut self,
        relationship: Relationship,
        from: EntityId,
        to: EntityId,
    ) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.remove(from, to);
    }

    pub fn relationship(
        &self,
        relationship: Relationship,
        from: EntityId,
        to: EntityId,
    ) -> Option<f32> {
        let rel_data = &self.relationships[relationship as usize];
        rel_data.get(from, to)
    }

    pub fn has_relationship(
        &self,
        relationship: Relationship,
        from: EntityId,
        to: EntityId,
    ) -> bool {
        self.relationship(relationship, from, to).is_some()
    }

    pub fn relationships_from(
        &self,
        relationship: Relationship,
        from: EntityId,
    ) -> impl Iterator<Item = (EntityId, f32)> + use<'_> {
        let rel_data = &self.relationships[relationship as usize];
        rel_data.from(from)
    }

    pub fn clear_relationships_from(&mut self, relationship: Relationship, from: EntityId) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.clear_from(from);
    }

    pub fn clear_relationships_to(&mut self, relationship: Relationship, to: EntityId) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.clear_to(to);
    }
}

impl std::ops::Index<EntityId> for Entities {
    type Output = Entity;

    fn index(&self, index: EntityId) -> &Self::Output {
        match self.entries.get(index) {
            Some(x) => x,
            None => {
                // It is ok to index with the Null id, which results in the null-dummy.
                // This is according to ZII
                if index.is_null() {
                    &self.null_dummy
                } else {
                    panic!("Invalid entity id used")
                }
            }
        }
    }
}

impl std::ops::IndexMut<EntityId> for Entities {
    fn index_mut(&mut self, index: EntityId) -> &mut Self::Output {
        match self.entries.get_mut(index) {
            Some(x) => x,
            None => {
                // It is ok to index with the Null id, which results in the null-dummy.
                // This is according to ZII
                if index.is_null() {
                    &mut self.null_dummy
                } else {
                    panic!("Invalid entity id used")
                }
            }
        }
    }
}

impl std::ops::Index<ArchetypeId> for Entities {
    type Output = Archetype;

    fn index(&self, index: ArchetypeId) -> &Self::Output {
        &self.types[index]
    }
}

impl std::ops::IndexMut<ArchetypeId> for Entities {
    fn index_mut(&mut self, index: ArchetypeId) -> &mut Self::Output {
        &mut self.types[index]
    }
}

const SET_BITSET_SIZE: usize = (Set::COUNT + 63) / 64;
const FLAG_BITSET_SIZE: usize = (Flag::COUNT + 63) / 64;

#[derive(Default, Clone, Copy)]
pub(crate) struct Entity {
    pub id: EntityId,
    pub name: Name,
    pub is_player: bool,
    pub behavior: Behavior,
    pub fixed_behavior: Option<Behavior>,
    pub location: Location,
    pub task: Task,
    vars: [f64; Var::COUNT],
    sets: BitSet<SET_BITSET_SIZE>,
    pub flags: Flags,
    // Party-like
    pub type_id: ArchetypeId,
    pub body: Body,
    pub speed: f32,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Body {
    pub pos: V2,
    pub size: f32,
}

#[derive(Default, Clone, Copy)]
pub struct Flags(BitSet<FLAG_BITSET_SIZE>);

impl Flags {
    pub(crate) const fn new() -> Self {
        Self(BitSet::new())
    }

    #[inline]
    pub(crate) const fn get(&self, flag: Flag) -> bool {
        self.0.get(flag as usize)
    }

    #[inline]
    pub(crate) const fn set(&mut self, flag: Flag, value: bool) {
        self.0.set(flag as usize, value);
    }

    #[inline]
    pub(crate) const fn with(mut self, flag: Flag) -> Self {
        self.set(flag, true);
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum Location {
    Nowhere,
    FarOut,
    Near(EntityId),
    Proximate(EntityId),
    Inside(EntityId),
}

impl Location {
    pub fn is_at(self, entity: EntityId) -> bool {
        match self {
            Self::Proximate(id) => id == entity,
            Self::Inside(id) => id == entity,
            _ => false,
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Self::Nowhere
    }
}

#[derive(Default, Clone, Copy)]
pub(crate) struct Task {
    /// The kind of the taks. Useful to know what we are even doing
    pub kind: TaskKind,
    /// Primary destination of this task
    pub destination: TaskDestination,
    /// Interaction to be performed at destination
    pub interaction: TaskInteraction,
    /// Ticks to wait after arrival before interactions can resolve.
    pub arrival_wait: u32,
    /// Has the task actually been accomplished?
    pub is_complete: bool,
    /// Some tasks carry a designated position
    pub designated_pos: V2,
    /// Despawn if this task is chosen
    pub despawn_on_choice: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum TaskKind {
    Init,
    Generic,
    Deliver,
    Load,
    ReturnToBase,
    Hunt,
}

impl Default for TaskKind {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum TaskDestination {
    Nothing,
    DesignatedPos,
    Home,
    Base,
    WorkArea,
    MarketOfHome,
    TradingDestination,
}

impl Default for TaskDestination {
    fn default() -> Self {
        Self::Nothing
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum InteractionKind {
    SellFood,
    LoadFood,
    LoadMinerals,
    UnloadMinerals,
    UnloadGoods,
    CaravanVisit,
}

const INTERACTION_BITSET_SIZE: usize = (InteractionKind::COUNT + 63) / 64;

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TaskInteraction {
    flags: BitSet<INTERACTION_BITSET_SIZE>,
}

impl TaskInteraction {
    pub(crate) fn new(to_set: &[InteractionKind]) -> Self {
        let mut this = Self::default();
        for &x in to_set {
            this.set(x, true);
        }
        this
    }

    pub(crate) fn with(interaction: InteractionKind) -> Self {
        let mut this = Self::default();
        this.set(interaction, true);
        this
    }

    pub(crate) fn get(&self, interaction: InteractionKind) -> bool {
        self.flags.get(interaction as usize)
    }

    pub(crate) fn set(&mut self, interaction: InteractionKind, value: bool) {
        self.flags.set(interaction as usize, value);
    }

    pub(crate) fn any(&self) -> bool {
        self.flags.iter().next().is_some()
    }

    pub(crate) fn iter_active(self) -> impl Iterator<Item = InteractionKind> {
        InteractionKind::iter().filter(move |&x| self.get(x))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Behavior {
    Idle,
    ToPos(V2),
    ToEntity {
        target: EntityId,
        on_arrival: OnArrival,
    },
    Player,
}

impl Default for Behavior {
    fn default() -> Self {
        Self::Idle
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum Goal {
    Idle,
    MoveTo(V2),
    ToEntity {
        target: EntityId,
        distance: f32,
        on_arrival: OnArrival,
    },
}

impl Default for Goal {
    fn default() -> Self {
        Self::Idle
    }
}

impl Entity {
    pub(crate) fn in_set(&self, flag: Set) -> bool {
        self.sets.get(flag as usize)
    }

    pub(crate) fn vars_mut(&mut self) -> VarsMut<'_> {
        VarsMut(&mut self.vars)
    }

    #[inline]
    pub(crate) fn get_var(&self, var: Var) -> f64 {
        self.vars[var as usize]
    }

    #[inline]
    pub(crate) fn set_var(&mut self, var: Var, value: f64) {
        self.vars[var as usize] = value;
    }

    #[inline]
    pub(crate) fn inc_var(&mut self, var: Var, value: f64) {
        self.vars[var as usize] += value;
    }

    #[inline]
    pub(crate) fn modify_var(&mut self, var: Var, f: impl FnOnce(f64) -> f64) {
        self.vars[var as usize] = f(self.vars[var as usize]);
    }

    #[inline]
    pub(crate) fn get_flag(&self, flag: Flag) -> bool {
        self.flags.get(flag)
    }

    #[inline]
    pub(crate) fn set_flag(&mut self, flag: Flag, value: bool) {
        self.flags.set(flag, value);
    }
}

#[derive(Default)]
pub(crate) struct VarsMut<'a>(&'a mut [f64]);

impl<'a> VarsMut<'a> {
    #[inline]
    pub(crate) fn get(&self, var: Var) -> f64 {
        self.try_get(var).unwrap_or(0.)
    }

    #[inline]
    pub(crate) fn try_get(&self, var: Var) -> Option<f64> {
        let idx = var as usize;
        self.0.get(idx).copied()
    }

    #[inline]
    pub(crate) fn set(&mut self, var: Var, value: f64) {
        let idx = var as usize;
        if let Some(var) = self.0.get_mut(idx) {
            *var = value;
        }
    }

    #[inline]
    pub(crate) fn set_many(&mut self, vars: &[(Var, f64)]) {
        for &(var, value) in vars {
            self.set(var, value);
        }
    }

    #[inline]
    pub(crate) fn modify(&mut self, var: Var, f: impl FnOnce(f64) -> f64) {
        let x = self.get(var);
        self.set(var, f(x));
    }

    #[inline]
    pub(crate) fn with(mut self, var: Var, value: f64) -> Self {
        self.set(var, value);
        self
    }
}

#[derive(Default)]
struct HierarchyData {
    child_to_parent: SecondaryMap<EntityId, EntityId>,
    // We store parent to child "semi-contiguously" as pairs.
    // We store the child as a raw u64 before we need to have a min-max range.
    // The u64 is just the raw encoding of the Slotmap key.
    parent_to_child: BTreeSet<(EntityId, u64)>,
}

impl HierarchyData {
    fn change(&mut self, parent: EntityId, child: EntityId) {
        let previous_parent = if parent.is_null() {
            self.child_to_parent.remove(child)
        } else {
            self.parent_to_child.insert((parent, child.data().as_ffi()));
            self.child_to_parent.insert(child, parent)
        }
        .unwrap_or_default();

        // Remove previous parent -> child if it changed
        if !previous_parent.is_null() && previous_parent != parent {
            self.parent_to_child
                .remove(&(previous_parent, child.data().as_ffi()));
        }
    }

    fn parent_of(&self, entity: EntityId) -> EntityId {
        self.child_to_parent
            .get(entity)
            .copied()
            .unwrap_or_default()
    }

    fn children_of(&self, parent: EntityId) -> impl Iterator<Item = EntityId> + use<'_> {
        self.parent_to_child
            .range(Self::range_of(parent))
            .map(|&(_, child)| EntityId::from(KeyData::from_ffi(child)))
    }

    fn range_of(parent: EntityId) -> std::ops::RangeInclusive<(EntityId, u64)> {
        (parent, u64::MIN)..=(parent, u64::MAX)
    }

    fn remove_entity(&mut self, entity: EntityId) {
        // Remove slice of children
        let children_list = self
            .parent_to_child
            .extract_if(Self::range_of(entity), |_| true)
            .map(|(_, child)| EntityId::from(KeyData::from_ffi(child)));

        // For each childi n the slice, remove from the parent
        for child in children_list {
            let old_parent = self.child_to_parent.remove(child);
            assert!(old_parent == Some(entity));
        }

        // Remove entity's own parent
        if let Some(parent) = self.child_to_parent.remove(entity) {
            let idx = entity.0.as_ffi();
            self.parent_to_child.remove(&(parent, idx));
        }
    }
}

#[derive(Default)]
struct RelationshipData {
    out_edges: SecondaryMap<EntityId, BTreeMap<EntityId, f32>>,
    in_edges: SecondaryMap<EntityId, BTreeSet<EntityId>>,
}

impl RelationshipData {
    fn set(&mut self, from: EntityId, to: EntityId, value: f32) {
        if let Some(map) = self.out_edges.get_mut(from) {
            map.insert(to, value);
        } else {
            let mut map = BTreeMap::new();
            map.insert(to, value);
            self.out_edges.insert(from, map);
        }

        if let Some(set) = self.in_edges.get_mut(to) {
            set.insert(from);
        } else {
            let mut set = BTreeSet::new();
            set.insert(from);
            self.in_edges.insert(to, set);
        }
    }

    fn remove(&mut self, from: EntityId, to: EntityId) {
        if let Some(map) = self.out_edges.get_mut(from) {
            map.remove(&to);
            if map.is_empty() {
                self.out_edges.remove(from);
            }
        }

        if let Some(set) = self.in_edges.get_mut(to) {
            set.remove(&from);
            if set.is_empty() {
                self.in_edges.remove(to);
            }
        }
    }

    fn get(&self, from: EntityId, to: EntityId) -> Option<f32> {
        self.out_edges
            .get(from)
            .and_then(|map| map.get(&to).copied())
    }

    fn from(&self, from: EntityId) -> impl Iterator<Item = (EntityId, f32)> + use<'_> {
        self.out_edges
            .get(from)
            .map(|map| map.iter().map(|(id, value)| (*id, *value)))
            .into_iter()
            .flatten()
    }

    fn clear_from(&mut self, from: EntityId) {
        let edges = match self.out_edges.remove(from) {
            Some(edges) => edges,
            None => {
                return;
            }
        };

        for to in edges.keys().copied() {
            if let Some(set) = self.in_edges.get_mut(to) {
                set.remove(&from);
                if set.is_empty() {
                    self.in_edges.remove(to);
                }
            }
        }
    }

    fn clear_to(&mut self, to: EntityId) {
        let froms = match self.in_edges.remove(to) {
            Some(froms) => froms,
            None => {
                return;
            }
        };

        for from in froms {
            if let Some(map) = self.out_edges.get_mut(from) {
                map.remove(&to);
                if map.is_empty() {
                    self.out_edges.remove(from);
                }
            }
        }
    }

    fn remove_entity(&mut self, entity: EntityId) {
        self.clear_from(entity);
        self.clear_to(entity);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn insert_entity(entities: &mut Entities) -> EntityId {
        let id = entities.entries.insert(Entity::default());
        if let Some(entity) = entities.entries.get_mut(id) {
            entity.id = id;
        }
        id
    }

    #[test]
    fn hierarchy_parent_despawn_clears_child_links() {
        let mut entities = Entities::default();
        let parent = insert_entity(&mut entities);
        let child = insert_entity(&mut entities);

        entities.set_parent(Hierarchy::FactionMembership, parent, child);
        entities.despawn(parent);

        let children: Vec<_> = entities
            .children_of(Hierarchy::FactionMembership, parent)
            .collect();
        assert!(children.is_empty());

        let h_data = &entities.hierarchies[Hierarchy::FactionMembership as usize];
        assert!(h_data.parent_of(child).is_null());
    }

    #[test]
    fn hierarchy_child_despawn_clears_parent_links() {
        let mut entities = Entities::default();
        let parent = insert_entity(&mut entities);
        let child = insert_entity(&mut entities);

        entities.set_parent(Hierarchy::FactionMembership, parent, child);
        entities.despawn(child);

        let children: Vec<_> = entities
            .children_of(Hierarchy::FactionMembership, parent)
            .collect();
        assert!(children.is_empty());
    }

    #[test]
    fn hierarchy_reparenting_removes_previous_link() {
        let mut entities = Entities::default();
        let parent_a = insert_entity(&mut entities);
        let parent_b = insert_entity(&mut entities);
        let child = insert_entity(&mut entities);

        entities.set_parent(Hierarchy::FactionMembership, parent_a, child);
        entities.set_parent(Hierarchy::FactionMembership, parent_b, child);

        let children_a: Vec<_> = entities
            .children_of(Hierarchy::FactionMembership, parent_a)
            .collect();
        let children_b: Vec<_> = entities
            .children_of(Hierarchy::FactionMembership, parent_b)
            .collect();
        assert!(children_a.is_empty());
        assert_eq!(children_b, vec![child]);

        let h_data = &entities.hierarchies[Hierarchy::FactionMembership as usize];
        assert_eq!(h_data.parent_of(child), parent_b);
    }

    #[test]
    fn relationships_set_get_and_has() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);
        let c = insert_entity(&mut entities);

        entities.set_relationship(Relationship::Placeholder1, a, b, 1.5);
        assert_eq!(
            entities.relationship(Relationship::Placeholder1, a, b),
            Some(1.5)
        );
        assert!(entities.has_relationship(Relationship::Placeholder1, a, b));
        assert!(
            entities
                .relationship(Relationship::Placeholder1, a, c)
                .is_none()
        );
    }

    #[test]
    fn relationships_update_value() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);

        entities.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        entities.set_relationship(Relationship::Placeholder1, a, b, 2.0);
        assert_eq!(
            entities.relationship(Relationship::Placeholder1, a, b),
            Some(2.0)
        );
    }

    #[test]
    fn relationships_clear_from_only_removes_outgoing() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);
        let c = insert_entity(&mut entities);
        let d = insert_entity(&mut entities);

        entities.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        entities.set_relationship(Relationship::Placeholder1, a, c, 2.0);
        entities.set_relationship(Relationship::Placeholder1, d, a, 3.0);

        entities.clear_relationships_from(Relationship::Placeholder1, a);

        assert!(
            entities
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert!(
            entities
                .relationship(Relationship::Placeholder1, a, c)
                .is_none()
        );
        assert_eq!(
            entities.relationship(Relationship::Placeholder1, d, a),
            Some(3.0)
        );
    }

    #[test]
    fn relationships_clear_to_only_removes_incoming() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);
        let c = insert_entity(&mut entities);
        let d = insert_entity(&mut entities);

        entities.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        entities.set_relationship(Relationship::Placeholder1, c, b, 2.0);
        entities.set_relationship(Relationship::Placeholder1, b, d, 3.0);

        entities.clear_relationships_to(Relationship::Placeholder1, b);

        assert!(
            entities
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert!(
            entities
                .relationship(Relationship::Placeholder1, c, b)
                .is_none()
        );
        assert_eq!(
            entities.relationship(Relationship::Placeholder1, b, d),
            Some(3.0)
        );
    }

    #[test]
    fn relationships_despawn_clears_incoming_and_outgoing() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);
        let c = insert_entity(&mut entities);
        let d = insert_entity(&mut entities);
        let e = insert_entity(&mut entities);

        entities.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        entities.set_relationship(Relationship::Placeholder1, b, c, 2.0);
        entities.set_relationship(Relationship::Placeholder1, c, b, 3.0);
        entities.set_relationship(Relationship::Placeholder1, d, e, 4.0);

        entities.despawn(b);

        assert!(
            entities
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert!(
            entities
                .relationship(Relationship::Placeholder1, b, c)
                .is_none()
        );
        assert!(
            entities
                .relationship(Relationship::Placeholder1, c, b)
                .is_none()
        );
        assert_eq!(
            entities.relationship(Relationship::Placeholder1, d, e),
            Some(4.0)
        );
    }

    #[test]
    fn relationships_are_isolated_by_type() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);

        entities.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        entities.set_relationship(Relationship::Placeholder2, a, b, 2.0);
        entities.remove_relationship(Relationship::Placeholder1, a, b);

        assert!(
            entities
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert_eq!(
            entities.relationship(Relationship::Placeholder2, a, b),
            Some(2.0)
        );
    }

    #[test]
    fn relationships_from_are_sorted_by_entity_id() {
        let mut entities = Entities::default();
        let from = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);
        let c = insert_entity(&mut entities);

        assert!(b < c);

        entities.set_relationship(Relationship::Placeholder1, from, c, 2.0);
        entities.set_relationship(Relationship::Placeholder1, from, b, 1.0);

        let edges: Vec<_> = entities
            .relationships_from(Relationship::Placeholder1, from)
            .collect();
        assert_eq!(edges, vec![(b, 1.0), (c, 2.0)]);
    }

    #[test]
    fn sets_add_remove_and_iterate() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);

        assert!(entities.add_to_set(Set::People, a));
        assert!(entities.add_to_set(Set::People, b));
        assert!(!entities.add_to_set(Set::People, a));

        let mut members: Vec<_> = entities.iter_set_ids(Set::People).collect();
        members.sort();
        assert_eq!(members, vec![a, b]);

        assert!(entities.remove_from_set(Set::People, a));
        assert!(!entities.remove_from_set(Set::People, a));
        let members: Vec<_> = entities.iter_set_ids(Set::People).collect();
        assert_eq!(members, vec![b]);
    }

    #[test]
    fn sets_despawn_removes_membership() {
        let mut entities = Entities::default();
        let a = insert_entity(&mut entities);
        let b = insert_entity(&mut entities);

        entities.add_to_set(Set::Settlements, a);
        entities.add_to_set(Set::Settlements, b);

        entities.despawn(a);
        let members: Vec<_> = entities.iter_set_ids(Set::Settlements).collect();
        assert_eq!(members, vec![b]);
    }

    #[test]
    fn vars_default_to_zero() {
        let mut entities = Entities::default();
        entities.spawn();
        let entity = entities.iter().next().unwrap();

        let entity = &entities[entity.id];
        assert_eq!(entity.get_var(Var::Renown), 0.0);
        assert_eq!(entity.get_var(Var::Population), 0.0);
        assert_eq!(entity.get_var(Var::Prosperity), 0.0);
    }

    #[test]
    fn vars_mut_set_persists() {
        let mut entities = Entities::default();
        entities.spawn();
        let id = entities.ids().next().unwrap();

        {
            let mut vars = entities.vars_mut(id);
            vars.set(Var::Renown, 3.5);
            vars.set(Var::Population, 10.0);
        }

        let entity = &entities[id];
        assert_eq!(entity.get_var(Var::Renown), 3.5);
        assert_eq!(entity.get_var(Var::Population), 10.0);
    }

    #[test]
    fn vars_entity_methods_are_consistent() {
        let mut entities = Entities::default();
        let id = entities.spawn().id;

        {
            let entity = entities.get_mut(id).unwrap();
            let mut vars = entity.vars_mut();
            vars.set(Var::Renown, 4.0);
        }

        let entity = entities.get(id).unwrap();
        assert_eq!(entity.get_var(Var::Renown), 4.);
    }

    #[test]
    fn entity_flags_default_false() {
        let entity = Entity::default();
        assert!(!entity.flags.get(Flag::IsFarmer));
    }

    #[test]
    fn entity_flags_set_and_clear() {
        let mut entity = Entity::default();
        entity.flags.set(Flag::IsFarmer, true);
        assert!(entity.flags.get(Flag::IsFarmer));
        entity.flags.set(Flag::IsFarmer, false);
        assert!(!entity.flags.get(Flag::IsFarmer));
    }
}
