use std::collections::{BTreeMap, BTreeSet};

use crate::{names::Name, parties::PartyId};
use slotmap::*;
use strum::*;
use util::bitset::BitSet;

new_key_type! { pub struct AgentId; }

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Var {
    // Person
    Renown,
    // Settlement
    Population,
    Prosperity,
    FoodStored,
    FoodCapacity,
    // Economic action
    ProsperityBonus,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Set {
    Locations,
    People,
    Settlements,
    Villages,
    Hillforts,
    Towns,
    Mines,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Flag {
    IsFarmer,
    IsMiner,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Hierarchy {
    Attachment,
    FactionMembership,
    LocalMarket,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Relationship {
    Placeholder1,
    Placeholder2,
}

#[derive(Default)]
pub(crate) struct Agents {
    entries: SlotMap<AgentId, Agent>,
    hierarchies: [HierarchyData; Hierarchy::COUNT],
    relationships: [RelationshipData; Relationship::COUNT],
    sets: [BTreeSet<AgentId>; Set::COUNT],
}

impl Agents {
    pub(crate) fn spawn(&mut self) -> &mut Agent {
        let id = self.entries.insert(Agent::default());
        let agent = Agent {
            id,
            ..Default::default()
        };
        let entry = &mut self.entries[id];
        *entry = agent;
        entry
    }

    pub(crate) fn despawn(&mut self, id: AgentId) {
        let entity = match self.entries.remove(id) {
            Some(entity) => entity,
            None => {
                return;
            }
        };

        for set_idx in entity.sets.iter() {
            self.sets[set_idx].remove(&entity.id);
        }

        for hierarchy in &mut self.hierarchies {
            hierarchy.remove_agent(id);
        }

        for relationship in &mut self.relationships {
            relationship.remove_agent(id);
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Agent> + ExactSizeIterator {
        self.entries.values()
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut Agent> + ExactSizeIterator {
        self.entries.values_mut()
    }

    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    pub(crate) fn ids(&self) -> impl Iterator<Item = AgentId> + ExactSizeIterator {
        self.entries.keys()
    }

    pub(crate) fn get(&self, id: AgentId) -> Option<&Agent> {
        self.entries.get(id)
    }

    pub(crate) fn get_mut(&mut self, id: AgentId) -> Option<&mut Agent> {
        self.entries.get_mut(id)
    }

    pub fn vars(&self, id: AgentId) -> Vars<'_> {
        self.entries.get(id).map(Agent::vars).unwrap_or_default()
    }

    pub fn vars_mut(&mut self, id: AgentId) -> VarsMut<'_> {
        self.entries
            .get_mut(id)
            .map(Agent::vars_mut)
            .unwrap_or_default()
    }

    pub fn add_to_set(&mut self, set: Set, agent: AgentId) -> bool {
        let idx = set as usize;
        self.entries[agent].sets.set(idx, true);
        self.sets[idx].insert(agent)
    }

    pub fn remove_from_set(&mut self, set: Set, agent: AgentId) -> bool {
        let idx = set as usize;
        self.entries[agent].sets.set(idx, false);
        self.sets[idx].remove(&agent)
    }

    pub fn iter_set_ids(&self, set: Set) -> impl Iterator<Item = AgentId> + use<'_> {
        self.sets[set as usize].iter().copied()
    }

    pub fn iter_set(&self, set: Set) -> impl Iterator<Item = &Agent> + use<'_> {
        self.iter_set_ids(set).map(|id| &self.entries[id])
    }

    pub fn set_parent(&mut self, hierarchy: Hierarchy, parent: AgentId, child: AgentId) {
        let h_data = &mut self.hierarchies[hierarchy as usize];
        h_data.change(parent, child);
    }

    pub fn remove_parent(&mut self, hierarchy: Hierarchy, child: AgentId) {
        let h_data = &mut self.hierarchies[hierarchy as usize];
        h_data.change(AgentId::null(), child);
    }

    pub fn children_of(
        &self,
        hierarchy: Hierarchy,
        parent: AgentId,
    ) -> impl Iterator<Item = AgentId> + use<'_> {
        let h_data = &self.hierarchies[hierarchy as usize];
        h_data.children_of(parent)
    }

    pub fn parent_of(&self, hierarchy: Hierarchy, child: AgentId) -> AgentId {
        let h_data = &self.hierarchies[hierarchy as usize];
        h_data.parent_of(child)
    }

    pub fn set_relationship(
        &mut self,
        relationship: Relationship,
        from: AgentId,
        to: AgentId,
        value: f32,
    ) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.set(from, to, value);
    }

    pub fn remove_relationship(&mut self, relationship: Relationship, from: AgentId, to: AgentId) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.remove(from, to);
    }

    pub fn relationship(
        &self,
        relationship: Relationship,
        from: AgentId,
        to: AgentId,
    ) -> Option<f32> {
        let rel_data = &self.relationships[relationship as usize];
        rel_data.get(from, to)
    }

    pub fn has_relationship(&self, relationship: Relationship, from: AgentId, to: AgentId) -> bool {
        self.relationship(relationship, from, to).is_some()
    }

    pub fn relationships_from(
        &self,
        relationship: Relationship,
        from: AgentId,
    ) -> impl Iterator<Item = (AgentId, f32)> + use<'_> {
        let rel_data = &self.relationships[relationship as usize];
        rel_data.from(from)
    }

    pub fn clear_relationships_from(&mut self, relationship: Relationship, from: AgentId) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.clear_from(from);
    }

    pub fn clear_relationships_to(&mut self, relationship: Relationship, to: AgentId) {
        let rel_data = &mut self.relationships[relationship as usize];
        rel_data.clear_to(to);
    }
}

impl std::ops::Index<AgentId> for Agents {
    type Output = Agent;

    fn index(&self, index: AgentId) -> &Self::Output {
        &self.entries[index]
    }
}

impl std::ops::IndexMut<AgentId> for Agents {
    fn index_mut(&mut self, index: AgentId) -> &mut Self::Output {
        &mut self.entries[index]
    }
}

const SET_BITSET_SIZE: usize = (Set::COUNT + 63) / 64;
const FLAG_BITSET_SIZE: usize = (Flag::COUNT + 63) / 64;

#[derive(Default, Clone, Copy)]
pub(crate) struct Agent {
    pub id: AgentId,
    pub name: Name,
    pub party: PartyId,
    pub is_player: bool,
    pub behavior: Behavior,
    pub fixed_behavior: Option<Behavior>,
    pub location: Location,
    pub task: Task,
    vars: [f64; Var::COUNT],
    sets: BitSet<SET_BITSET_SIZE>,
    pub flags: Flags,
}

#[derive(Default, Clone, Copy)]
pub struct Flags(BitSet<FLAG_BITSET_SIZE>);

impl Flags {
    #[inline]
    pub(crate) fn get(&self, flag: Flag) -> bool {
        self.0.get(flag as usize)
    }

    #[inline]
    pub(crate) fn set(&mut self, flag: Flag, value: bool) {
        self.0.set(flag as usize, value);
    }

    #[inline]
    pub(crate) fn with(mut self, flag: Flag) -> Self {
        self.set(flag, true);
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum Location {
    Nowhere,
    FarOut,
    Near(AgentId),
    Proximate(AgentId),
    Inside(AgentId),
}

impl Location {
    pub fn is_at(self, agent: AgentId) -> bool {
        match self {
            Self::Proximate(id) => id == agent,
            Self::Inside(id) => id == agent,
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
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum TaskKind {
    Init,
    Deliver,
    Load,
    ReturnHome,
}

impl Default for TaskKind {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum TaskDestination {
    Nothing,
    Home,
    MarketOfHome,
}

impl Default for TaskDestination {
    fn default() -> Self {
        Self::Nothing
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Interaction {
    UnloadFood,
    LoadFood,
    LoadProsperityBonus,
    IncreaseProsperity,
    ResetProsperityBonus,
}

const INTERACTION_BITSET_SIZE: usize = (Interaction::COUNT + 63) / 64;

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TaskInteraction {
    flags: BitSet<INTERACTION_BITSET_SIZE>,
}

impl TaskInteraction {
    pub(crate) fn new(to_set: &[Interaction]) -> Self {
        let mut this = Self::default();
        for &x in to_set {
            this.set(x, true);
        }
        this
    }

    pub(crate) fn with(interaction: Interaction) -> Self {
        let mut this = Self::default();
        this.set(interaction, true);
        this
    }

    pub(crate) fn get(&self, interaction: Interaction) -> bool {
        self.flags.get(interaction as usize)
    }

    pub(crate) fn set(&mut self, interaction: Interaction, value: bool) {
        self.flags.set(interaction as usize, value);
    }

    pub(crate) fn any(&self) -> bool {
        self.flags.iter().next().is_some()
    }

    pub(crate) fn iter_active(self) -> impl Iterator<Item = Interaction> {
        Interaction::iter().filter(move |&x| self.get(x))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Behavior {
    Idle,
    GoTo {
        target: AgentId,
        enter_on_arrival: bool,
    },
    Player,
    Test,
}

impl Default for Behavior {
    fn default() -> Self {
        Self::Idle
    }
}

impl Agent {
    pub(crate) fn in_set(&self, flag: Set) -> bool {
        self.sets.get(flag as usize)
    }

    pub(crate) fn vars(&self) -> Vars<'_> {
        Vars(&self.vars)
    }

    pub(crate) fn vars_mut(&mut self) -> VarsMut<'_> {
        VarsMut(&mut self.vars)
    }

    pub(crate) fn get_var(&self, var: Var) -> f64 {
        self.vars[var as usize]
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Vars<'a>(&'a [f64]);

impl Vars<'_> {
    pub(crate) fn get(&self, var: Var) -> f64 {
        self.try_get(var).unwrap_or(0.)
    }

    pub(crate) fn try_get(&self, var: Var) -> Option<f64> {
        let idx = var as usize;
        self.0.get(idx).copied()
    }

    pub(crate) fn copy_out<'a>(&self, alloc: impl VarAllocator<'a>) -> Vars<'a> {
        alloc.host_vars(self).into_shared()
    }

    pub(crate) fn copy_out_mut<'a>(&self, alloc: impl VarAllocator<'a>) -> VarsMut<'a> {
        alloc.host_vars(self)
    }
}

pub(crate) trait VarAllocator<'a> {
    fn host_vars(self, vars: &Vars) -> VarsMut<'a>;
}

impl<'a> VarAllocator<'a> for &'a bumpalo::Bump {
    fn host_vars(self, vars: &Vars) -> VarsMut<'a> {
        let mut vec = bumpalo::collections::Vec::with_capacity_in(vars.0.len(), self);
        vec.extend(vars.0.iter().copied());
        VarsMut(vec.into_bump_slice_mut())
    }
}

impl<'a> VarAllocator<'a> for &'a mut Vec<f64> {
    fn host_vars(self, vars: &Vars) -> VarsMut<'a> {
        self.reserve(vars.0.len().saturating_sub(self.len()));
        self.extend(vars.0.iter().copied());
        VarsMut(self.as_mut_slice())
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
    pub(crate) fn into_shared(self) -> Vars<'a> {
        Vars(&*self.0)
    }

    #[inline]
    pub(crate) fn with(mut self, var: Var, value: f64) -> Self {
        self.set(var, value);
        self
    }
}

#[derive(Default)]
struct HierarchyData {
    child_to_parent: SecondaryMap<AgentId, AgentId>,
    // We store parent to child "semi-contiguously" as pairs.
    // We store the child as a raw u64 before we need to have a min-max range.
    // The u64 is just the raw encoding of the Slotmap key.
    parent_to_child: BTreeSet<(AgentId, u64)>,
}

impl HierarchyData {
    fn change(&mut self, parent: AgentId, child: AgentId) {
        let previous_parent = if parent.is_null() {
            self.child_to_parent.remove(child)
        } else {
            self.parent_to_child.insert((parent, child.data().as_ffi()));
            self.child_to_parent.insert(child, parent)
        }
        .unwrap_or_default();

        // Remove previous parent -> child
        if !previous_parent.is_null() {
            self.parent_to_child
                .remove(&(previous_parent, child.data().as_ffi()));
        }
    }

    fn parent_of(&self, agent: AgentId) -> AgentId {
        self.child_to_parent.get(agent).copied().unwrap_or_default()
    }

    fn children_of(&self, parent: AgentId) -> impl Iterator<Item = AgentId> + use<'_> {
        self.parent_to_child
            .range(Self::range_of(parent))
            .map(|&(_, child)| AgentId::from(KeyData::from_ffi(child)))
    }

    fn range_of(parent: AgentId) -> std::ops::RangeInclusive<(AgentId, u64)> {
        (parent, u64::MIN)..=(parent, u64::MAX)
    }

    fn remove_agent(&mut self, agent: AgentId) {
        // Remove slice of children
        let children_list = self
            .parent_to_child
            .extract_if(Self::range_of(agent), |_| true)
            .map(|(_, child)| AgentId::from(KeyData::from_ffi(child)));

        // For each childi n the slice, remove from the parent
        for child in children_list {
            let old_parent = self.child_to_parent.remove(child);
            assert!(old_parent == Some(agent));
        }

        // Remove agent's own parent
        if let Some(parent) = self.child_to_parent.remove(agent) {
            let idx = agent.0.as_ffi();
            self.parent_to_child.remove(&(parent, idx));
        }
    }
}

#[derive(Default)]
struct RelationshipData {
    out_edges: SecondaryMap<AgentId, BTreeMap<AgentId, f32>>,
    in_edges: SecondaryMap<AgentId, BTreeSet<AgentId>>,
}

impl RelationshipData {
    fn set(&mut self, from: AgentId, to: AgentId, value: f32) {
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

    fn remove(&mut self, from: AgentId, to: AgentId) {
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

    fn get(&self, from: AgentId, to: AgentId) -> Option<f32> {
        self.out_edges
            .get(from)
            .and_then(|map| map.get(&to).copied())
    }

    fn from(&self, from: AgentId) -> impl Iterator<Item = (AgentId, f32)> + use<'_> {
        self.out_edges
            .get(from)
            .map(|map| map.iter().map(|(id, value)| (*id, *value)))
            .into_iter()
            .flatten()
    }

    fn clear_from(&mut self, from: AgentId) {
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

    fn clear_to(&mut self, to: AgentId) {
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

    fn remove_agent(&mut self, agent: AgentId) {
        self.clear_from(agent);
        self.clear_to(agent);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn insert_agent(agents: &mut Agents) -> AgentId {
        let id = agents.entries.insert(Agent::default());
        if let Some(agent) = agents.entries.get_mut(id) {
            agent.id = id;
        }
        id
    }

    #[test]
    fn hierarchy_parent_despawn_clears_child_links() {
        let mut agents = Agents::default();
        let parent = insert_agent(&mut agents);
        let child = insert_agent(&mut agents);

        agents.set_parent(Hierarchy::FactionMembership, parent, child);
        agents.despawn(parent);

        let children: Vec<_> = agents
            .children_of(Hierarchy::FactionMembership, parent)
            .collect();
        assert!(children.is_empty());

        let h_data = &agents.hierarchies[Hierarchy::FactionMembership as usize];
        assert!(h_data.parent_of(child).is_null());
    }

    #[test]
    fn hierarchy_child_despawn_clears_parent_links() {
        let mut agents = Agents::default();
        let parent = insert_agent(&mut agents);
        let child = insert_agent(&mut agents);

        agents.set_parent(Hierarchy::FactionMembership, parent, child);
        agents.despawn(child);

        let children: Vec<_> = agents
            .children_of(Hierarchy::FactionMembership, parent)
            .collect();
        assert!(children.is_empty());
    }

    #[test]
    fn hierarchy_reparenting_removes_previous_link() {
        let mut agents = Agents::default();
        let parent_a = insert_agent(&mut agents);
        let parent_b = insert_agent(&mut agents);
        let child = insert_agent(&mut agents);

        agents.set_parent(Hierarchy::FactionMembership, parent_a, child);
        agents.set_parent(Hierarchy::FactionMembership, parent_b, child);

        let children_a: Vec<_> = agents
            .children_of(Hierarchy::FactionMembership, parent_a)
            .collect();
        let children_b: Vec<_> = agents
            .children_of(Hierarchy::FactionMembership, parent_b)
            .collect();
        assert!(children_a.is_empty());
        assert_eq!(children_b, vec![child]);

        let h_data = &agents.hierarchies[Hierarchy::FactionMembership as usize];
        assert_eq!(h_data.parent_of(child), parent_b);
    }

    #[test]
    fn relationships_set_get_and_has() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);
        let c = insert_agent(&mut agents);

        agents.set_relationship(Relationship::Placeholder1, a, b, 1.5);
        assert_eq!(
            agents.relationship(Relationship::Placeholder1, a, b),
            Some(1.5)
        );
        assert!(agents.has_relationship(Relationship::Placeholder1, a, b));
        assert!(
            agents
                .relationship(Relationship::Placeholder1, a, c)
                .is_none()
        );
    }

    #[test]
    fn relationships_update_value() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);

        agents.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        agents.set_relationship(Relationship::Placeholder1, a, b, 2.0);
        assert_eq!(
            agents.relationship(Relationship::Placeholder1, a, b),
            Some(2.0)
        );
    }

    #[test]
    fn relationships_clear_from_only_removes_outgoing() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);
        let c = insert_agent(&mut agents);
        let d = insert_agent(&mut agents);

        agents.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        agents.set_relationship(Relationship::Placeholder1, a, c, 2.0);
        agents.set_relationship(Relationship::Placeholder1, d, a, 3.0);

        agents.clear_relationships_from(Relationship::Placeholder1, a);

        assert!(
            agents
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert!(
            agents
                .relationship(Relationship::Placeholder1, a, c)
                .is_none()
        );
        assert_eq!(
            agents.relationship(Relationship::Placeholder1, d, a),
            Some(3.0)
        );
    }

    #[test]
    fn relationships_clear_to_only_removes_incoming() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);
        let c = insert_agent(&mut agents);
        let d = insert_agent(&mut agents);

        agents.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        agents.set_relationship(Relationship::Placeholder1, c, b, 2.0);
        agents.set_relationship(Relationship::Placeholder1, b, d, 3.0);

        agents.clear_relationships_to(Relationship::Placeholder1, b);

        assert!(
            agents
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert!(
            agents
                .relationship(Relationship::Placeholder1, c, b)
                .is_none()
        );
        assert_eq!(
            agents.relationship(Relationship::Placeholder1, b, d),
            Some(3.0)
        );
    }

    #[test]
    fn relationships_despawn_clears_incoming_and_outgoing() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);
        let c = insert_agent(&mut agents);
        let d = insert_agent(&mut agents);
        let e = insert_agent(&mut agents);

        agents.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        agents.set_relationship(Relationship::Placeholder1, b, c, 2.0);
        agents.set_relationship(Relationship::Placeholder1, c, b, 3.0);
        agents.set_relationship(Relationship::Placeholder1, d, e, 4.0);

        agents.despawn(b);

        assert!(
            agents
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert!(
            agents
                .relationship(Relationship::Placeholder1, b, c)
                .is_none()
        );
        assert!(
            agents
                .relationship(Relationship::Placeholder1, c, b)
                .is_none()
        );
        assert_eq!(
            agents.relationship(Relationship::Placeholder1, d, e),
            Some(4.0)
        );
    }

    #[test]
    fn relationships_are_isolated_by_type() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);

        agents.set_relationship(Relationship::Placeholder1, a, b, 1.0);
        agents.set_relationship(Relationship::Placeholder2, a, b, 2.0);
        agents.remove_relationship(Relationship::Placeholder1, a, b);

        assert!(
            agents
                .relationship(Relationship::Placeholder1, a, b)
                .is_none()
        );
        assert_eq!(
            agents.relationship(Relationship::Placeholder2, a, b),
            Some(2.0)
        );
    }

    #[test]
    fn relationships_from_are_sorted_by_agent_id() {
        let mut agents = Agents::default();
        let from = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);
        let c = insert_agent(&mut agents);

        assert!(b < c);

        agents.set_relationship(Relationship::Placeholder1, from, c, 2.0);
        agents.set_relationship(Relationship::Placeholder1, from, b, 1.0);

        let edges: Vec<_> = agents
            .relationships_from(Relationship::Placeholder1, from)
            .collect();
        assert_eq!(edges, vec![(b, 1.0), (c, 2.0)]);
    }

    #[test]
    fn sets_add_remove_and_iterate() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);

        assert!(agents.add_to_set(Set::People, a));
        assert!(agents.add_to_set(Set::People, b));
        assert!(!agents.add_to_set(Set::People, a));

        let mut members: Vec<_> = agents.iter_set_ids(Set::People).collect();
        members.sort();
        assert_eq!(members, vec![a, b]);

        assert!(agents.remove_from_set(Set::People, a));
        assert!(!agents.remove_from_set(Set::People, a));
        let members: Vec<_> = agents.iter_set_ids(Set::People).collect();
        assert_eq!(members, vec![b]);
    }

    #[test]
    fn sets_despawn_removes_membership() {
        let mut agents = Agents::default();
        let a = insert_agent(&mut agents);
        let b = insert_agent(&mut agents);

        agents.add_to_set(Set::Settlements, a);
        agents.add_to_set(Set::Settlements, b);

        agents.despawn(a);
        let members: Vec<_> = agents.iter_set_ids(Set::Settlements).collect();
        assert_eq!(members, vec![b]);
    }

    #[test]
    fn vars_default_to_zero() {
        let mut agents = Agents::default();
        agents.spawn();
        let agent = agents.iter().next().unwrap();

        let vars = agents.vars(agent.id);
        assert_eq!(vars.get(Var::Renown), 0.0);
        assert_eq!(vars.get(Var::Population), 0.0);
        assert_eq!(vars.get(Var::Prosperity), 0.0);
    }

    #[test]
    fn vars_mut_set_persists() {
        let mut agents = Agents::default();
        agents.spawn();
        let id = agents.ids().next().unwrap();

        {
            let mut vars = agents.vars_mut(id);
            vars.set(Var::Renown, 3.5);
            vars.set(Var::Population, 10.0);
        }

        let vars = agents.vars(id);
        assert_eq!(vars.get(Var::Renown), 3.5);
        assert_eq!(vars.get(Var::Population), 10.0);
    }

    #[test]
    fn vars_copy_out_to_vec() {
        let mut agents = Agents::default();
        agents.spawn();
        let id = agents.ids().next().unwrap();

        {
            let mut vars = agents.vars_mut(id);
            vars.set(Var::Renown, 1.25);
            vars.set(Var::Prosperity, 7.0);
        }

        let vars = agents.vars(id);
        let mut buffer = Vec::new();
        let copy = vars.copy_out(&mut buffer);
        assert_eq!(copy.get(Var::Renown), 1.25);
        assert_eq!(copy.get(Var::Prosperity), 7.0);
    }

    #[test]
    fn vars_copy_out_mut_is_independent() {
        let mut agents = Agents::default();
        agents.spawn();
        let id = agents.ids().next().unwrap();

        {
            let mut vars = agents.vars_mut(id);
            vars.set(Var::Renown, 2.0);
        }

        let vars = agents.vars(id);
        let mut buffer = Vec::new();
        let mut copy = vars.copy_out_mut(&mut buffer);
        copy.set(Var::Renown, 9.0);

        let vars = agents.vars(id);
        assert_eq!(vars.get(Var::Renown), 2.0);
    }

    #[test]
    fn vars_agent_methods_are_consistent() {
        let mut agents = Agents::default();
        let id = agents.spawn().id;

        {
            let agent = agents.get_mut(id).unwrap();
            let mut vars = agent.vars_mut();
            vars.set(Var::Renown, 4.0);
        }

        let agent = agents.get(id).unwrap();
        let vars = agent.vars();
        assert_eq!(vars.get(Var::Renown), 4.0);
    }

    #[test]
    fn agent_flags_default_false() {
        let agent = Agent::default();
        assert!(!agent.flags.get(Flag::IsFarmer));
    }

    #[test]
    fn agent_flags_set_and_clear() {
        let mut agent = Agent::default();
        agent.flags.set(Flag::IsFarmer, true);
        assert!(agent.flags.get(Flag::IsFarmer));
        agent.flags.set(Flag::IsFarmer, false);
        assert!(!agent.flags.get(Flag::IsFarmer));
    }
}
