use std::collections::{BTreeMap, BTreeSet};

use crate::{names::Name, parties::PartyId};
use slotmap::{Key, KeyData, SecondaryMap, SlotMap, new_key_type};
use strum::{EnumCount, EnumIter};
use util::span::Span;

new_key_type! { pub struct AgentId; }

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Var {
    /// Person
    Renown,
    /// Settlement
    Population,
    Prosperity,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Hierarchy {
    FactionMembership,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumCount, EnumIter)]
pub(crate) enum Relationship {
    Placeholder1,
    Placeholder2,
}

#[derive(Default)]
pub(crate) struct Agents {
    entries: SlotMap<AgentId, Agent>,
    var_storage: VarStorage,
    hierarchies: [HierarchyData; Hierarchy::COUNT],
    relationships: [RelationshipData; Relationship::COUNT],
}

impl Agents {
    pub(crate) fn spawn(&mut self) -> &mut Agent {
        let id = self.entries.insert(Agent::default());
        let vars = self.var_storage.alloc();
        let agent = Agent {
            id,
            vars,
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

        // Add var packs to free lists
        self.var_storage.free(entity.vars);

        for hierarchy in &mut self.hierarchies {
            hierarchy.remove_agent(id);
        }

        for relationship in &mut self.relationships {
            relationship.remove_agent(id);
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = AgentId> {
        self.entries.keys()
    }

    pub(crate) fn get(&self, id: AgentId) -> Option<&Agent> {
        self.entries.get(id)
    }

    pub(crate) fn get_mut(&mut self, id: AgentId) -> Option<&mut Agent> {
        self.entries.get_mut(id)
    }

    pub fn vars(&self, id: AgentId) -> Vars<'_> {
        let slice = self
            .entries
            .get(id)
            .map(|agent| agent.vars.view(self.var_storage.inner()))
            .unwrap_or_default();
        Vars(slice)
    }

    pub fn vars_mut(&mut self, id: AgentId) -> VarsMut<'_> {
        let slice = self
            .entries
            .get(id)
            .map(|agent| agent.vars.view_mut(self.var_storage.inner_mut()))
            .unwrap_or_default();
        VarsMut(slice)
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

#[derive(Default, Clone, Copy)]
pub(crate) struct Agent {
    pub id: AgentId,
    pub name: Name,
    pub party: PartyId,
    vars: Span,
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
struct VarStorage {
    vars: Vec<f64>,
    free_list: Vec<Span>,
}

impl VarStorage {
    fn alloc(&mut self) -> Span {
        match self.free_list.pop() {
            Some(span) => {
                span.fill_with_copy(&mut self.vars, 0.);
                span
            }
            None => {
                let zeros = std::iter::repeat_n(0., Var::COUNT);
                Span::of_extension(&mut self.vars, zeros)
            }
        }
    }

    fn free(&mut self, span: Span) {
        self.free_list.push(span);
    }

    fn inner(&self) -> &[f64] {
        &self.vars
    }

    fn inner_mut(&mut self) -> &mut [f64] {
        &mut self.vars
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
    fn vars_default_to_zero() {
        let mut agents = Agents::default();
        agents.spawn();
        let id = agents.iter().next().unwrap();

        let vars = agents.vars(id);
        assert_eq!(vars.get(Var::Renown), 0.0);
        assert_eq!(vars.get(Var::Population), 0.0);
        assert_eq!(vars.get(Var::Prosperity), 0.0);
    }

    #[test]
    fn vars_mut_set_persists() {
        let mut agents = Agents::default();
        agents.spawn();
        let id = agents.iter().next().unwrap();

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
        let id = agents.iter().next().unwrap();

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
        let id = agents.iter().next().unwrap();

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
}
