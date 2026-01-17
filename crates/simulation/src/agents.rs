use std::collections::BTreeSet;

use crate::{names::Name, parties::PartyId};
use slotmap::{Key, KeyData, SecondaryMap, SlotMap, new_key_type};
use strum::{EnumCount, EnumIter};
use util::span::Span;

new_key_type! { pub struct AgentId; }

#[derive(Default)]
pub(crate) struct Agents {
    entries: SlotMap<AgentId, Agent>,
    vars: Vec<f64>,
    vars_free_list: Vec<Span>,
    hierarchies: [HierarchyData; Hierarchy::COUNT],
}

impl Agents {
    pub(crate) fn spawn(&mut self) {
        let id = self.entries.insert(Agent::default());
        let vars = match self.vars_free_list.pop() {
            Some(span) => {
                span.fill_with_copy(&mut self.vars, 0.);
                span
            }
            None => {
                let zeros = std::iter::repeat_n(0., Var::COUNT);
                Span::of_extension(&mut self.vars, zeros)
            }
        };
        let agent = Agent {
            id,
            vars,
            ..Default::default()
        };
        let entry = &mut self.entries[id];
        *entry = agent;
    }

    pub(crate) fn despawn(&mut self, id: AgentId) {
        let entity = match self.entries.remove(id) {
            Some(entity) => entity,
            None => {
                return;
            }
        };

        // Add var packs to free lists
        self.vars_free_list.push(entity.vars);
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = AgentId> {
        self.entries.keys()
    }

    pub fn name(&self, id: AgentId) -> Name {
        self.entries
            .get(id)
            .map(|agent| agent.name)
            .unwrap_or_default()
    }

    pub fn set_name(&mut self, id: AgentId, name: Name) {
        if let Some(agent) = self.entries.get_mut(id) {
            agent.name = name;
        }
    }

    pub fn party(&self, id: AgentId) -> PartyId {
        self.entries
            .get(id)
            .map(|agent| agent.party)
            .unwrap_or_default()
    }

    pub fn set_party(&mut self, id: AgentId, party: PartyId) {
        if let Some(agent) = self.entries.get_mut(id) {
            agent.party = party;
        }
    }

    pub fn vars(&self, id: AgentId) -> Vars<'_> {
        let slice = self
            .entries
            .get(id)
            .map(|agent| agent.vars.view(&self.vars))
            .unwrap_or_default();
        Vars(slice)
    }

    pub fn vars_mut(&mut self, id: AgentId) -> VarsMut<'_> {
        let slice = self
            .entries
            .get(id)
            .map(|agent| agent.vars.view_mut(&mut self.vars))
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
}

#[derive(Default, Clone, Copy)]
struct Agent {
    id: AgentId,
    name: Name,
    party: PartyId,
    vars: Span,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Vars<'a>(&'a [f64]);

impl Vars<'_> {
    pub(crate) fn get(&self, var: Var) -> f64 {
        let idx = var as usize;
        self.0.get(idx).copied().unwrap_or(0.)
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
    pub(crate) fn get(&self, var: Var) -> f64 {
        let idx = var as usize;
        self.0.get(idx).copied().unwrap_or(0.)
    }

    pub(crate) fn set(&mut self, var: Var, value: f64) {
        let idx = var as usize;
        if let Some(var) = self.0.get_mut(idx) {
            *var = value;
        }
    }

    pub(crate) fn into_shared(self) -> Vars<'a> {
        Vars(&*self.0)
    }
}
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
            .range((parent, u64::MIN)..=(parent, u64::MAX))
            .map(|&(_, child)| AgentId::from(KeyData::from_ffi(child)))
    }
}
