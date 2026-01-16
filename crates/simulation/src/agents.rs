use slotmap::{SlotMap, new_key_type};
use strum::{EnumCount, EnumIter};

use crate::{names::Name, parties::PartyId};

new_key_type! { pub struct AgentId; }

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
/// Identifies a slice in the properties bin. The index is the LOGICAL INDEX, ie, the actual offset
/// of the bin array is at index * property_bin_size[type]
struct PropertyBinId(usize);

pub struct Agents {
    entries: SlotMap<AgentId, Agent>,
    property_bin_size: [usize; AgentType::COUNT],
    properties: [Vec<f64>; AgentType::COUNT],
    property_free_list: [Vec<PropertyBinId>; AgentType::COUNT],
}

impl Default for Agents {
    fn default() -> Self {
        Self {
            entries: SlotMap::default(),
            property_bin_size: [PersonVar::COUNT, SettlementVar::COUNT],
            properties: Default::default(),
            property_free_list: Default::default(),
        }
    }
}

impl Agents {
    pub fn spawn(&mut self, typ: AgentType) -> AgentId {
        let agent_id = self.entries.insert(Agent::default());
        let mut agent = Agent::default();
        agent.id = agent_id;
        agent.typ = typ;

        let type_idx = typ as usize;
        let bin_size = self.property_bin_size[type_idx];
        agent.props_id = match self.property_free_list[type_idx].pop() {
            Some(id) => {
                // Reset bin contents
                let idx = id.0 * bin_size;
                let bin = &mut self.properties[type_idx][idx..idx + bin_size];
                for x in bin {
                    *x = 0.;
                }
                id
            }
            None => {
                let bin = &mut self.properties[type_idx];
                let id = bin.len() / bin_size;
                let init_values = std::iter::repeat_n(0.0, bin_size);
                bin.extend(init_values);
                PropertyBinId(id)
            }
        };

        self.entries[agent_id] = agent;
        agent_id
    }

    pub fn despawn(&mut self, id: AgentId) {
        let agent = match self.entries.remove(id) {
            Some(x) => x,
            None => {
                return;
            }
        };
        let type_idx = agent.typ as usize;
        self.property_free_list[type_idx].push(agent.props_id);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Agent> + use<'_> {
        self.entries.values()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter, EnumCount)]
pub(crate) enum AgentType {
    Person,
    Settlement,
}

impl Default for AgentType {
    fn default() -> Self {
        Self::Person
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter, EnumCount)]
pub(crate) enum PersonVar {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter, EnumCount)]
pub(crate) enum SettlementVar {}

#[derive(Clone, Copy, Default)]
pub struct Agent {
    pub id: AgentId,
    pub typ: AgentType,
    pub name: Name,
    props_id: PropertyBinId,
    pub party: Option<PartyId>,
}

impl Agents {}
