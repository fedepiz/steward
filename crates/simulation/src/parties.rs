use slotmap::{SecondaryMap, SlotMap, new_key_type};

use util::span::Span;

use crate::{agents::AgentId, geom::V2, names::Name};

new_key_type! { pub(crate) struct PartyId; }
new_key_type! { pub(crate) struct PartyTypeId; }

#[derive(Default)]
pub(crate) struct Parties {
    types: SlotMap<PartyTypeId, PartyType>,
    entities: SlotMap<PartyId, Party>,
    pub(crate) detections: Detections,
}

#[derive(Default)]
pub(crate) struct Detections {
    data: Vec<Detection>,
    spans: SecondaryMap<PartyId, Span>,
}

impl Detections {
    pub(crate) fn clear(&mut self) {
        self.data.clear();
        self.spans.clear();
    }

    pub(crate) fn set(&mut self, id: PartyId, detections: &[Detection]) {
        let detections = detections.iter().copied();
        let span = Span::of_extension(&mut self.data, detections);
        self.spans.insert(id, span);
    }

    pub(crate) fn get(&self, id: PartyId) -> &[Detection] {
        self.spans
            .get(id)
            .map(|span| span.view(&self.data))
            .unwrap_or_default()
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Detection {
    pub target: PartyId,
    pub distance: f32,
}

impl Parties {
    pub(crate) fn spawn(&mut self) -> &mut Party {
        let id = self.entities.insert(Party::default());
        let data = &mut self.entities[id];
        data.id = id;
        data
    }

    pub(crate) fn spawn_with_type(&mut self, type_id: PartyTypeId) -> &mut Party {
        let typ = self.types.get(type_id).copied().unwrap();
        let entity = self.spawn();
        Self::set_type(entity, &typ);
        entity
    }

    fn set_type(entity: &mut Party, typ: &PartyType) {
        entity.type_id = typ.id;
        entity.body.size = typ.size;
    }

    pub(crate) fn add_type(&mut self) -> &mut PartyType {
        let id = self.types.insert(PartyType::default());
        let data = &mut self.types[id];
        data.id = id;
        data
    }

    pub(crate) fn find_type_by_tag(&self, tag: &str) -> Option<PartyType> {
        self.types.values().find(|typ| typ.tag == tag).copied()
    }

    pub(crate) fn get_type(&self, id: PartyTypeId) -> PartyType {
        self.types.get(id).copied().unwrap_or_default()
    }

    pub(crate) fn iter(&self) -> slotmap::basic::Values<'_, PartyId, Party> {
        self.entities.values()
    }

    pub(crate) fn iter_mut(&mut self) -> slotmap::basic::ValuesMut<'_, PartyId, Party> {
        self.entities.values_mut()
    }

    pub(crate) fn len(&self) -> usize {
        self.entities.len()
    }
}

impl std::ops::Index<PartyId> for Parties {
    type Output = Party;

    fn index(&self, index: PartyId) -> &Self::Output {
        &self.entities[index]
    }
}

impl std::ops::IndexMut<PartyId> for Parties {
    fn index_mut(&mut self, index: PartyId) -> &mut Self::Output {
        &mut self.entities[index]
    }
}

impl std::ops::Index<PartyTypeId> for Parties {
    type Output = PartyType;

    fn index(&self, index: PartyTypeId) -> &Self::Output {
        &self.types[index]
    }
}

impl std::ops::IndexMut<PartyTypeId> for Parties {
    fn index_mut(&mut self, index: PartyTypeId) -> &mut Self::Output {
        &mut self.types[index]
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum Goal {
    Idle,
    MoveTo(V2),
    ToParty { target: PartyId, distance: f32 },
}

impl Default for Goal {
    fn default() -> Self {
        Self::Idle
    }
}

#[derive(Default, Clone, Copy)]
pub(crate) struct PartyType {
    pub id: PartyTypeId,
    pub tag: &'static str,
    pub image: &'static str,
    pub name: Name,
    pub size: f32,
    pub always_show_name: bool,
    pub layer: usize,
}

#[derive(Default, Clone, Copy)]
pub(crate) struct Party {
    pub id: PartyId,
    pub name: Name,
    pub type_id: PartyTypeId,
    pub body: Body,
    pub speed: f32,
    pub is_player: bool,
    pub agent: AgentId,
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub(crate) enum MovementTarget {
    Immobile,
    FixedPos(V2),
    Party(PartyId),
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

pub(crate) struct PartyAction {
    pub movement_target: MovementTarget,
}

pub(crate) fn party_ai(_: &Party, detections: &[Detection], goal: Goal) -> PartyAction {
    let movement_target = match goal {
        Goal::Idle => MovementTarget::Immobile,
        Goal::MoveTo(pos) => MovementTarget::FixedPos(pos),
        Goal::ToParty { target, distance } => {
            let close_to_target = detections
                .iter()
                .find(|d| target == d.target)
                .map(|d| d.distance <= distance)
                .unwrap_or(false);

            if !close_to_target {
                MovementTarget::Party(target)
            } else {
                MovementTarget::Immobile
            }
        }
    };

    PartyAction { movement_target }
}
