use std::hash::{Hash, Hasher};

use strum::{EnumCount, EnumIter, IntoEnumIterator};

use util::bitset::BitSet;

use crate::V2;

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub(crate) struct ThingId {
    slot: u32,
    generation: u32,
}

impl ThingId {
    #[inline]
    pub(crate) fn null() -> Self {
        Self::default()
    }

    #[inline]
    pub(crate) fn is_null(self) -> bool {
        self.slot == 0
    }

    #[inline]
    pub(crate) fn is_valid(self) -> bool {
        self.slot != 0 && self.generation % 2 == 1
    }

    #[inline]
    pub(crate) fn as_valid(self) -> Option<ThingId> {
        if self.is_valid() { Some(self) } else { None }
    }

    #[inline]
    pub(crate) fn get_as_valid(self, ctx: &Things) -> Option<&Thing> {
        self.as_valid().map(|x| x.get(ctx))
    }

    #[inline]
    pub(crate) fn get_as_valid_mut(self, ctx: &mut Things) -> Option<&mut Thing> {
        self.as_valid().map(|x| x.get_mut(ctx))
    }

    #[inline]
    pub(crate) fn slot(self) -> usize {
        self.slot as usize
    }

    #[inline]
    pub(crate) fn get(self, things: &Things) -> &Thing {
        &things[self]
    }

    #[inline]
    pub(crate) fn get_mut(self, ctx: &mut Things) -> &mut Thing {
        &mut ctx[self]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Flag {
    Dummy,
    IsLocation,
    IsSettlement,
    IsPerson,
    IsPath,
    IsActivity,
    // Tokens are an abstract kind of thing used to model control of some kind of resource or power share.
    IsToken,
    Teleport,
    IsInvisible,
    // Insideness
    WantsToBeInside,
    IsInside,
    IsOrder,
}

impl Default for Flag {
    fn default() -> Self {
        Self::Dummy
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum List {
    Dummy,
    // Links a location to all the movable people currently at that location (inside or outside)
    AtLocation,
    // Links a person to all the thing they 'possess'.
    // This includes:
    // - settlements
    Possessions,
    // Links a 'liege' to all the people who are 'loyal' to them
    Subordinates,
    // Links messages to the player object
    Messages,
    // Links the order chain to the person that has received said orders
    Orders,
    // Tokens
    TokensSourced,
    TokensHeld,
    // Activity
    Partecipants,
}

impl Default for List {
    fn default() -> Self {
        List::Dummy
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
struct ListThingData {
    // For the contained element
    parent: ThingId,
    sibling: ThingId,
    // For the container element
    children: (ThingId, ThingId),
    length: usize,
}

const NUM_FLAGS: usize = Flag::COUNT;
const NUM_LISTS: usize = List::COUNT;

#[derive(Clone, Copy, Default)]
pub(crate) struct Thing {
    id: ThingId,
    flags: BitSet<NUM_FLAGS>,
    next_free: ThingId,
    tag: &'static str,
    tag_chain_next: ThingId,
    lists: [ListThingData; NUM_LISTS],
    // Generic
    pub name: &'static str,
    pub owner: ThingId,
    pub kind: u16,
    // Movement
    pub sprite: &'static str,
    pub body: Body,
    pub wait_time: f32,
    pub movement_time: f32,
    pub destination: ThingId,
    // Orders
    pub current_order: ThingId,
    // Path
    pub edge_from: ThingId,
    pub edge_to: ThingId,
    // Generic parameter things
    pub params: [ThingId; 4],
}

impl Thing {
    #[inline]
    pub(crate) fn id(&self) -> ThingId {
        self.id
    }

    #[inline]
    pub(crate) fn set_flag(&mut self, flag: Flag, value: bool) {
        self.flags.set(flag as usize, value);
    }

    #[inline]
    pub(crate) fn flag(&self, flag: Flag) -> bool {
        self.flags.get(flag as usize)
    }

    #[inline]
    pub(crate) fn parent(&self, list: List) -> ThingId {
        self.lists[list as usize].parent
    }

    #[inline]
    pub(crate) fn first(&self, list: List) -> ThingId {
        self.lists[list as usize].children.0
    }

    #[inline]
    pub(crate) fn last(&self, list: List) -> ThingId {
        self.lists[list as usize].children.1
    }

    #[inline]
    pub(crate) fn list_len(&self, list: List) -> usize {
        self.lists[list as usize].length
    }
}

#[derive(Default, Clone, Copy)]
pub(crate) struct Body {
    pub x: f32,
    pub y: f32,
    pub size: u8,
    pub layer: u8,
}

impl Body {
    pub(crate) fn pos(&self) -> V2 {
        V2::new(self.x, self.y)
    }
}

pub(crate) const NUM_THINGS: usize = 10_000;
const LAST_IDX_THING: usize = NUM_THINGS - 1;

#[derive(Default, Clone, Copy)]
struct MetaData {
    free_list_head: ThingId,
    free_list_tail: ThingId,
}

#[derive(Default)]
pub(crate) struct Things {
    entries: Vec<Thing>,
    write_buffer: Vec<Thing>,
    tag_hash_buckets: Vec<ThingId>,
    meta: MetaData,
}

impl Things {
    fn detach_children_from_list(&mut self, list: List, parent: ThingId) {
        let list_idx = list as usize;

        let mut child = self.entries[parent.slot as usize].lists[list_idx]
            .children
            .0;
        while !child.is_null() {
            let slot = child.slot as usize;
            let next = self.entries[slot].lists[list_idx].sibling;

            let child_list = &mut self.entries[slot].lists[list_idx];
            child_list.parent = ThingId::null();
            child_list.sibling = ThingId::null();

            child = next;
        }

        let parent_list = &mut self.entries[parent.slot as usize].lists[list_idx];
        parent_list.children = (ThingId::null(), ThingId::null());
        parent_list.length = 0;
    }

    pub(crate) fn new() -> Self {
        let mut entries = Vec::from_iter((0..NUM_THINGS).map(|_| Thing::default()));
        // Create a 'chain' of next_free pointing at each other, with the last pointing to "the null"
        for (idx, thing) in entries.iter_mut().enumerate() {
            let slot = if idx != LAST_IDX_THING { idx + 1 } else { 0 };
            // Set up my index
            thing.id.slot = idx as u32;
            // Set up the next index
            thing.next_free = ThingId {
                slot: slot as u32,
                generation: 0,
            }
        }
        let write_buffer = entries.clone();
        let mut meta = MetaData::default();
        // First element in the free list has index 1
        meta.free_list_head = ThingId {
            slot: 1,
            generation: 0,
        };
        // Last has index last index
        meta.free_list_tail = ThingId {
            slot: LAST_IDX_THING as u32,
            generation: 0,
        };

        let tag_hash_heads = vec![ThingId::default(); 256];

        Self {
            entries,
            write_buffer,
            tag_hash_buckets: tag_hash_heads,
            meta,
        }
    }

    pub(crate) fn spawn(&mut self) -> &mut Thing {
        if self.meta.free_list_head.is_null() {
            return &mut self.entries[0];
        }
        let thing = &mut self.entries[self.meta.free_list_head.slot as usize];

        assert!(thing.id == self.meta.free_list_head);
        thing.id.generation += 1;
        assert!(thing.id.generation % 2 == 1);
        // Advance free list pointer
        self.meta.free_list_head = thing.next_free;

        // Reset all fields, except id
        *thing = Thing {
            id: thing.id,
            ..Default::default()
        };

        thing
    }

    pub(crate) fn spawn_with_tag(&mut self, tag: &'static str) -> &mut Thing {
        let id = self.spawn().id();
        self.set_tag(tag, id);
        &mut self[id]
    }

    pub(crate) fn despawn(&mut self, id: ThingId) {
        if !id.is_valid() {
            return;
        }

        for list in List::iter() {
            self.remove_from_list(list, id);
        }

        for list in List::iter() {
            self.detach_children_from_list(list, id);
        }

        // Check for tag and remove
        {
            let tag = self.entries[id.slot as usize].tag;
            if !tag.is_empty() {
                self.untag(tag);
            }
        }

        let thing = &mut self.entries[id.slot as usize];

        // Remove myself from all lists
        assert!(thing.id == id);
        thing.id.generation += 1;
        assert!(thing.id.generation % 2 == 0);
        let end = &mut self.entries[self.meta.free_list_tail.slot as usize];
        end.next_free = id;
        self.meta.free_list_tail = id;
    }

    pub(crate) fn exists(&self, id: ThingId) -> bool {
        !self[id].id.is_null()
    }

    pub(crate) fn set_tag(&mut self, tag: &'static str, id: ThingId) {
        // Cannot already have a tag, tag can't be free
        assert!(self[id].tag.is_empty() && !tag.is_empty());
        self[id].tag = tag;

        let hash = {
            let mut hasher = std::hash::DefaultHasher::new();
            tag.hash(&mut hasher);
            hasher.finish()
        } as usize;
        let bucket_idx = hash % self.tag_hash_buckets.len();
        let bucket_first = self.tag_hash_buckets[bucket_idx];
        if bucket_first.is_null() {
            self.tag_hash_buckets[bucket_idx] = id;
        } else {
            // Find the end of the tag list
            let mut cursor = self.tag_hash_buckets[bucket_idx];
            while self[cursor].tag_chain_next.is_valid() {
                cursor = self[cursor].tag_chain_next;
            }
            // We now point at the end of the list
            self[cursor].tag_chain_next = id;
        }
    }

    #[inline]
    fn interal_lookup_tag(&self, tag: &str) -> (usize, ThingId, ThingId) {
        let hash = {
            let mut hasher = std::hash::DefaultHasher::new();
            tag.hash(&mut hasher);
            hasher.finish()
        } as usize;
        let bucket_idx = hash % self.tag_hash_buckets.len();

        let mut precursor = ThingId::null();
        let mut cursor = self.tag_hash_buckets[bucket_idx];
        while cursor.is_valid() {
            let thing = &self[cursor];
            if thing.tag == tag {
                return (bucket_idx, precursor, cursor);
            }
            precursor = cursor;
            cursor = thing.tag_chain_next;
        }
        Default::default()
    }

    pub(crate) fn lookup_tag(&self, tag: &str) -> ThingId {
        self.interal_lookup_tag(tag).2
    }

    pub(crate) fn untag(&mut self, tag: &str) {
        if tag.is_empty() {
            return;
        }
        let (bucket_idx, precursor, cursor) = self.interal_lookup_tag(tag);
        // Reset my tag and extract my next reference
        let my_next = {
            let cursor = &mut self[cursor];
            cursor.tag = "";
            std::mem::take(&mut cursor.tag_chain_next)
        };
        if precursor.is_null() {
            // This was the head of a tag chain, we update the bin
            self.tag_hash_buckets[bucket_idx] = my_next
        } else {
            // This is not the end of the tag chain: set the precusor's next to my next
            self[precursor].tag_chain_next = my_next;
        }
    }

    pub(crate) fn iter_list(&self, list: List, id: ThingId) -> ListChildrenIter<'_> {
        assert!(id.is_valid());
        assert!(self.entries[id.slot as usize].id == id);

        let list_data = &self.entries[id.slot as usize].lists[list as usize];

        ListChildrenIter {
            things: self,
            list,
            next: list_data.children.0,
            len: list_data.length,
        }
    }

    pub(crate) fn iter_list_get(
        &self,
        list: List,
        id: ThingId,
    ) -> impl Iterator<Item = &Thing> + ExactSizeIterator {
        self.iter_list(list, id).map(|id| id.get(self))
    }

    pub(crate) fn add_to_list(&mut self, list: List, parent: ThingId, child: ThingId) {
        if !child.is_valid() {
            return;
        }

        self.remove_from_list(list, child);

        if !parent.is_valid() {
            return;
        }

        let list_idx = list as usize;
        let old_tail = self[parent].lists[list_idx].children.1;
        let is_empty = self[parent].lists[list_idx].length == 0;

        {
            let child_list = &mut self[child].lists[list_idx];
            child_list.parent = parent;
            child_list.sibling = ThingId::null();
        }

        if is_empty {
            let parent_list = &mut self[parent].lists[list_idx];
            parent_list.children = (child, child);
            parent_list.length = 1;
            return;
        }

        self[old_tail].lists[list_idx].sibling = child;
        let parent_list = &mut self[parent].lists[list_idx];
        parent_list.children.1 = child;
        parent_list.length += 1;
    }

    pub(crate) fn remove_from_list(&mut self, list: List, child: ThingId) {
        if !child.is_valid() {
            return;
        }

        let list_idx = list as usize;
        let parent = self[child].lists[list_idx].parent;
        if !parent.is_valid() {
            let child_list = &mut self.entries[child.slot as usize].lists[list_idx];
            child_list.parent = ThingId::null();
            child_list.sibling = ThingId::null();
            return;
        }

        let parent_slot = parent.slot as usize;
        let mut prev = ThingId::null();
        let mut current = self.entries[parent_slot].lists[list_idx].children.0;

        while current.is_valid() && current != child {
            prev = current;
            current = self.entries[current.slot as usize].lists[list_idx].sibling;
        }

        if current != child {
            let child_list = &mut self.entries[child.slot as usize].lists[list_idx];
            child_list.parent = ThingId::null();
            child_list.sibling = ThingId::null();
            return;
        }

        let next = self.entries[current.slot as usize].lists[list_idx].sibling;

        if prev.is_null() {
            self.entries[parent_slot].lists[list_idx].children.0 = next;
        } else {
            self.entries[prev.slot as usize].lists[list_idx].sibling = next;
        }

        {
            let child_list = &mut self[child].lists[list_idx];
            child_list.parent = ThingId::null();
            child_list.sibling = ThingId::null();
        }

        {
            let mut len = 0;
            let mut tail = ThingId::null();
            let mut cursor = self.entries[parent_slot].lists[list_idx].children.0;
            while cursor.is_valid() {
                len += 1;
                tail = cursor;
                cursor = self.entries[cursor.slot as usize].lists[list_idx].sibling;
            }

            let parent_list = &mut self.entries[parent_slot].lists[list_idx];
            parent_list.length = len;
            parent_list.children.1 = tail;
            if len == 0 {
                parent_list.children.0 = ThingId::null();
            }
        }
    }

    pub(crate) fn iter(&self) -> ThingsIterator<'_> {
        ThingsIterator {
            entries: &self.entries,
            idx: 1,
        }
    }
}

impl std::ops::Index<ThingId> for Things {
    type Output = Thing;

    fn index(&self, index: ThingId) -> &Self::Output {
        let slot = if !index.is_valid() { 0 } else { index.slot };
        let thing = &self.entries[slot as usize];
        if thing.id != index {
            return &self.entries[0];
        }
        thing
    }
}

impl std::ops::IndexMut<ThingId> for Things {
    fn index_mut(&mut self, index: ThingId) -> &mut Self::Output {
        let slot = if !index.is_valid() { 0 } else { index.slot };
        if self.entries[slot as usize].id != index {
            return &mut self.entries[0];
        }
        &mut self.entries[slot as usize]
    }
}

impl<'a> IntoIterator for &'a Things {
    type Item = &'a Thing;
    type IntoIter = ThingsIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub(crate) struct ListChildrenIter<'a> {
    things: &'a Things,
    list: List,
    next: ThingId,
    len: usize,
}

impl Iterator for ListChildrenIter<'_> {
    type Item = ThingId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 || self.next.is_null() {
            return None;
        }

        let current = self.next;
        self.next = self.things.entries[current.slot as usize].lists[self.list as usize].sibling;
        self.len -= 1;
        Some(current)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }
}

impl ExactSizeIterator for ListChildrenIter<'_> {
    fn len(&self) -> usize {
        self.len
    }
}

// Iterator that returns all valid (allocated) things.
pub(crate) struct ThingsIterator<'a> {
    entries: &'a [Thing],
    idx: usize,
}

impl<'a> Iterator for ThingsIterator<'a> {
    type Item = &'a Thing;

    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < NUM_THINGS {
            let idx = self.idx;
            self.idx += 1;

            let thing = &self.entries[idx];
            if thing.id.is_valid() {
                return Some(thing);
            }
        }

        None
    }
}

impl Things {
    const SKIP_COPY_IF_POSSIBLE: bool = false;

    pub(crate) fn write_pass(&mut self, mut body: impl FnMut(&Things, &mut Thing, &mut Commands)) {
        let _span = tracing::trace_span!("Write Pass").entered();
        let mut write_buffer = std::mem::take(&mut self.write_buffer);
        let mut commands = Commands::new();

        for (thing, target) in self.entries.iter().zip(write_buffer.iter_mut()) {
            // Do the actual copy and update only if
            // - thing is valid, and therefore logic runs on it
            // - target has a different id than thing, in which case some structural spawning/despawning happened
            // - SKIP_COPY_IF_POSSIBLE is false, in which case we are asking to always copy
            if !Self::SKIP_COPY_IF_POSSIBLE || thing.id.is_valid() || thing.id() != target.id() {
                *target = *thing;
                if thing.id().is_valid() {
                    body(self, target, &mut commands);
                }
            }
        }

        self.write_buffer = write_buffer;
        std::mem::swap(&mut self.entries, &mut self.write_buffer);
        self.appy_commands(commands);
    }

    pub(crate) fn exclusive_pass(&mut self, mut body: impl FnMut(&mut Self, Thing)) {
        for i in 0..self.entries.len() {
            let thing = &self.entries[i];
            if !thing.id.is_valid() {
                continue;
            }
            let thing = *thing;
            body(self, thing);
        }
    }

    pub(crate) fn readonly_pass(&self, mut body: impl FnMut(&Things, &Thing)) {
        let _span = tracing::info_span!("Readonly Pass").entered();
        for thing in &self.entries {
            if thing.id.is_valid() {
                body(self, thing);
            }
        }
    }

    pub(crate) fn with_commands<R>(&mut self, f: impl FnOnce(&mut Self, &mut Commands) -> R) -> R {
        let _span = tracing::info_span!("Sequential commands Pass").entered();
        let mut commands = Commands::new();
        let value = f(self, &mut commands);
        self.appy_commands(commands);
        value
    }

    fn appy_commands(&mut self, mut commands: Commands) {
        for id in commands.despawns.drain(..) {
            self.despawn(id);
        }

        for (idx, spawn) in commands.spawns.drain(..).enumerate() {
            let thing = self.spawn();
            let this = thing.id;
            *thing = spawn;
            thing.id = this;
            commands.temp_id_map[idx] = thing.id;
        }

        for ListMutation {
            list,
            parent,
            child,
        } in std::mem::take(&mut commands.list_mutations)
        {
            let parent = commands.resolve(parent);
            let child = commands.resolve(child);
            self.add_to_list(list, parent, child);
        }
    }
}

#[derive(Default)]
pub(crate) struct Commands {
    temp_id_map: Vec<ThingId>,
    list_mutations: Vec<ListMutation>,
    despawns: Vec<ThingId>,
    spawns: Vec<Thing>,
}

impl Commands {
    fn new() -> Self {
        Self::default()
    }

    fn resolve(&self, rf: ThingRef) -> ThingId {
        match rf {
            ThingRef::Id(id) => id,
            ThingRef::TempId(idx) => self.temp_id_map[idx as usize],
        }
    }

    pub fn add_to_list(
        &mut self,
        list: List,
        parent: impl Into<ThingRef>,
        child: impl Into<ThingRef>,
    ) {
        let parent = parent.into();
        let child = child.into();
        self.list_mutations.push(ListMutation {
            list,
            parent,
            child,
        });
    }

    pub fn remove_from_list(&mut self, list: List, child: impl Into<ThingRef>) {
        self.list_mutations.push(ListMutation {
            list,
            parent: ThingId::null().into(),
            child: child.into(),
        });
    }

    pub fn despawn(&mut self, id: ThingId) {
        self.despawns.push(id);
    }

    pub fn spawn(&mut self) -> (ThingRef, &mut Thing) {
        let temp_id = ThingRef::TempId(self.temp_id_map.len() as u32);
        self.temp_id_map.push(ThingId::default());
        self.spawns.push(Thing::default());
        (temp_id, self.spawns.last_mut().unwrap())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ThingRef {
    Id(ThingId),
    TempId(u32),
}

impl From<ThingId> for ThingRef {
    fn from(value: ThingId) -> Self {
        Self::Id(value)
    }
}

#[derive(Clone, Copy)]
struct ListMutation {
    list: List,
    parent: ThingRef,
    child: ThingRef,
}
