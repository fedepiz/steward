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
    pub(crate) fn slot(self) -> usize {
        self.slot as usize
    }

    #[inline]
    pub(crate) fn get(self, things: &Things) -> &Thing {
        &things[self]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Flag {
    // This flag indicates the thing requires an owner (in Link) to be valid, or else it
    // will automatically despawn itselfs
    MustBeOwned,
    IsLocation,
    IsSettlement,
    IsPerson,
    IsPath,
    Teleport,
    IsVisible,
    IsInside,
    IsOrder,
    Test,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Var {
    Dummy,
    MovementTime,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Handle {
    Type,
}

pub(crate) type HandleValue = u16;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Link {
    Dummy,
    // A link that, in combination with the flag MustBeOwned, specified dynamic lifetime for this
    // thing. Used commonly for 'parts' of a whole
    Owner,
    // Generic A -> B links
    A,
    B,
    Destination,
    Order,
}

impl Default for Link {
    fn default() -> Self {
        Link::Dummy
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum List {
    Dummy,
    AtLocation,
    Messages,
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
const NUM_VARS: usize = Var::COUNT;
const NUM_HANDLES: usize = Handle::COUNT;
const NUM_LINKS: usize = Link::COUNT;
const NUM_LISTS: usize = List::COUNT;

#[derive(Clone, Copy, Default)]
pub(crate) struct Thing {
    id: ThingId,
    next_free: ThingId,
    tag: &'static str,
    tag_chain_next: ThingId,
    name: &'static str,
    sprite: &'static str,
    flags: BitSet<NUM_FLAGS>,
    vars: [f32; NUM_VARS],
    handles: [u16; NUM_HANDLES],
    links: [ThingId; NUM_LINKS],
    lists: [ListThingData; NUM_LISTS],
    pub(crate) body: Body,
}

impl Thing {
    #[inline]
    pub(crate) fn id(&self) -> ThingId {
        self.id
    }

    #[inline]
    pub(crate) fn name(&self) -> &'static str {
        self.name
    }

    #[inline]
    pub(crate) fn set_name(&mut self, name: &'static str) {
        self.name = name;
    }

    #[inline]
    pub(crate) fn sprite(&self) -> &'static str {
        self.sprite
    }

    #[inline]
    pub(crate) fn set_sprite(&mut self, sprite: &'static str) {
        self.sprite = sprite;
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
    pub(crate) fn set_var(&mut self, var: Var, value: f32) {
        self.vars[var as usize] = value;
    }

    #[inline]
    pub(crate) fn var(&self, var: Var) -> f32 {
        self.vars[var as usize]
    }

    #[inline]
    pub(crate) fn set_handle(&mut self, handle: Handle, value: HandleValue) {
        self.handles[handle as usize] = value;
    }

    #[inline]
    pub(crate) fn handle(&self, handle: Handle) -> HandleValue {
        self.handles[handle as usize]
    }

    #[inline]
    pub(crate) fn set_link(&mut self, link: Link, value: ThingId) {
        self.links[link as usize] = value;
    }

    #[inline]
    pub(crate) fn clear_link(&mut self, link: Link) {
        self.set_link(link, ThingId::null());
    }

    #[inline]
    pub(crate) fn link(&self, link: Link) -> ThingId {
        self.links[link as usize]
    }

    #[inline]
    pub(crate) fn parent(&self, list: List) -> ThingId {
        self.lists[list as usize].parent
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

pub(crate) const NUM_THINGS: usize = 32000;
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

        ListChildrenIter {
            things: self,
            list,
            next: self.entries[id.slot as usize].lists[list as usize]
                .children
                .0,
        }
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
        if child.is_null() {
            return;
        }

        let list_idx = list as usize;
        let parent = self[child].lists[list_idx].parent;
        if parent.is_null() {
            return;
        }

        let child_next = self[child].lists[list_idx].sibling;
        let parent_head = self[parent].lists[list_idx].children.0;
        let parent_tail = self[parent].lists[list_idx].children.1;

        let prev = if parent_head == child {
            ThingId::null()
        } else {
            let mut prev = ThingId::null();
            let mut current = parent_head;
            while !current.is_null() {
                let next = self[current].lists[list_idx].sibling;
                if next == child {
                    prev = current;
                    break;
                }
                current = next;
            }
            assert!(!prev.is_null());
            prev
        };

        if prev.is_null() {
            self[parent].lists[list_idx].children.0 = child_next;
        } else {
            self[prev].lists[list_idx].sibling = child_next;
        }

        if parent_tail == child {
            self[parent].lists[list_idx].children.1 = prev;
        }

        {
            let child_list = &mut self[child].lists[list_idx];
            child_list.parent = ThingId::null();
            child_list.sibling = ThingId::null();
        }

        {
            let parent_list = &mut self[parent].lists[list_idx];
            parent_list.length -= 1;
            if parent_list.length == 0 {
                parent_list.children = (ThingId::null(), ThingId::null());
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
}

impl Iterator for ListChildrenIter<'_> {
    type Item = ThingId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next.is_null() {
            return None;
        }

        let current = self.next;
        self.next = self.things.entries[current.slot as usize].lists[self.list as usize].sibling;
        Some(current)
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
    pub(crate) fn write_pass(
        &mut self,
        mut filter: impl FnMut(&Things, &Thing) -> bool,
        mut body: impl FnMut(&Things, &mut Thing, &mut Commands),
    ) {
        let mut write_buffer = std::mem::take(&mut self.write_buffer);
        let mut commands = Commands::new();

        for (thing, target) in self.entries.iter().zip(write_buffer.iter_mut()) {
            if thing.id.is_valid() && filter(self, thing) {
                *target = *thing;
                body(self, target, &mut commands);
            }
        }

        self.write_buffer = write_buffer;
        std::mem::swap(&mut self.entries, &mut self.write_buffer);
        self.appy_commands(commands);
    }

    pub(crate) fn readonly_pass(&self, mut body: impl FnMut(&Things, &Thing)) {
        for thing in &self.entries {
            if thing.id.is_valid() {
                body(self, thing);
            }
        }
    }

    fn appy_commands(&mut self, commands: Commands) {
        for ListMutation {
            list,
            parent,
            child,
        } in commands.list_mutations
        {
            self.add_to_list(list, parent, child);
        }

        for id in commands.despawns {
            self.despawn(id);
        }

        for spawn in commands.spawns {
            let (link, parent, mode) = spawn.to_link;
            match mode {
                LinkCollisionMode::DoNotCreate => {
                    // Do not spawn if we are in "do not create" mode
                    if self[parent].link(link) != ThingId::null() {
                        return;
                    }
                }
                LinkCollisionMode::Replace => {}
            };

            let thing = self.spawn();
            let id = thing.id;
            *thing = spawn.thing;
            thing.id = id;

            // Save important state on the side
            let (list, parent) = spawn.to_list;
            if !parent.is_null() {
                self.add_to_list(list, parent, id);
            }

            // Link to target
            let (link, parent, _) = spawn.to_link;
            if !parent.is_null() {
                self[parent].set_link(link, id);
            }
        }
    }
}

pub(crate) struct Commands {
    list_mutations: Vec<ListMutation>,
    despawns: Vec<ThingId>,
    spawns: Vec<Spawn>,
}

impl Commands {
    fn new() -> Self {
        Self {
            list_mutations: vec![],
            despawns: vec![],
            spawns: vec![],
        }
    }

    pub fn add_to_list(&mut self, list: List, parent: ThingId, child: ThingId) {
        self.list_mutations.push(ListMutation {
            list,
            parent,
            child,
        });
    }

    pub fn remove_from_list(&mut self, list: List, child: ThingId) {
        self.list_mutations.push(ListMutation {
            list,
            parent: ThingId::null(),
            child,
        });
    }

    pub fn despawn(&mut self, id: ThingId) {
        self.despawns.push(id);
    }

    pub fn spawn_and_append_to_list(&mut self, list: List, parent: ThingId) -> &mut Thing {
        self.spawns.push(Spawn {
            to_list: (list, parent),
            ..Default::default()
        });
        &mut self.spawns.last_mut().unwrap().thing
    }

    pub fn spawn_and_set_link(
        &mut self,
        link: Link,
        parent: ThingId,
        mode: LinkCollisionMode,
    ) -> &mut Thing {
        self.spawns.push(Spawn {
            to_link: (link, parent, mode),
            ..Default::default()
        });
        &mut self.spawns.last_mut().unwrap().thing
    }
}

#[derive(Clone, Copy)]
struct ListMutation {
    list: List,
    parent: ThingId,
    child: ThingId,
}

#[derive(Default)]
struct Spawn {
    thing: Thing,
    to_list: (List, ThingId),
    to_link: (Link, ThingId, LinkCollisionMode),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum LinkCollisionMode {
    Replace,
    DoNotCreate,
}

impl Default for LinkCollisionMode {
    fn default() -> Self {
        LinkCollisionMode::Replace
    }
}
