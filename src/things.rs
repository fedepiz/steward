use strum::{EnumCount, EnumIter};

use util::bitset::BitSet;

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
}

#[derive(Clone, Copy)]
struct StringBuf([u8; 128]);

impl Default for StringBuf {
    fn default() -> Self {
        Self([0; 128])
    }
}

impl StringBuf {
    fn as_str(&self) -> &str {
        str::from_utf8(&self.0).unwrap()
    }

    fn set(&mut self, source: &str) {
        self.0.fill(0);

        let mut copy_len = source.len().min(self.0.len());
        while !source.is_char_boundary(copy_len) {
            copy_len -= 1;
        }

        self.0[..copy_len].copy_from_slice(&source.as_bytes()[..copy_len]);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Flag {
    IsPerson,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Var {
    Dummy,
    PosX,
    PosY,
    Size,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum Link {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, EnumIter, EnumCount)]
pub(crate) enum List {
    AtLocation,
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
const NUM_LINKS: usize = Link::COUNT;
const NUM_LISTS: usize = List::COUNT;

#[derive(Clone, Copy, Default)]
pub(crate) struct Thing {
    id: ThingId,
    next_free: ThingId,
    name_buf: StringBuf,
    flags: BitSet<NUM_FLAGS>,
    vars: [f32; NUM_VARS],
    links: [ThingId; NUM_LINKS],
    lists: [ListThingData; NUM_LISTS],
}

impl Thing {
    #[inline]
    pub(crate) fn id(&self) -> ThingId {
        self.id
    }

    #[inline]
    pub(crate) fn name(&self) -> &str {
        self.name_buf.as_str()
    }

    #[inline]
    pub(crate) fn set_name(&mut self, name: &str) {
        self.name_buf.set(name);
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
    pub(crate) fn set_link(&mut self, link: Link, value: ThingId) {
        self.links[link as usize] = value;
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
    meta: MetaData,
}

impl Things {
    pub(crate) fn init(&mut self) {
        self.entries.resize(NUM_THINGS, Thing::default());
        self.write_buffer = self.entries.clone();
        // Create a 'chain' of next_free pointing at each other, with the last pointing to "the null"
        for (idx, thing) in self.entries.iter_mut().enumerate() {
            let slot = if idx != LAST_IDX_THING { idx + 1 } else { 0 };
            // Set up my index
            thing.id.slot = idx as u32;
            // Set up the next index
            thing.next_free = ThingId {
                slot: slot as u32,
                generation: 0,
            }
        }
        // First element in the free list has index 1
        self.meta.free_list_head = ThingId {
            slot: 1,
            generation: 0,
        };
        // Last has index last index
        self.meta.free_list_tail = ThingId {
            slot: LAST_IDX_THING as u32,
            generation: 0,
        };
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
        thing
    }

    pub(crate) fn despawn(&mut self, id: ThingId) {
        if !id.is_valid() {
            return;
        }
        let thing = &mut self.entries[id.slot as usize];
        assert!(thing.id == id);
        thing.id.generation += 1;
        assert!(thing.id.generation % 2 == 0);
        let end = &mut self.entries[self.meta.free_list_tail.slot as usize];
        end.next_free = id;
        self.meta.free_list_tail = id;
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

    pub(crate) fn pass(&mut self, mut body: impl FnMut(&Things, &mut Thing)) {
        let mut write_buffer = std::mem::take(&mut self.write_buffer);

        for (thing, target) in self.entries.iter().zip(write_buffer.iter_mut()) {
            if thing.id.is_valid() {
                *target = *thing;
                body(self, target);
            }
        }

        self.write_buffer = write_buffer;
        std::mem::swap(&mut self.entries, &mut self.write_buffer);
    }

    pub(crate) fn pass_readonly(&self, mut body: impl FnMut(&Things, &Thing)) {
        for thing in &self.entries {
            if thing.id.is_valid() {
                body(self, thing);
            }
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

struct ListChildrenIter<'a> {
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
