use std::collections::HashMap;

use string_interner::{DefaultStringInterner, DefaultSymbol};
use util::{
    span::Span,
    string_pool::{SpanHandle, StringPool},
};

type Symbol = DefaultSymbol;

#[derive(Default)]
pub struct Objects {
    interner: DefaultStringInterner,
    text: StringPool,
    instances: Vec<Object>,
    properties: Properties,
    // Contiguous list of list items
    list_items: Vec<ObjectId>,
    // String keyed lookup of object id
    tags: HashMap<Symbol, ObjectId>,
}

impl Objects {
    pub fn clear(&mut self) {
        self.text.clear();
        self.interner = DefaultStringInterner::default();
        self.instances.clear();
        self.properties.clear();
        self.list_items.clear();
        self.tags.clear();
    }

    pub fn lookup_by_tag(&self, key: &str) -> Option<ObjectId> {
        let symbol = self.interner.get(key)?;
        self.tags.get(&symbol).copied()
    }

    pub fn lookup_ref_by_tag<'a>(&'a self, key: &str) -> Option<ObjectRef<'a>> {
        let id = self.lookup_by_tag(key)?;
        Some(self.get_ref(id))
    }

    fn get_property(&self, id: ObjectId, key: &str) -> Option<Property> {
        // Find span in property array(s)
        let property_span = self
            .instances
            .get(id.0)
            .map(|x| x.properties)
            .unwrap_or_default();

        // Resolve the key into a symbol
        let symbol = match self.interner.get(key) {
            Some(x) => x,
            None => return Default::default(),
        };

        self.properties.lookup(property_span, symbol)
    }

    pub fn try_handle(&self, id: ObjectId, key: &str) -> Option<u64> {
        self.get_property(id, key).and_then(|prop| match prop {
            Property::Handle(handle) => Some(handle),
            _ => None,
        })
    }

    pub fn try_str<'a>(&'a self, id: ObjectId, key: &str) -> Option<&'a str> {
        self.get_property(id, key).and_then(|prop| match prop {
            Property::String(span) => Some(self.text.get(span)),
            _ => None,
        })
    }

    const UNDEFINED: &'static str = "UNDEFINED";

    pub fn str<'a>(&'a self, id: ObjectId, key: &str) -> &'a str {
        self.try_str(id, key).unwrap_or(Self::UNDEFINED)
    }

    pub fn try_child<'a>(&'a self, id: ObjectId, key: &str) -> Option<ObjectId> {
        self.get_property(id, key).and_then(|prop| match prop {
            Property::Object(id) => Some(id),
            _ => None,
        })
    }

    pub fn try_list<'a>(&'a self, id: ObjectId, key: &str) -> Option<&'a [ObjectId]> {
        self.get_property(id, key).and_then(|prop| match prop {
            Property::List(span) => Some(span.view(&self.list_items)),
            _ => None,
        })
    }

    pub fn list<'a>(&'a self, id: ObjectId, key: &str) -> &'a [ObjectId] {
        self.try_list(id, key).unwrap_or_default()
    }

    pub fn get_ref<'a>(&'a self, id: ObjectId) -> ObjectRef<'a> {
        ObjectRef { objects: self, id }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ObjectId(pub usize);

#[derive(Default, Clone, Copy)]
struct Object {
    properties: Span,
}

#[derive(Clone, Copy)]
pub struct ObjectRef<'a> {
    objects: &'a Objects,
    id: ObjectId,
}

impl<'a> ObjectRef<'a> {
    #[inline]
    pub fn str(self, key: &str) -> &'a str {
        self.objects.str(self.id, key)
    }
    #[inline]
    pub fn try_list(self, key: &str) -> Option<&'a [ObjectId]> {
        self.objects.try_list(self.id, key)
    }
    #[inline]
    pub fn list(self, key: &str) -> &'a [ObjectId] {
        self.objects.list(self.id, key)
    }
}

#[derive(Clone, Copy)]
enum Property {
    Handle(u64),
    String(SpanHandle),
    Object(ObjectId),
    List(Span),
}

#[derive(Default)]
struct Properties {
    // SOA of properties: keys and values split (for faster search?)
    keys: Vec<Symbol>,
    values: Vec<Property>,
}

impl Properties {
    fn clear(&mut self) {
        self.keys.clear();
        self.values.clear();
    }

    fn get_pos(&self) -> usize {
        self.keys.len()
    }

    fn push(&mut self, key: Symbol, value: Property) {
        self.keys.push(key);
        self.values.push(value);
    }

    fn lookup(&self, span: Span, key: Symbol) -> Option<Property> {
        let prop_idx = span.index_where(&self.keys, |x| *x == key)?;
        self.values.get(prop_idx).copied()
    }
}

#[derive(Default)]
pub struct ObjectsBuilder {
    data: Objects,
    /// A stack of object ids, mirroring the order of spawn calls.
    /// active_stack.last() is the currently-constructed object
    active_stack: Vec<ObjectId>,
    /// A list of object_id items that are waiting for construction.
    /// EVery time an object is spawned within a list, its added here.
    /// When the list closes, the objects are popped of this list and copied
    /// into the data list.
    list_items: Vec<ObjectId>,
    /// The stack of begin-positions for lists. Every time a list() begins, the current position in
    /// list_items is pushed in. When the list is closed, we pop off the index from here, and take all
    /// items until the end as the items belonging to the newly created list.
    list_stack: Vec<usize>,
}

impl ObjectsBuilder {
    pub fn build(self) -> Objects {
        self.data
    }

    pub fn spawn(&mut self, build: impl FnOnce(&mut ObjectsBuilder) -> ()) -> ObjectId {
        let props_base = self.data.properties.get_pos();

        let object_id = ObjectId(self.data.instances.len());
        self.data.instances.push(Object::default());
        self.active_stack.push(object_id);
        build(self);

        let props_end = self.data.properties.get_pos();
        self.data.instances[object_id.0].properties = Span::between(props_base, props_end);

        // If there is an open list, add yourself to it.
        if !self.list_stack.is_empty() {
            self.list_items.push(object_id);
        }

        object_id
    }

    pub fn tag(&mut self, tag: &str) {
        let id = match self.active_stack.last().copied() {
            Some(id) => id,
            None => {
                return;
            }
        };
        let symbol = self.data.interner.get_or_intern(tag);
        self.data.tags.insert(symbol, id);
    }

    fn set_property(&mut self, key: &'static str, value: Property) {
        let symbol = self.data.interner.get_or_intern_static(key);
        self.data.properties.push(symbol, value);
    }

    pub fn handle(&mut self, key: &'static str, handle: u64) {
        self.set_property(key, Property::Handle(handle));
    }

    pub fn str(&mut self, key: &'static str, value: impl AsRef<str>) {
        let text_span = self.data.text.push_str(value.as_ref());
        self.set_property(key, Property::String(text_span));
    }

    pub fn fmt(&mut self, key: &'static str, args: std::fmt::Arguments) {
        let text_span = self.data.text.push_fmt(args);
        self.set_property(key, Property::String(text_span));
    }

    pub fn display(&mut self, key: &'static str, value: impl std::fmt::Display) {
        self.fmt(key, format_args!("{}", value));
    }

    pub fn child(&mut self, key: &'static str, id: ObjectId) {
        self.set_property(key, Property::Object(id));
    }

    pub fn list(&mut self, key: &'static str, build: impl FnOnce(&mut ObjectsBuilder) -> ()) {
        self.list_stack.push(self.list_items.len());
        build(self);
        let span = {
            let start = self.list_stack.pop().unwrap();
            let drain = self.list_items.drain(start..);
            Span::of_extension(&mut self.data.list_items, drain)
        };
        self.set_property(key, Property::List(span));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_properties_and_fmt() {
        let mut builder = ObjectsBuilder::default();

        let id = builder.spawn(|ctx| {
            ctx.str("name", "Geralt");
            ctx.str("class", "Witcher");
            ctx.fmt("level", format_args!("{}", 35));
        });

        let objects = builder.build();

        assert_eq!(objects.str(id, "name"), "Geralt");
        assert_eq!(objects.str(id, "class"), "Witcher");
        assert_eq!(objects.str(id, "level"), "35");

        // Test undefined
        assert_eq!(objects.str(id, "mana"), Objects::UNDEFINED);
        assert!(objects.try_str(id, "mana").is_none());
    }

    #[test]
    fn test_nested_objects() {
        let mut builder = ObjectsBuilder::default();

        let sword_id = builder.spawn(|ctx| {
            ctx.str("name", "Silver Sword");
            ctx.fmt("damage", format_args!("{}", 100));
        });

        let player_id = builder.spawn(|ctx| {
            ctx.str("name", "Player");
            ctx.child("weapon", sword_id);
        });

        let objects = builder.build();

        // 1. Check parent has child
        let weapon_ref = objects.try_child(player_id, "weapon");
        assert!(weapon_ref.is_some());
        let weapon_ref = weapon_ref.unwrap();

        assert_eq!(weapon_ref, sword_id);

        // 2. Resolve data through the child ID
        assert_eq!(objects.str(weapon_ref, "name"), "Silver Sword");
        assert_eq!(objects.str(weapon_ref, "damage"), "100");
    }

    #[test]
    fn test_lists_and_recursion() {
        let mut builder = ObjectsBuilder::default();

        let party_id = builder.spawn(|ctx| {
            ctx.str("type", "Party");

            // Nested list building
            ctx.list("members", |ctx| {
                // Member 1
                ctx.spawn(|c| {
                    c.str("name", "Member A");
                });
                // Member 2
                ctx.spawn(|c| {
                    c.str("name", "Member B");
                });
                // Member 3
                ctx.spawn(|c| {
                    c.str("name", "Member C");
                });
            });
        });

        let objects = builder.build();

        let members = objects.list(party_id, "members");
        assert_eq!(members.len(), 3);

        assert_eq!(objects.str(members[0], "name"), "Member A");
        assert_eq!(objects.str(members[1], "name"), "Member B");
        assert_eq!(objects.str(members[2], "name"), "Member C");
    }

    #[test]
    fn test_soa_alignment() {
        // This test ensures that when we write properties, they align correctly
        // in the SoA (Structure of Arrays) layout.
        let mut builder = ObjectsBuilder::default();

        let id = builder.spawn(|ctx| {
            // Write keys out of alphabetical order to ensure
            // the implementation doesn't rely on sorting (unless intended)
            // and that values map to the correct keys.
            ctx.str("z_key", "Z_VAL");
            ctx.str("a_key", "A_VAL");
            ctx.str("m_key", "M_VAL");
        });

        let objects = builder.build();

        assert_eq!(objects.str(id, "z_key"), "Z_VAL");
        assert_eq!(objects.str(id, "a_key"), "A_VAL");
        assert_eq!(objects.str(id, "m_key"), "M_VAL");
    }

    #[test]
    fn test_empty_object() {
        let mut builder = ObjectsBuilder::default();
        let id = builder.spawn(|_| {}); // Do nothing
        let objects = builder.build();

        assert_eq!(objects.str(id, "anything"), Objects::UNDEFINED);
        assert!(objects.list(id, "anything").is_empty());
    }

    #[test]
    fn test_deep_mixed_structure() {
        let mut builder = ObjectsBuilder::default();

        let root = builder.spawn(|ctx| {
            ctx.str("id", "root");
            ctx.list("folders", |ctx| {
                ctx.spawn(|c| {
                    c.str("name", "bin");
                    c.list("files", |c| {
                        c.spawn(|f| f.str("name", "exe"));
                    });
                });
                ctx.spawn(|c| {
                    c.str("name", "etc");
                    c.list("files", |_| {}); // Empty list
                });
            });
        });

        let objs = builder.build();

        let folders = objs.list(root, "folders");
        assert_eq!(folders.len(), 2);

        let bin_folder = folders[0];
        assert_eq!(objs.str(bin_folder, "name"), "bin");

        let bin_files = objs.list(bin_folder, "files");
        assert_eq!(bin_files.len(), 1);
        assert_eq!(objs.str(bin_files[0], "name"), "exe");

        let etc_folder = folders[1];
        assert_eq!(objs.str(etc_folder, "name"), "etc");
        assert!(objs.list(etc_folder, "files").is_empty());
    }
}
