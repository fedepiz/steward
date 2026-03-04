use std::marker::PhantomData;

use crate::things::{self, ThingId};

pub(crate) trait Slot {
    fn slot(&self) -> usize;
}

impl Slot for ThingId {
    fn slot(&self) -> usize {
        ThingId::slot(*self)
    }
}

pub(crate) struct CsrBuilder<K, V> {
    bins: Vec<usize>,
    entries: Vec<(K, V)>,
    max_slot: usize,
}

impl<K: Slot, V: Default + Clone> CsrBuilder<K, V> {
    /// Creates an empty CSR builder with fixed bin capacity for all thing slots.
    pub fn new() -> Self {
        Self {
            bins: vec![0; things::NUM_THINGS],
            entries: vec![],
            max_slot: 0,
        }
    }

    /// Appends one adjacency value under `key`.
    ///
    /// Internally this increments the per-slot bin count and records the entry
    /// for compaction during `build`.
    pub fn push(&mut self, key: K, value: V) {
        let slot = key.slot();
        self.bins[slot] += 1;
        self.entries.push((key, value));
        self.max_slot = slot.max(self.max_slot);
    }

    /// Compacts pushed entries into CSR layout.
    ///
    /// Produces prefix offsets per key slot and a contiguous value array where
    /// neighbors for a slot are stored in `values[offsets[i]..offsets[i + 1]]`.
    pub fn build(self) -> Csr<K, V> {
        let mut offsets = vec![0; self.max_slot + 2];
        // Prefix sum to calculate offsets
        for i in 0..=self.max_slot {
            offsets[i + 1] = offsets[i] + self.bins[i];
        }
        let mut counts = self.bins;
        counts.clear();
        counts.resize(self.max_slot + 1, 0);

        let mut values = vec![V::default(); self.entries.len()];

        for (key, value) in self.entries {
            let idx = offsets[key.slot()] + counts[key.slot()];
            counts[key.slot()] += 1;
            values[idx] = value
        }

        Csr {
            key_typ: PhantomData,
            offsets,
            values,
        }
    }
}

#[derive(Default, Clone)]
pub(crate) struct Csr<K, V> {
    key_typ: PhantomData<K>,
    offsets: Vec<usize>,
    values: Vec<V>,
}

impl<K: Slot, V> Csr<K, V> {
    /// Returns the adjacency slice for `id`.
    ///
    /// The returned slice is borrowed from internal CSR storage and is empty
    /// when the slot is out of range or has no entries.
    pub fn get_slice(&self, id: K) -> &[V] {
        let idx = id.slot();
        if idx + 1 >= self.offsets.len() {
            return &[];
        }
        let start = self.offsets[idx];
        let end = self.offsets[idx + 1];
        &self.values[start..end]
    }
}
