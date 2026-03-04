use crate::{
    csr::{Csr, CsrBuilder},
    things::ThingId,
};

pub(crate) struct NavCacheBuilder {
    graph: CsrBuilder<ThingId, ThingId>,
    cache_size: usize,
}

impl NavCacheBuilder {
    pub fn new(cache_size: usize) -> Self {
        Self {
            graph: CsrBuilder::new(),
            cache_size,
        }
    }

    pub fn add_connection(&mut self, a: ThingId, b: ThingId) {
        self.graph.push(a, b);
        self.graph.push(b, a);
    }

    pub fn build(self) -> NavCache {
        NavCache {
            graph: self.graph.build(),
            cache: Vec::with_capacity(self.cache_size),
            counters: NavCacheCounters::default(),
        }
    }
}

#[derive(Default, Clone, Copy)]
struct NavCacheEntry {
    source: ThingId,
    destination: ThingId,
    next_step: ThingId,
}

#[derive(Clone)]
pub struct NavCache {
    graph: Csr<ThingId, ThingId>,
    cache: Vec<NavCacheEntry>,
    counters: NavCacheCounters,
}

#[derive(Clone, Default)]
struct NavCacheCounters {
    num_hits: u64,
    num_miss: u64,
    num_reset: u64,
}

impl NavCache {
    /// Returns the next hop from `source` toward `destination`.
    ///
    /// The cache stores `(source, destination) -> next_step` entries so repeated
    /// queries can skip pathfinding. On a cache miss, this runs A* over the CSR
    /// graph, caches each hop along the discovered path, and returns the first
    /// step after `source`.
    ///
    /// If no path is found (or `source == destination`), this returns a null
    /// `ThingId`.
    pub fn pathfind(
        &mut self,
        source: ThingId,
        destination: ThingId,
        cost_fn: &impl Fn(ThingId, ThingId) -> i32,
    ) -> ThingId {
        // Find an existing step, if one exists
        let entry = self
            .cache
            .iter()
            .find(|entry| entry.source == source && entry.destination == destination);

        // Return the next step if found
        match entry {
            Some(entry) => {
                self.counters.num_hits = self.counters.num_hits.saturating_add(1);
                return entry.next_step;
            }
            None => {}
        };

        // No step found, run pathfinding
        let pathfind_result = pathfinding::directed::astar::astar(
            &source,
            |&node| {
                self.graph
                    .get_slice(node)
                    .into_iter()
                    .map(move |&x| (x, cost_fn(node, x)))
            },
            |&node| cost_fn(node, destination),
            |&node| node == destination,
        );
        self.counters.num_miss = self.counters.num_miss.saturating_add(1);
        let path = pathfind_result
            .as_ref()
            .map(|x| x.0.as_slice())
            .unwrap_or_default();

        // Blow the cache if out of room
        if self.cache.len() + path.len() >= self.cache.capacity() {
            self.counters.num_reset = self.counters.num_reset.saturating_add(1);
            self.cache.clear();
        }

        // Cache the new steps
        self.cache
            .extend(path.windows(2).map(|steps| NavCacheEntry {
                source: steps[0],
                destination,
                next_step: steps[1],
            }));

        // And return the next one, if any
        path.get(1).copied().unwrap_or_default()
    }
}
