use crate::{geom::V2, parties::PartyId};
use fast_voxel_traversal::raycast_2d as fvt;
use slotmap::SecondaryMap;

use util::bucket_sort::BucketSort;

#[derive(Clone, Copy)]
pub(crate) struct Element {
    pub id: PartyId,
    pub speed: f32,
    pub pos: V2,
    pub destination: V2,
}

pub(crate) struct Output {
    pub positions: Vec<(PartyId, V2)>,
    pub spatial_map: SpatialMap,
}

#[derive(Default)]
pub(crate) struct MovementCache {
    paths: SecondaryMap<PartyId, MovementPath>,
}

pub(crate) trait MovementGraph {
    fn size(&self) -> (usize, usize);

    fn get_speed_at(&self, x: i64, y: i64) -> f32;
}

#[derive(Default)]
struct MovementPath {
    /// Destination of this path
    destination: V2,
    /// Position the entity is moving towards.
    /// When steps_reverse.len() == 0, this should be identical to pos,
    /// else, it should be identical to path.last().
    /// (Ie, it's a trick to avoid a cache miss to find the next destination)
    next_step: V2,
    /// All the following steps, stored in reverse order, popped off for efficient removal
    steps_reversed: Vec<V2>,
}

pub(crate) fn tick_movement<G: MovementGraph>(
    cache: &mut MovementCache,
    elements: &[Element],
    graph: &G,
) -> Output {
    const SPEED_MULT: f32 = 0.03;
    const BUCKET_SIZE: usize = 8;

    let _span = tracing::info_span!("Movement").entered();
    let next_positions: Vec<_> = elements
        .iter()
        .map(|element| {
            // Derive the next step
            let next_step = {
                // Temporary on-stack location for a fresh path
                let mut new_path = None;

                // Get the path to the destination. Uses a cache path if valid, else pathfinds.
                let path = cache
                    .paths
                    .get_mut(element.id)
                    .filter(|path| path.destination == element.destination)
                    .unwrap_or_else(|| {
                        new_path =
                            Some(calculate_new_path(element.pos, element.destination, graph));
                        new_path.as_mut().unwrap()
                    });

                // Advance path (the path is not over and the next step is the current position)
                while element.pos == path.next_step && path.next_step != path.destination {
                    path.steps_reversed.pop();
                    path.next_step = path
                        .steps_reversed
                        .last()
                        .copied()
                        .unwrap_or(path.destination);
                }

                let next_step = path.next_step;
                // If we calculated a new path, cache it
                if let Some(new_path) = new_path {
                    cache.paths.insert(element.id, new_path);
                }

                next_step
            };

            // Calculate next position
            let current_tile = pos_to_tile(element.pos);
            // Define a minimum speed, to make sure nothing gets stuck even in the 'unlikely event' entities try to
            // traverse untraversable spaces.
            const MINIMUM_SPEED: f32 = 0.1;
            let terrain_mult = graph
                .get_speed_at(current_tile.0, current_tile.1)
                .max(MINIMUM_SPEED);
            let speed = element.speed * terrain_mult * SPEED_MULT;
            let next_pos = interpolate_position(element.pos, next_step, speed);
            (element.id, next_pos)
        })
        .collect();

    let spatial_map = {
        let _span = tracing::info_span!("Spatial Map").entered();
        let (w, h) = graph.size();
        let map = SpatialMap::new(&next_positions, w, h, BUCKET_SIZE);
        map
    };

    assert!(elements.len() == next_positions.len());

    Output {
        positions: next_positions,
        spatial_map,
    }
}

fn interpolate_position(pos: V2, dest: V2, speed: f32) -> V2 {
    if pos == dest || speed == 0.0 {
        return pos;
    }

    // Calculate distance
    let distance = V2::distance(pos, dest);
    // Let normalize speed by distance and clamp between 0 and 1)
    let speed_t = (speed / distance).clamp(0., 1.);
    // New position is interpolation
    V2::lerp(pos, dest, speed_t)
}

fn simplify_path<G: MovementGraph>(path: &mut Vec<V2>, graph: &G) {
    // Simplifies the path. The algorithm works entirely in-place. Removing nodes is done via
    // a final deduplication step
    if path.len() < 2 {}

    let mut start = 0;
    while start < path.len() {
        let end = find_simplifiable_series(&path, start, graph);
        for i in start + 1..end {
            path[i] = path[start];
        }
        if start == end {
            start = start + 1;
        } else {
            start = end;
        }
    }

    path.dedup();
}

fn find_simplifiable_series<G: MovementGraph>(nodes: &[V2], start: usize, graph: &G) -> usize {
    let mut end = start;
    let start_p = nodes[start];
    while end + 1 < nodes.len() {
        end += 1;
        let end_p = nodes[end];
        if !can_simplify_pair(start_p, end_p, graph) {
            break;
        }
    }
    end
}

fn can_simplify_pair<G: MovementGraph>(p1: V2, p2: V2, graph: &G) -> bool {
    let (w, h) = graph.size();
    let volume = fvt::BoundingVolume2 {
        size: (w as i32, h as i32),
    };
    // let half_unit = V2::splat(0.5);
    // From center-based positions to corner-based positions
    // let p1 = p1;
    // let p2 = p2;
    let ray = fvt::Ray2 {
        origin: p1.as_pair(),
        direction: (p2 - p1).normalize().as_pair(),
        length: V2::distance(p2, p1),
    };

    let base_speed = {
        let (x, y) = pos_to_tile(p1);
        graph.get_speed_at(x, y)
    };

    let hits = fvt::VoxelRay2Iterator::new(volume, ray);
    hits.into_iter().all(|hit| {
        let (x, y) = hit.voxel;
        // Look around at a neighbour of size 1...
        let min_x = (x - 1).max(0);
        let max_x = (x + 2).min(w as i32);
        let min_y = (y - 1).max(0);
        let max_y = (y + 2).min(h as i32);

        (min_y..max_y)
            .flat_map(|y| (min_x..max_x).map(move |x| (x, y)))
            .all(|(x, y)| {
                let speed = graph.get_speed_at(x as i64, y as i64);
                speed >= base_speed
            })
    })
}

fn calculate_new_path<G: MovementGraph>(source: V2, destination: V2, graph: &G) -> MovementPath {
    if source == destination {
        // TODO: Actual pathfinding, once we have a terrain grid
        return MovementPath {
            destination,
            next_step: destination,
            steps_reversed: vec![destination],
        };
    }

    let result = {
        let (width, height) = graph.size();
        let source = pos_to_tile(source);
        let destination = pos_to_tile(destination);
        pathfinding::directed::astar::astar(
            &source,
            |&current| {
                let (x, y) = current;
                let min_x = (x - 1).max(0);
                let max_x = (x + 2).min(width as i64);

                let min_y = (y - 1).max(0);
                let max_y = (y + 2).min(height as i64);

                (min_y..max_y)
                    .flat_map(move |j| (min_x..max_x).map(move |i| (i, j)))
                    .filter(move |&neighbour| {
                        let speed = graph.get_speed_at(neighbour.0, neighbour.1);
                        current != neighbour && speed > 0.
                    })
                    .map(move |neighbor| {
                        let cost_current = speed_to_cost(graph.get_speed_at(current.0, current.1));
                        let cost_neighbour =
                            speed_to_cost(graph.get_speed_at(neighbor.0, neighbor.1));
                        let unit_cost_scale = (cost_current + cost_neighbour) / 2.;
                        let c = distance_to_cost(distance(current, neighbor) * unit_cost_scale);
                        (neighbor, c)
                    })
            },
            |&node| distance_to_cost(distance(node, destination)),
            |&node| node == destination,
        )
    };

    let mut path: Vec<_> = result
        .as_ref()
        .map(|(x, _)| x.as_slice())
        .unwrap_or_default()
        .into_iter()
        // First step is just the source tile, so we don't need it
        .skip(1)
        .map(|&(x, y)| V2::new(x as f32, y as f32))
        .chain(std::iter::once(destination))
        .collect();

    // IF the destination and the last point are co-tiled, collapse them.
    if path.len() >= 2 {
        let idx = path.len() - 2;
        let next_to_last = path[idx];
        if next_to_last.round() == destination.round() {
            path[idx] = destination;
            path.pop();
        }
    }

    simplify_path(&mut path, graph);

    path.reverse();
    let next_step = path.last().copied().unwrap_or(source);

    MovementPath {
        destination,
        next_step,
        steps_reversed: path,
    }
}

fn distance_to_cost(x: f32) -> i64 {
    (x * 100.) as i64
}

fn speed_to_cost(speed: f32) -> f32 {
    if speed == 0.0 { 100000. } else { 1. / speed }
}

fn distance((ax, ay): (i64, i64), (bx, by): (i64, i64)) -> f32 {
    (((ax - bx).pow(2) + (ay - by).pow(2)) as f32).sqrt()
}

fn pos_to_tile(pos: V2) -> (i64, i64) {
    (pos.x.round() as i64, pos.y.round() as i64)
}

pub(crate) struct SpatialMap {
    sort: BucketSort<SpatialMapEntry>,
    // Witdth of map (in world units)
    width: usize,
    // Height of map (in world units)
    height: usize,
    // Width and height of each bucket, in world units
    bucket_size: usize,
}

impl SpatialMap {
    fn calculate_bucket_counts(width: usize, height: usize, bucket_size: usize) -> (usize, usize) {
        let num_buckets_x = width / bucket_size + (width % bucket_size).min(1);
        let num_buckets_y = height / bucket_size + (height % bucket_size).min(1);
        (num_buckets_x, num_buckets_y)
    }

    fn new(entities: &[(PartyId, V2)], width: usize, height: usize, bucket_size: usize) -> Self {
        let (num_buckets_x, num_buckets_y) =
            Self::calculate_bucket_counts(width, height, bucket_size);
        let num_buckets = { num_buckets_x * num_buckets_y };

        let bucketed_entities = entities
            .iter()
            .copied()
            .map(|(id, pos)| {
                let bucket_x = pos.x as usize / bucket_size;
                let bucket_y = pos.y as usize / bucket_size;
                let bucket_idx = bucket_x + bucket_y * num_buckets_x;
                let entry = SpatialMapEntry { id, pos };
                (entry, bucket_idx)
            })
            .collect();

        let sort = BucketSort::new(bucketed_entities, num_buckets);

        Self {
            sort,
            width,
            height,
            bucket_size,
        }
    }

    pub fn search(&self, pos: V2, range: f32) -> impl Iterator<Item = PartyId> {
        // Determine the range of buckets to check
        let bucket_range = (range / self.bucket_size as f32).ceil() as isize;

        let center_bucket_x = (pos.x as usize / self.bucket_size) as isize;
        let center_bucket_y = (pos.y as usize / self.bucket_size) as isize;

        let (num_buckets_x, num_buckets_y) =
            Self::calculate_bucket_counts(self.width, self.height, self.bucket_size);

        let min_bx = (center_bucket_x - bucket_range).max(0);
        let max_bx = (center_bucket_x + bucket_range).min(num_buckets_x as isize - 1);

        let min_by = (center_bucket_y - bucket_range).max(0);
        let max_by = (center_bucket_y + bucket_range).min(num_buckets_y as isize - 1);

        // Iterate over the buckets in the range
        (min_by..=max_by).flat_map(move |by| {
            (min_bx..=max_bx).flat_map(move |bx| {
                let bucket_idx = (bx + by * num_buckets_x as isize) as usize;

                // For each bucket, iterate over the (entity ID, pos) tuples and map them to SpatialItems
                self.sort.get(bucket_idx).iter().map(|entry| entry.id)
            })
        })
    }
}

#[derive(Default, PartialEq, Debug, Clone, Copy)]
pub(crate) struct SpatialMapEntry {
    pub id: PartyId,
    pub pos: V2,
}

#[cfg(test)]
mod spatial_map_tests {
    use super::*;
    use slotmap::SlotMap;

    fn setup_entities() -> Vec<(PartyId, V2)> {
        let mut sm = SlotMap::with_key();
        vec![
            (sm.insert(()), V2::new(10.0, 10.0)), // bucket (1,1)
            (sm.insert(()), V2::new(15.0, 15.0)), // bucket (1,1)
            (sm.insert(()), V2::new(35.0, 35.0)), // bucket (3,3)
            (sm.insert(()), V2::new(90.0, 90.0)), // bucket (9,9)
        ]
    }

    #[test]
    fn new_spatial_map() {
        let entities = setup_entities();
        let spatial_map = SpatialMap::new(&entities, 100, 100, 10);
        // 10*10 = 100 buckets
        assert_eq!(spatial_map.sort.num_buckets(), 100);
    }

    #[test]
    fn search_empty() {
        let entities = vec![];
        let spatial_map = SpatialMap::new(&entities, 100, 100, 10);
        let results: Vec<_> = spatial_map.search(V2::new(50.0, 50.0), 20.0).collect();
        assert!(results.is_empty());
    }

    #[test]
    fn search_no_results() {
        let entities = setup_entities();
        let spatial_map = SpatialMap::new(&entities, 100, 100, 10);
        let results: Vec<_> = spatial_map.search(V2::new(50.0, 50.0), 5.0).collect();
        assert!(results.is_empty());
    }

    #[test]
    fn search_simple_hit() {
        let entities = setup_entities();
        let spatial_map = SpatialMap::new(&entities, 100, 100, 10);

        // Search position is (12,12), which is in bucket (1,1)
        // Range is 5.0, bucket_size is 10. bucket_range is ceil(5/10) = 1.
        // Search will cover buckets from (1-1, 1-1) to (1+1, 1+1), i.e. (0,0) to (2,2).
        // It should find the two entities in bucket (1,1).
        let results: Vec<_> = spatial_map.search(V2::new(12.0, 12.0), 5.0).collect();
        assert_eq!(results.len(), 2);

        let expected_ids = [entities[0].0, entities[1].0];
        for item in &results {
            assert!(expected_ids.contains(&item));
        }
    }

    #[test]
    fn search_finds_in_multiple_buckets() {
        let entities = setup_entities();
        let spatial_map = SpatialMap::new(&entities, 100, 100, 10);

        // Search position is (25,25), which is in bucket (2,2)
        // Range is 15.0, bucket_size is 10. bucket_range is ceil(15/10) = 2.
        // Search will cover buckets from (2-2, 2-2) to (2+2, 2+2), i.e. (0,0) to (4,4).
        // It should find entities in bucket (1,1) and (3,3).
        let results: Vec<_> = spatial_map.search(V2::new(25.0, 25.0), 15.0).collect();
        assert_eq!(results.len(), 3);

        let expected_ids = [entities[0].0, entities[1].0, entities[2].0];
        for item in &results {
            assert!(expected_ids.contains(&item));
        }
    }

    #[test]
    fn search_edge_of_map() {
        let entities = setup_entities();
        let spatial_map = SpatialMap::new(&entities, 100, 100, 10);

        // Search position is (2,2), in bucket (0,0)
        // Range is 15.0, bucket_size is 10. bucket_range is 2.
        // Search should cover from (0-2, 0-2) to (0+2, 0+2), clamped to (0,0) to (2,2).
        // It should find entities in bucket (1,1).
        let results: Vec<_> = spatial_map.search(V2::new(2.0, 2.0), 15.0).collect();
        assert_eq!(results.len(), 2);
        let expected_ids = [entities[0].0, entities[1].0];
        for item in &results {
            assert!(expected_ids.contains(&item));
        }
    }

    #[test]
    fn search_all() {
        let entities = setup_entities();
        let spatial_map = SpatialMap::new(&entities, 100, 100, 10);
        let results: Vec<_> = spatial_map.search(V2::new(50.0, 50.0), 100.0).collect();
        assert_eq!(results.len(), 4);
    }
}
