use slotmap::SecondaryMap;
use util::bucket_sort::BucketSort;

use crate::{entities::EntityId, geom::V2};

#[derive(Default)]
pub(crate) struct MovementSystem {
    entities: SecondaryMap<EntityId, MovementData>,
    spatial_map: Option<SpatialMap>,
    map_width: usize,
    map_height: usize,
}

#[derive(Default)]
pub(crate) struct Input {
    pub advance_time: bool,
    pub want_new_path: Vec<(EntityId, V2)>,
}

#[derive(Default)]
pub(crate) struct Output {
    pub update_positions: Vec<(EntityId, V2)>,
}

impl MovementSystem {
    pub fn new(map_width: usize, map_height: usize) -> Self {
        Self {
            map_width,
            map_height,
            ..Default::default()
        }
    }

    const SPEED_MULT: f32 = 0.025;
    const BUCKET_SIZE: usize = 32;

    pub fn tick(&mut self, input: Input) -> Output {
        let mut out = Output::default();
        if input.advance_time {
            // Calculate paths for all those that want a new one.
            for (id, destination) in input.want_new_path {
                let data = &mut self.entities[id];
                data.path = calculate_new_path(data.pos, destination);
                data.next_step = data.path.last().copied().unwrap_or(data.pos);
            }

            // Calculate new positins for each entity
            let next_pos: Vec<_> = self
                .entities
                .iter_mut()
                .map(|(id, data)| {
                    let next_pos = calculate_next_position(data, Self::SPEED_MULT);
                    if data.next_step == next_pos {
                        // Advance path
                        data.path.pop();
                        data.next_step = data.path.last().copied().unwrap_or(data.pos);
                    }
                    (id, next_pos)
                })
                .collect();

            // Recalculate the spatial map
            self.spatial_map = Some(SpatialMap::new(
                &next_pos,
                self.map_width,
                self.map_height,
                Self::BUCKET_SIZE,
            ));

            // Return only what positions change for the external world to update
            out.update_positions = self
                .entities
                .iter_mut()
                .zip(next_pos)
                .filter(|((_, data), new_pos)| data.pos != new_pos.1)
                .map(|((_, data), (id, new_pos))| {
                    data.pos = new_pos;
                    (id, new_pos)
                })
                .collect();
        }
        out
    }

    pub fn insert(&mut self, entities: &[(EntityId, V2, f32)]) {
        for &(id, pos, speed) in entities {
            self.entities.insert(id, MovementData::new(pos, speed));
        }
    }

    pub fn remove(&mut self, entities: &[EntityId]) {
        for &id in entities {
            self.entities.remove(id);
        }
    }
}

fn calculate_next_position(data: &MovementData, speed_mult: f32) -> V2 {
    if data.pos == data.next_step {
        return data.pos;
    }
    let speed = data.speed * speed_mult;
    let dv = data.next_step - data.pos;
    let length = dv.magnitude();
    let change = if length <= speed {
        dv
    } else {
        dv.normalize() * speed
    };
    data.pos + change
}

fn calculate_new_path(source: V2, dest: V2) -> Vec<V2> {
    // TODO: Actual pathfinding, once we have a terrain grid
    vec![dest]
}

#[derive(Default)]
struct MovementData {
    /// Current position
    pos: V2,
    /// Position the entity is moving towards.
    /// When path.len() == 0, this should be identical to pos,
    /// else, it should be identical to path.last().
    /// (Ie, it's a trick to avoid a cache miss to find the next destination)
    next_step: V2,
    /// The entity movement speed
    speed: f32,
    /// All the following steps.
    path: Vec<V2>,
}

impl MovementData {
    fn new(pos: V2, speed: f32) -> Self {
        Self {
            pos,
            next_step: pos,
            speed,
            ..Default::default()
        }
    }
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
    fn new(entities: &[(EntityId, V2)], width: usize, height: usize, bucket_size: usize) -> Self {
        debug_assert!(
            width % bucket_size == 0,
            "width must be divisible by bucket_size"
        );
        debug_assert!(
            height % bucket_size == 0,
            "height must be divisible by bucket_size"
        );

        let num_buckets_x = width / bucket_size;
        let num_buckets = {
            let num_buckets_y = height / bucket_size;
            num_buckets_x * num_buckets_y
        };

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

    pub fn search(&self, pos: V2, range: f32) -> impl Iterator<Item = SpatialMapEntry> {
        // Determine the range of buckets to check
        let bucket_range = (range / self.bucket_size as f32).ceil() as isize;

        let center_bucket_x = (pos.x as usize / self.bucket_size) as isize;
        let center_bucket_y = (pos.y as usize / self.bucket_size) as isize;

        let num_buckets_x = self.width / self.bucket_size;
        let num_buckets_y = self.height / self.bucket_size;

        let min_bx = (center_bucket_x - bucket_range).max(0);
        let max_bx = (center_bucket_x + bucket_range).min(num_buckets_x as isize - 1);

        let min_by = (center_bucket_y - bucket_range).max(0);
        let max_by = (center_bucket_y + bucket_range).min(num_buckets_y as isize - 1);

        // Iterate over the buckets in the range
        (min_by..=max_by).flat_map(move |by| {
            (min_bx..=max_bx).flat_map(move |bx| {
                let bucket_idx = (bx + by * num_buckets_x as isize) as usize;

                // For each bucket, iterate over the (entity ID, pos) tuples and map them to SpatialItems
                self.sort.get(bucket_idx).iter().copied()
            })
        })
    }
}

#[derive(Default, PartialEq, Debug, Clone, Copy)]
pub(crate) struct SpatialMapEntry {
    pub id: EntityId,
    pub pos: V2,
}

#[cfg(test)]
mod spatial_map_tests {
    use super::*;
    use slotmap::SlotMap;

    fn setup_entities() -> Vec<(EntityId, V2)> {
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
            assert!(expected_ids.contains(&item.id));
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
            assert!(expected_ids.contains(&item.id));
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
            assert!(expected_ids.contains(&item.id));
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
