use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Default)]
pub(crate) struct TerrainMap {
    terrain_types: Vec<TerrainType>,
    cells: Vec<TerrainCell>,
    width: usize,
    height: usize,
    hash: u64,
}

impl TerrainMap {
    const EMPTY_TERRAIN: TerrainType = TerrainType {
        name: "NULL",
        movement_speed_multiplier: 0.,
        color: (0, 0, 0),
        elevation_range: (0, 0),
    };

    pub fn terrain_at(&self, x: usize, y: usize) -> &TerrainType {
        let idx = self.width * y + x;
        self.terrain_types.get(idx).unwrap_or(&Self::EMPTY_TERRAIN)
    }

    pub fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    pub fn hash(&self) -> u64 {
        self.hash
    }

    pub fn iter_terrains(&self) -> impl Iterator<Item = &TerrainType> {
        self.cells.iter().map(|cell| {
            self.terrain_types
                .get(cell.type_idx)
                .unwrap_or(&Self::EMPTY_TERRAIN)
        })
    }
}

#[derive(Default, Clone, Copy)]
pub(crate) struct TerrainType {
    pub(crate) name: &'static str,
    pub(crate) movement_speed_multiplier: f32,
    pub(crate) color: (u8, u8, u8),
    pub(crate) elevation_range: (u8, u8),
}

#[derive(Default, Clone, Copy)]
pub(crate) struct TerrainCell {
    type_idx: usize,
}

pub(crate) fn init(elevations: &[u8], width: usize, height: usize) -> TerrainMap {
    assert!(elevations.len() == width * height);
    let mut terrain_map = TerrainMap::default();
    terrain_map.terrain_types = init_types();
    terrain_map.width = width;
    terrain_map.height = height;
    terrain_map.cells = elevations
        .iter()
        .map(|&elevation| {
            let elevation = elevation.min(100);
            let type_idx = terrain_map
                .terrain_types
                .iter()
                .position(|typ| {
                    let (min, max) = typ.elevation_range;
                    elevation >= min && elevation <= max
                })
                .unwrap_or(0);
            TerrainCell { type_idx }
        })
        .collect();

    terrain_map.hash = {
        let mut hasher = DefaultHasher::new();
        elevations.hash(&mut hasher);
        hasher.finish()
    };

    terrain_map
}

fn init_types() -> Vec<TerrainType> {
    [
        TerrainType {
            name: "Water",
            movement_speed_multiplier: 0.,
            color: (0, 0, 255),
            elevation_range: (0, 5),
        },
        TerrainType {
            name: "Plains",
            movement_speed_multiplier: 1.,
            color: (0, 255, 0),
            elevation_range: (5, 15),
        },
        TerrainType {
            name: "Forest",
            movement_speed_multiplier: 0.85,
            color: (0, 125, 0),
            elevation_range: (15, 30),
        },
        TerrainType {
            name: "Hills",
            movement_speed_multiplier: 0.85,
            color: (125, 50, 50),
            elevation_range: (30, 50),
        },
        TerrainType {
            name: "Mountains",
            movement_speed_multiplier: 0.,
            color: (255, 255, 255),
            elevation_range: (50, 100),
        },
    ]
    .into_iter()
    .collect()
}
