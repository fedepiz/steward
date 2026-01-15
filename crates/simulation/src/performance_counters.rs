use std::time::Duration;

use strum::{EnumCount, EnumIter, IntoEnumIterator};

#[derive(Default)]
pub(crate) struct PerformanceCounts {
    counts: [usize; PerfCounter::COUNT],
    times: [Duration; PerfTime::COUNT],
}

impl PerformanceCounts {
    pub fn add_count(&mut self, counter: PerfCounter, count: usize) {
        let idx = counter as usize;
        self.counts[idx] += count;
    }

    pub fn add_time(&mut self, time: PerfTime, duration: Duration) {
        let idx = time as usize;
        self.times[idx] += duration;
    }

    pub fn print(&self, turn_num: usize) {
        println!("PRINTING TIMING METRICS FOR TURN {turn_num}");
        for counter in PerfCounter::iter() {
            let idx = counter as usize;
            let count = self.counts[idx];
            let name = match counter {
                PerfCounter::DistanceCalcsInCollisionCheck => "Distance calcs in collision checks",
            };
            println!("{name}: {count}");
        }

        for timing in PerfTime::iter() {
            let idx = timing as usize;
            let duration = self.times[idx];
            let name = match timing {
                PerfTime::BuildSpatialMap => "Spatial Map building",
            };
            println!("{name}: {:1.2} ms", duration.as_nanos() as f64 / 1_000_000.);
        }
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, EnumIter, EnumCount)]
pub(crate) enum PerfCounter {
    DistanceCalcsInCollisionCheck,
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, EnumIter, EnumCount)]
pub(crate) enum PerfTime {
    BuildSpatialMap,
}
