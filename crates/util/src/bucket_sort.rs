use crate::span::Span;

#[derive(Default)]
pub struct BucketSort<T> {
    entries: Vec<T>,
    offsets: Vec<usize>,
}

impl<T: Default> BucketSort<T> {
    pub fn new(input: Vec<(T, usize)>, num_buckets: usize) -> BucketSort<T> {
        let mut row_lengths = vec![0; num_buckets];
        // Count the lengths of each bucket
        for &(_, bucket) in &input {
            row_lengths[bucket] += 1;
        }

        // Use the row lengths to calculate the 'prefix sum' for the csr spans in the map
        let offsets = prefix_sum(row_lengths.iter().copied());
        // We are going to reuse the row lengths to calculate the current write index for each row
        let mut write_indexes = vec![0; num_buckets];

        // Create the entry list
        let mut entries = Vec::new();
        entries.resize_with(input.len(), || T::default());

        // Write entries in order
        for (entry, bucket) in input {
            let write_idx = &mut write_indexes[bucket];
            entries[offsets[bucket] + *write_idx] = entry;
            // Increment write index
            *write_idx += 1;
        }

        BucketSort { entries, offsets }
    }

    pub fn num_buckets(&self) -> usize {
        self.offsets.len() - 1
    }

    pub fn get(&self, bucket: usize) -> &[T] {
        if bucket >= self.num_buckets() {
            &[]
        } else {
            let span = Span::between(self.offsets[bucket], self.offsets[bucket + 1]);
            span.view(&self.entries)
        }
    }
}

fn prefix_sum(nums: impl IntoIterator<Item = usize>) -> Vec<usize> {
    let it = nums.into_iter().scan(0, |accum, x| {
        *accum += x;
        Some(*accum)
    });
    std::iter::once(0).chain(it).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bucket_sort_simple() {
        let data = vec![(0, 0), (1, 0), (2, 1), (3, 1), (4, 2)];
        let bucket_sort = BucketSort::new(data, 3);

        assert_eq!(bucket_sort.get(0), &[0, 1]);
        assert_eq!(bucket_sort.get(1), &[2, 3]);
        assert_eq!(bucket_sort.get(2), &[4]);
    }

    #[test]
    fn test_bucket_sort_empty_bucket() {
        let data = vec![(0, 0), (1, 2)];
        let bucket_sort = BucketSort::new(data, 3);

        assert_eq!(bucket_sort.get(0), &[0]);
        assert_eq!(bucket_sort.get(1), &[]);
        assert_eq!(bucket_sort.get(2), &[1]);
    }

    #[test]
    fn test_bucket_sort_all_in_one_bucket() {
        let data = vec![(0, 0), (1, 0), (2, 0)];
        let bucket_sort = BucketSort::new(data, 1);

        assert_eq!(bucket_sort.get(0), &[0, 1, 2]);
    }

    #[test]
    fn test_bucket_sort_no_data() {
        let data: Vec<(i32, usize)> = vec![];
        let bucket_sort = BucketSort::new(data, 5);

        for i in 0..5 {
            assert_eq!(bucket_sort.get(i), &[]);
        }
    }

    #[test]
    fn test_bucket_sort_out_of_bounds_bucket() {
        let data = vec![(0, 0)];
        let bucket_sort = BucketSort::new(data, 1);

        assert_eq!(bucket_sort.get(0), &[0]);
        assert_eq!(bucket_sort.get(1), &[]); // Accessing out-of-bounds bucket should return empty
        assert_eq!(bucket_sort.get(100), &[]); // Accessing out-of-bounds bucket should return empty
    }

    #[test]
    fn test_prefix_sum() {
        assert_eq!(prefix_sum(vec![1, 2, 3].into_iter()), vec![0, 1, 3, 6]);
        assert_eq!(prefix_sum(vec![0, 0, 0].into_iter()), vec![0, 0, 0, 0]);
        assert_eq!(prefix_sum(vec![5].into_iter()), vec![0, 5]);
        assert_eq!(prefix_sum(vec![].into_iter()), vec![0]);
    }
}
