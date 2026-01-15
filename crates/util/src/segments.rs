use std::marker::PhantomData;

#[derive(Clone)]
pub struct Segments<K: Id, T> {
    data: Vec<T>,
    typ: PhantomData<K>,
}

impl<K: Id, T> Segments<K, T> {
    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn push(&mut self, items: impl IntoIterator<Item = T>) -> K {
        let begin = self.data.len();
        self.data.extend(items);
        let end = self.data.len();
        let raw = RawId { begin, end };
        K::from_raw(raw)
    }

    pub fn get(&self, id: K) -> &[T] {
        let raw = id.raw();
        let begin = raw.begin.min(self.data.len());
        let end = raw.end.clamp(begin, self.data.len());
        &self.data[begin..end]
    }
}

impl<K: Id, T> Default for Segments<K, T> {
    fn default() -> Self {
        Self {
            data: Vec::default(),
            typ: PhantomData,
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RawId {
    begin: usize,
    end: usize,
}

pub trait Id: Clone + Copy {
    fn raw(&self) -> RawId;
    fn from_raw(raw: RawId) -> Self;
}

#[macro_export]
macro_rules! new_segment_id {
    ($vis:vis struct $name: ident;) => {
        #[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        $vis struct $name($crate::segments::RawId);

        impl $crate::segments::Id for $name {
            fn raw(&self) -> $crate::segments::RawId {
                self.0
            }

            fn from_raw(raw: $crate::segments::RawId) -> Self {
                Self(raw)
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    new_segment_id!(pub struct MyId);

    #[test]
    fn test_push_and_get() {
        let mut segments: Segments<MyId, u32> = Segments::default();
        let data = vec![1, 2, 3];
        let id = segments.push(data.clone());
        let segment = segments.get(id);
        assert_eq!(segment, &data);
    }

    #[test]
    fn test_multiple_segments() {
        let mut segments: Segments<MyId, u32> = Segments::default();

        let data1 = vec![1, 2, 3];
        let id1 = segments.push(data1.clone());

        let data2 = vec![4, 5];
        let id2 = segments.push(data2.clone());

        let data3 = vec![6, 7, 8, 9];
        let id3 = segments.push(data3.clone());

        assert_eq!(segments.get(id1), &data1);
        assert_eq!(segments.get(id2), &data2);
        assert_eq!(segments.get(id3), &data3);
    }

    #[test]
    fn test_empty_segment() {
        let mut segments: Segments<MyId, u32> = Segments::default();
        let data: Vec<u32> = vec![];
        let id = segments.push(data.clone());
        let segment = segments.get(id);
        assert_eq!(segment, &data);
        assert!(segment.is_empty());
    }

    #[test]
    fn test_get_from_empty() {
        let segments: Segments<MyId, u32> = Segments::default();
        let raw_id = RawId { begin: 0, end: 0 };
        let id = MyId::from_raw(raw_id);
        let segment = segments.get(id);
        assert!(segment.is_empty());
    }

    #[test]
    fn test_out_of_bounds_get() {
        let mut segments: Segments<MyId, u32> = Segments::default();
        let data = vec![1, 2, 3];
        segments.push(data);

        // Create an ID that is out of bounds.
        let raw_id = RawId {
            begin: 100,
            end: 200,
        };
        let id = MyId::from_raw(raw_id);
        let segment = segments.get(id);

        // The implementation should clamp the indices, resulting in an empty slice.
        assert!(segment.is_empty());
    }
}
