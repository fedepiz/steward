use crate::vec_map::VecMap;
use std::ops::{AddAssign, MulAssign, Sub, SubAssign, Neg};
use std::iter::FromIterator;

pub struct Tally<K>(VecMap<K, f32>);

impl<K: Copy + Ord> Tally<K> {
    /// Creates a new, empty Tally.
    pub fn new() -> Self {
        Self(VecMap::new())
    }

    /// Returns the value associated with a key.
    /// If the key is not in the tally, it returns 0.0.
    pub fn get(&self, key: &K) -> f32 {
        self.0.get(key).copied().unwrap_or(0.0)
    }

    /// Sets the value for a key.
    /// If the value is 0.0, the key is removed from the tally.
    pub fn set(&mut self, key: K, value: f32) {
        if value == 0.0 {
            self.0.remove(&key);
        } else {
            self.0.insert(key, value);
        }
    }

    /// Adds a scalar to the value of a key.
    pub fn add(&mut self, key: K, scalar: f32) {
        if scalar == 0.0 {
            return;
        }
        let new_value = self.get(&key) + scalar;
        self.set(key, new_value);
    }

    /// Returns an iterator over the non-zero key-value pairs.
    pub fn iter(&self) -> std::slice::Iter<'_, (K, f32)> {
        self.0.iter()
    }

    /// Returns an iterator that allows modifying the values.
    pub fn iter_mut(&mut self) -> crate::vec_map::IterMut<'_, K, f32> {
        self.0.iter_mut()
    }

    /// Returns the number of non-zero entries in the Tally.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the Tally contains no non-zero entries.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Removes all entries from the Tally, making it empty.
    pub fn clear(&mut self) {
        self.0.clear();
    }
}

impl<K: Copy + Ord> Default for Tally<K> {
    fn default() -> Self {
        Self::new()
    }
}

// --- Vector Operations ---

impl<K: Copy + Ord> Tally<K> {
    /// Adds another Tally to this one.
    pub fn add_tally(&mut self, other: &Tally<K>) {
        for (key, value) in other.iter() {
            self.add(*key, *value);
        }
    }

    /// Computes the dot product with another Tally.
    pub fn dot(&self, other: &Tally<K>) -> f32 {
        let (smaller, larger) = if self.0.len() < other.0.len() {
            (self, other)
        } else {
            (other, self)
        };

        smaller.iter().map(|(k, v)| *v * larger.get(k)).sum()
    }

    /// Scales all values in the Tally by a scalar.
    pub fn scale(&mut self, scalar: f32) {
        if scalar == 0.0 {
            self.0.clear();
            return;
        }
        for (_, value) in self.iter_mut() {
            *value *= scalar;
        }
    }

    /// Computes the squared magnitude of the Tally.
    pub fn magnitude_sq(&self) -> f32 {
        self.iter().map(|(_, v)| v * v).sum()
    }

    /// Computes the magnitude (or L2 norm) of the Tally.
    pub fn magnitude(&self) -> f32 {
        self.magnitude_sq().sqrt()
    }

    /// Normalizes the Tally to a unit vector.
    pub fn normalize(&mut self) {
        let mag = self.magnitude();
        if mag > 0.0 {
            self.scale(1.0 / mag);
        }
    }
}

// --- Operator Overloads ---

impl<K: Copy + Ord> AddAssign<&Tally<K>> for Tally<K> {
    fn add_assign(&mut self, rhs: &Tally<K>) {
        self.add_tally(rhs);
    }
}

impl<K: Copy + Ord> MulAssign<f32> for Tally<K> {
    fn mul_assign(&mut self, rhs: f32) {
        self.scale(rhs);
    }
}

impl<'a, 'b, K: Copy + Ord> Sub<&'b Tally<K>> for &'a Tally<K> {
    type Output = Tally<K>;

    fn sub(self, rhs: &'b Tally<K>) -> Self::Output {
        let mut result = Tally::new();
        // Start with self's values
        for (k, v) in self.iter() {
            result.set(*k, *v);
        }
        // Subtract rhs's values
        for (k, v) in rhs.iter() {
            result.add(*k, -*v); // Add the negative of the value
        }
        result
    }
}

impl<K: Copy + Ord> SubAssign<&Tally<K>> for Tally<K> {
    fn sub_assign(&mut self, rhs: &Tally<K>) {
        for (key, value) in rhs.iter() {
            self.add(*key, -*value); // Add the negative of the value
        }
    }
}

impl<K: Copy + Ord> Neg for &Tally<K> {
    type Output = Tally<K>;

    fn neg(self) -> Self::Output {
        let mut result = Tally::new();
        for (key, value) in self.iter() {
            // Only include non-zero negated values
            result.set(*key, -*value);
        }
        result
    }
}

impl<K: Copy + Ord> FromIterator<(K, f32)> for Tally<K> {
    fn from_iter<T: IntoIterator<Item = (K, f32)>>(iter: T) -> Self {
        let mut tally = Tally::new();
        for (key, value) in iter {
            tally.add(key, value); // Accumulate
        }
        tally
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_get_set() {
        let mut tally = Tally::new();
        assert_eq!(tally.get(&1), 0.0);
        tally.set(1, 10.0);
        assert_eq!(tally.get(&1), 10.0);
        tally.set(1, 0.0);
        assert_eq!(tally.get(&1), 0.0);
    }

    #[test]
    fn test_add() {
        let mut tally = Tally::new();
        tally.add(1, 5.0);
        assert_eq!(tally.get(&1), 5.0);
        tally.add(1, -2.0);
        assert_eq!(tally.get(&1), 3.0);
        tally.add(1, -3.0);
        assert_eq!(tally.get(&1), 0.0);
    }

    #[test]
    fn test_iter() {
        let mut tally = Tally::new();
        tally.set(1, 1.0);
        tally.set(2, 2.0);
        tally.set(3, 0.0);

        let mut values = tally.iter().map(|(k, v)| (*k, *v)).collect::<Vec<_>>();
        values.sort_by(|a, b| a.0.cmp(&b.0));
        assert_eq!(values, vec![(1, 1.0), (2, 2.0)]);
    }

    #[test]
    fn test_add_tally() {
        let mut tally1 = Tally::new();
        tally1.set(1, 1.0);
        tally1.set(2, 2.0);

        let mut tally2 = Tally::new();
        tally2.set(2, 3.0);
        tally2.set(3, 4.0);

        tally1.add_tally(&tally2);

        assert_eq!(tally1.get(&1), 1.0);
        assert_eq!(tally1.get(&2), 5.0);
        assert_eq!(tally1.get(&3), 4.0);
    }

    #[test]
    fn test_dot_product() {
        let mut tally1 = Tally::new();
        tally1.set(1, 2.0);
        tally1.set(2, 3.0);

        let mut tally2 = Tally::new();
        tally2.set(2, 4.0);
        tally2.set(3, 5.0);

        assert_eq!(tally1.dot(&tally2), 12.0); // 2*0 + 3*4 + 0*5
    }

    #[test]
    fn test_scale() {
        let mut tally = Tally::new();
        tally.set(1, 2.0);
        tally.set(2, -3.0);
        tally.scale(2.0);
        assert_eq!(tally.get(&1), 4.0);
        assert_eq!(tally.get(&2), -6.0);
        tally.scale(0.0);
        assert_eq!(tally.get(&1), 0.0);
        assert_eq!(tally.get(&2), 0.0);
    }

    #[test]
    fn test_magnitude_and_normalize() {
        let mut tally = Tally::new();
        tally.set(1, 3.0);
        tally.set(2, 4.0);

        assert_eq!(tally.magnitude_sq(), 25.0);
        assert_eq!(tally.magnitude(), 5.0);

        tally.normalize();
        assert!((tally.get(&1) - 0.6).abs() < 1e-6);
        assert!((tally.get(&2) - 0.8).abs() < 1e-6);
        assert!((tally.magnitude() - 1.0).abs() < 1e-6);
    }

    #[test]
    fn test_ops_assign() {
        let mut tally1 = Tally::new();
        tally1.set(1, 1.0);
        tally1.set(2, 2.0);

        let mut tally2 = Tally::new();
        tally2.set(2, 3.0);
        tally2.set(3, 4.0);

        tally1 += &tally2;

        assert_eq!(tally1.get(&1), 1.0);
        assert_eq!(tally1.get(&2), 5.0);
        assert_eq!(tally1.get(&3), 4.0);

        tally1 *= 2.0;
        assert_eq!(tally1.get(&1), 2.0);
        assert_eq!(tally1.get(&2), 10.0);
        assert_eq!(tally1.get(&3), 8.0);
    }

    #[test]
    fn test_len_is_empty_clear() {
        let mut tally = Tally::new();
        assert!(tally.is_empty());
        assert_eq!(tally.len(), 0);

        tally.set(1, 10.0);
        assert!(!tally.is_empty());
        assert_eq!(tally.len(), 1);

        tally.set(2, 20.0);
        assert_eq!(tally.len(), 2);

        tally.set(1, 0.0); // Should remove key 1
        assert_eq!(tally.len(), 1);
        assert_eq!(tally.get(&2), 20.0);

        tally.clear();
        assert!(tally.is_empty());
        assert_eq!(tally.len(), 0);
    }

    #[test]
    fn test_neg() {
        let mut tally = Tally::new();
        tally.set(1, 5.0);
        tally.set(2, -10.0);
        tally.set(3, 0.0); // Should not appear in result

        let negated_tally = -&tally; // Use &tally for Neg trait
        assert_eq!(negated_tally.get(&1), -5.0);
        assert_eq!(negated_tally.get(&2), 10.0);
        assert_eq!(negated_tally.get(&3), 0.0);
        assert_eq!(negated_tally.len(), 2);
    }

    #[test]
    fn test_sub() {
        let mut tally1 = Tally::new();
        tally1.set(1, 10.0);
        tally1.set(2, 5.0);

        let mut tally2 = Tally::new();
        tally2.set(1, 3.0);
        tally2.set(3, 2.0);

        let tally3 = &tally1 - &tally2; // Use &tally for Sub trait
        assert_eq!(tally3.get(&1), 7.0);
        assert_eq!(tally3.get(&2), 5.0);
        assert_eq!(tally3.get(&3), -2.0);
        assert_eq!(tally3.len(), 3);
    }

    #[test]
    fn test_sub_assign() {
        let mut tally1 = Tally::new();
        tally1.set(1, 10.0);
        tally1.set(2, 5.0);

        let mut tally2 = Tally::new();
        tally2.set(1, 3.0);
        tally2.set(3, 2.0);

        tally1 -= &tally2; // Use &tally for SubAssign trait
        assert_eq!(tally1.get(&1), 7.0);
        assert_eq!(tally1.get(&2), 5.0);
        assert_eq!(tally1.get(&3), -2.0);
        assert_eq!(tally1.len(), 3);
    }

    #[test]
    fn test_from_iterator() {
        let data = vec![(1, 5.0), (2, 10.0), (1, 3.0), (3, 2.0), (2, -10.0)];
        let tally: Tally<i32> = data.into_iter().collect();

        assert_eq!(tally.get(&1), 8.0); // 5.0 + 3.0
        assert_eq!(tally.get(&2), 0.0); // 10.0 - 10.0
        assert_eq!(tally.get(&3), 2.0);
        assert_eq!(tally.len(), 2); // (1, 8.0) and (3, 2.0)
    }
}