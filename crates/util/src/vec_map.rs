use std::borrow::Borrow;

pub struct VecMap<K, V>(Vec<(K, V)>);

impl<K, V> Default for VecMap<K, V> {
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<K: PartialOrd, V> VecMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, key: K, value: V) {
        match self.get_mut(&key) {
            Some(entry) => *entry = value,
            None => {
                self.0.push((key, value));
            }
        }
    }

    pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: PartialEq,
    {
        let idx = self.0.iter().position(|(k, _)| k.borrow() == key)?;
        Some(self.0.swap_remove(idx).1)
    }

    pub fn contains<Q: ?Sized>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: PartialEq,
    {
        self.0.iter().any(|(k, _)| k.borrow() == key)
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: PartialEq,
    {
        self.0
            .iter()
            .find(|(k, _)| k.borrow() == key)
            .map(|(_, v)| v)
    }

    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: PartialEq,
    {
        self.0
            .iter_mut()
            .find(|(k, _)| k.borrow() == key)
            .map(|(_, v)| v)
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, (K, V)> {
        self.into_iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        self.into_iter()
    }
}

impl<'a, K: PartialOrd, V> std::ops::Index<&'a K> for VecMap<K, V> {
    type Output = V;

    fn index(&self, index: &'a K) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<'a, K: PartialOrd, V> std::ops::IndexMut<&'a K> for VecMap<K, V> {
    fn index_mut(&mut self, index: &'a K) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

impl<K, V> IntoIterator for VecMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a VecMap<K, V> {
    type Item = &'a (K, V);
    type IntoIter = std::slice::Iter<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, K, V> IntoIterator for &'a mut VecMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut(self.0.iter_mut())
    }
}

pub struct IterMut<'a, K, V>(std::slice::IterMut<'a, (K, V)>);

impl<'a, K: 'a, V: 'a> Iterator for IterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        let (k, v) = self.0.next()?;
        Some((k, v))
    }
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for IterMut<'a, K, V> {}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for IterMut<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let (k, v) = self.0.next_back()?;
        Some((k, v))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_len_clear() {
        let mut map: VecMap<i32, &str> = VecMap::new();
        assert_eq!(map.len(), 0);
        map.insert(1, "a");
        assert_eq!(map.len(), 1);
        map.clear();
        assert_eq!(map.len(), 0);
    }

    #[test]
    fn test_insert_get() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        assert_eq!(map.get("a"), Some(&1));
        assert_eq!(map.get("b"), None);
    }

    #[test]
    fn test_insert_overwrite() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        map.insert("a".to_string(), 2);
        assert_eq!(map.get("a"), Some(&2));
        assert_eq!(map.len(), 1);
    }

    #[test]
    fn test_remove() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        assert_eq!(map.remove("a"), Some(1));
        assert_eq!(map.len(), 0);
        assert_eq!(map.remove("a"), None);
    }

    #[test]
    fn test_contains() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        assert!(map.contains("a"));
        assert!(!map.contains("b"));
    }

    #[test]
    fn test_get_mut() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        if let Some(val) = map.get_mut("a") {
            *val = 2;
        }
        assert_eq!(map.get("a"), Some(&2));
    }

    #[test]
    fn test_index() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        // Note: Indexing still requires the exact key type or a compatible borrow.
        // For String keys, you can't use &str directly with the current Index impl.
        // You would typically use map.get("a").unwrap() or pass &"a".to_string().
        // For the purpose of this test, we demonstrate a working scenario with &String.
        assert_eq!(map[&"a".to_string()], 1);
    }

    #[test]
    #[should_panic]
    fn test_index_panic() {
        let map: VecMap<String, i32> = VecMap::new();
        let _ = map[&"a".to_string()];
    }

    #[test]
    fn test_index_mut() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        map[&"a".to_string()] = 2;
        assert_eq!(map[&"a".to_string()], 2);
    }

    #[test]
    fn test_iter() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        map.insert("b".to_string(), 2);
        let mut count = 0;
        let mut found_a = false;
        let mut found_b = false;
        for (k, v) in map.iter() {
            count += 1;
            if k == "a" && *v == 1 {
                found_a = true;
            } else if k == "b" && *v == 2 {
                found_b = true;
            }
        }
        assert_eq!(count, 2);
        assert!(found_a);
        assert!(found_b);
    }

    #[test]
    fn test_iter_mut() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        map.insert("b".to_string(), 2);
        for (k, v) in map.iter_mut() {
            if k == "a" {
                *v = 3;
            }
        }
        assert_eq!(map.get("a"), Some(&3));
        assert_eq!(map.get("b"), Some(&2));
    }

    #[test]
    fn test_into_iter() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        map.insert("b".to_string(), 2);

        let mut vec: Vec<(String, i32)> = map.into_iter().collect();
        vec.sort_by(|a, b| a.0.cmp(&b.0));
        assert_eq!(vec, vec![("a".to_string(), 1), ("b".to_string(), 2)]);
    }

    #[test]
    fn test_string_key_lookup() {
        let mut map = VecMap::new();
        map.insert("a".to_string(), 1);
        // After the fix, this now works directly with `&str`
        assert_eq!(map.get("a"), Some(&1));
    }
}
