#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BitSet<const N: usize>([u64; N]);

impl<const N: usize> Default for BitSet<N> {
    fn default() -> Self {
        Self([0; N])
    }
}

impl<const N: usize> BitSet<N> {
    pub fn set(&mut self, i: usize, value: bool) {
        if i >= N * 64 {
            return;
        }
        let outer_idx = i / 64;
        let inner_idx = i % 64;
        let mask = 1u64 << inner_idx;
        if value {
            self.0[outer_idx] |= mask;
        } else {
            self.0[outer_idx] &= !mask;
        }
    }

    pub fn get(&self, i: usize) -> bool {
        if i >= N * 64 {
            return false;
        }
        let outer_idx = i / 64;
        let inner_idx = i % 64;
        let mask = 1u64 << inner_idx;
        let read = self.0[outer_idx] & mask;
        read != 0
    }

    pub fn clear(&mut self) {
        self.0 = [0; N]
    }

    pub fn iter(&self) -> BitSetIter<'_, N> {
        let first = if N > 0 { self.0[0] } else { 0 };
        BitSetIter {
            bits: &self.0,
            word: 0,
            current: first,
        }
    }
}

pub struct BitSetIter<'a, const N: usize> {
    bits: &'a [u64; N],
    word: usize,
    current: u64,
}

impl<'a, const N: usize> Iterator for BitSetIter<'a, N> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current != 0 {
                let bit = self.current.trailing_zeros() as usize;
                self.current &= self.current - 1;
                return Some(self.word * 64 + bit);
            }

            self.word += 1;
            if self.word >= N {
                return None;
            }
            self.current = self.bits[self.word];
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iter_empty_is_none() {
        let bits: BitSet<2> = BitSet::default();
        let mut iter = bits.iter();
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn iter_single_word_indices() {
        let mut bits: BitSet<1> = BitSet::default();
        bits.set(0, true);
        bits.set(3, true);
        bits.set(63, true);

        let collected: Vec<_> = bits.iter().collect();
        assert_eq!(collected, vec![0, 3, 63]);
    }

    #[test]
    fn iter_multiple_words_indices() {
        let mut bits: BitSet<2> = BitSet::default();
        bits.set(1, true);
        bits.set(64, true);
        bits.set(127, true);

        let collected: Vec<_> = bits.iter().collect();
        assert_eq!(collected, vec![1, 64, 127]);
    }

    #[test]
    fn iter_after_clearing_bits() {
        let mut bits: BitSet<1> = BitSet::default();
        bits.set(2, true);
        bits.set(4, true);
        bits.set(2, false);

        let collected: Vec<_> = bits.iter().collect();
        assert_eq!(collected, vec![4]);
    }
}
