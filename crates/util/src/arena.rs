use bumpalo::*;
#[derive(Default)]
pub struct Arena(Bump);

impl Arena {
    #[inline]
    pub fn new() -> Self {
        Self(Bump::new())
    }

    #[inline]
    pub fn reset(&mut self) {
        self.0.reset();
    }

    #[inline]
    pub fn new_vec<T>(&self) -> AVec<'_, T> {
        AVec::new_in(&self.0)
    }

    #[inline]
    pub fn new_vec_with_capacity<T>(&self, capacity: usize) -> AVec<'_, T> {
        AVec::with_capacity_in(capacity, &self.0)
    }

    #[inline]
    pub fn new_string_with_capacity(&self, capacity: usize) -> AString<'_> {
        AString::with_capacity_in(capacity, &self.0)
    }

    #[inline]
    pub fn alloc_str(&self, src: &str) -> &str {
        self.0.alloc_str(src)
    }
}

pub type AString<'a> = bumpalo::collections::String<'a>;
pub type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;
