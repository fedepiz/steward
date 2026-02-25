use bumpalo::*;
#[derive(Default)]
pub struct Arena(Bump);

impl Arena {
    pub fn new() -> Self {
        Self(Bump::new())
    }

    pub fn reset(&mut self) {
        self.0.reset();
    }
}

pub type AString<'a> = bumpalo::collections::String<'a>;
pub type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;
