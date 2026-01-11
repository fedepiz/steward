use std::fmt::Write;

use num_enum::TryFromPrimitive;
use strum::{EnumCount, EnumIter};
use util::string_pool::*;

#[derive(Default)]
pub(crate) struct Names {
    interner: StringPool,
}

#[derive(Default, Clone, Copy)]
pub(crate) struct WordId(SpanHandle);

impl Names {
    pub(crate) fn define(&mut self, text: &str) -> WordId {
        WordId(self.interner.push_str(text))
    }

    pub(crate) fn resolve(&self, name: Name) -> ResolvedName<'_> {
        let mut resolved = ResolvedName::default();
        for i in 0..NamePart::COUNT {
            resolved.0[i] = self.interner.get(name.0[i]);
        }
        resolved
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter, EnumCount, TryFromPrimitive)]
#[repr(u8)]
pub(crate) enum NamePart {
    Main,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Name([SpanHandle; NamePart::COUNT]);

impl Name {
    pub fn simple(word: WordId) -> Self {
        let mut this = Self::default();
        this.set(NamePart::Main, word);
        this
    }

    pub fn set(&mut self, part: NamePart, value: WordId) {
        self.0[part as usize] = value.0;
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct ResolvedName<'a>([&'a str; NamePart::COUNT]);

impl std::fmt::Display for ResolvedName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut space_needed = false;
        for part in self.0 {
            if !part.is_empty() {
                if space_needed {
                    f.write_char(' ')?;
                }
                f.write_str(part)?;
                space_needed = true;
            }
        }
        Ok(())
    }
}
