#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Span {
    base: usize,
    length: usize,
}

impl Span {
    pub fn view<T>(self, x: &[T]) -> &[T] {
        let base = self.base.min(x.len());
        let length = self.length.min(x.len() - base);
        &x[base..][..length]
    }

    pub fn between(start: usize, end: usize) -> Span {
        Span {
            base: start,
            length: end.saturating_sub(start),
        }
    }

    pub fn of_extension<T>(container: &mut Vec<T>, items: impl IntoIterator<Item = T>) -> Span {
        let base = container.len();
        container.extend(items);
        let end = container.len();
        Span {
            base,
            length: end - base,
        }
    }

    pub fn index_where<T>(self, items: &[T], f: impl Fn(&T) -> bool) -> Option<usize> {
        let slice = self.view(items);
        let idx = slice
            .iter()
            .enumerate()
            .find(|(_, val)| f(val))
            .map(|(id, _)| id)?;
        Some(self.base + idx)
    }
}
