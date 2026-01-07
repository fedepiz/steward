use std::fmt::Write;
/// Handle to a string in a StringPool.
#[derive(Clone, Copy, Default)]
pub struct SpanHandle {
    offset: usize,
    len: usize,
}

/// A pool of strings stored contiguously. Supports push and retrieval via TextSpan handles.
#[derive(Default)]
pub struct StringPool {
    buffer: String,
}

impl StringPool {
    /// Clears all strings from the pool.
    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    /// Pushes a string into the pool and returns a handle to it.
    pub fn push(&mut self, s: &str) -> SpanHandle {
        let span = SpanHandle {
            offset: self.buffer.len(),
            len: s.len(),
        };
        self.buffer.push_str(s);
        span
    }

    /// Pushes a string format into the pool and returns a handle to it.
    pub fn push_fmt(&mut self, args: std::fmt::Arguments) -> SpanHandle {
        let offset = self.buffer.len();
        let _ = self.buffer.write_fmt(args);
        SpanHandle {
            offset,
            len: self.buffer.len().saturating_sub(offset),
        }
    }

    /// Retrieves a string by its handle.
    pub fn get(&self, span: SpanHandle) -> &str {
        &self.buffer[span.offset..][..span.len]
    }
}
