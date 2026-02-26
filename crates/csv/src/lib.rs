use util::arena::{AVec, Arena};

#[derive(Clone, Copy, Debug, Default)]
pub struct Entry<'a> {
    str: &'a str,
    num: f32,
}

const DUMMY: Entry = Entry { str: "", num: 0. };

impl<'a> Entry<'a> {
    #[inline]
    pub fn as_str(&self) -> &'a str {
        self.str
    }

    #[inline]
    pub fn as_num(&self) -> f32 {
        self.num
    }

    fn from_field(field: &'a str) -> Self {
        let field = field.trim();
        Self {
            str: field,
            num: parse_number(field),
        }
    }
}

#[derive(Clone, Copy, Debug)]
// A row is an arena-backed slice of parsed values.
pub struct Row<'a>(&'a [Entry<'a>]);

impl<'a> Row<'a> {
    pub fn len(self) -> usize {
        self.0.len()
    }

    pub fn is_empty(self) -> bool {
        self.0.is_empty()
    }

    pub fn get(self, index: usize) -> Option<Entry<'a>> {
        self.0.get(index).copied()
    }

    pub fn entries(self) -> &'a [Entry<'a>] {
        self.0
    }
}

impl<'a> std::ops::Index<usize> for Row<'a> {
    type Output = Entry<'a>;
    fn index(&self, index: usize) -> &Self::Output {
        self.0.get(index).unwrap_or(&DUMMY)
    }
}

#[derive(Clone, Copy, Debug)]
// A table is an arena-backed slice of rows.
pub struct Table<'a>(&'a [Row<'a>]);

impl<'a> Table<'a> {
    pub fn parse(arena: &'a Arena, text: &'a str) -> Self {
        parse(arena, text)
    }

    pub fn len(self) -> usize {
        self.0.len()
    }

    pub fn is_empty(self) -> bool {
        self.0.is_empty()
    }

    pub fn get(self, index: usize) -> Option<Row<'a>> {
        self.0.get(index).copied()
    }

    pub fn rows(self) -> &'a [Row<'a>] {
        self.0
    }
}

#[derive(Clone, Copy)]
enum Terminator {
    Comma,
    Newline,
    End,
}

pub fn parse_file<'a>(arena: &'a Arena, path: &str) -> Table<'a> {
    let src = arena.alloc_str(&std::fs::read_to_string(path).unwrap_or_default());
    parse(arena, &src)
}

pub fn parse<'a>(arena: &'a Arena, text: &'a str) -> Table<'a> {
    // Fast path for empty input: return an empty arena slice with no heap allocations.
    if text.is_empty() {
        let rows: AVec<'a, Row<'a>> = arena.new_vec();
        return Table(rows.into_bump_slice());
    }

    let mut rows: AVec<'a, Row<'a>> = arena.new_vec();
    let mut row_entries: AVec<'a, Entry<'a>> = arena.new_vec();
    let mut idx = 0usize;

    // Single-pass parser: consume one field at a time and flush rows on newline/end.
    loop {
        let (entry, next_idx, terminator) = parse_entry(arena, text, idx);
        row_entries.push(entry);
        idx = next_idx;

        match terminator {
            Terminator::Comma => {}
            Terminator::Newline => {
                rows.push(Row(row_entries.into_bump_slice()));
                row_entries = arena.new_vec();
                if idx >= text.len() {
                    break;
                }
            }
            Terminator::End => {
                rows.push(Row(row_entries.into_bump_slice()));
                break;
            }
        }
    }

    Table(rows.into_bump_slice())
}

fn parse_entry<'a>(
    arena: &'a Arena,
    text: &'a str,
    start: usize,
) -> (Entry<'a>, usize, Terminator) {
    let bytes = text.as_bytes();

    if start >= bytes.len() {
        return (Entry::from_field(""), start, Terminator::End);
    }

    // Quoted entries allow delimiters/newlines and escaped quotes; unquoted entries are cheaper.
    if bytes[start] == b'"' {
        parse_quoted_entry(arena, text, start)
    } else {
        parse_unquoted_entry(text, start)
    }
}

fn parse_unquoted_entry(text: &str, start: usize) -> (Entry<'_>, usize, Terminator) {
    let bytes = text.as_bytes();
    let mut end = start;

    while end < bytes.len() && bytes[end] != b',' && bytes[end] != b'\n' && bytes[end] != b'\r' {
        end += 1;
    }

    // Keep the original text, and also cache the numeric form when parsing succeeds.
    let field = &text[start..end];
    let entry = Entry::from_field(field);

    let (next, terminator) = parse_terminator(bytes, end);
    (entry, next, terminator)
}

fn parse_quoted_entry<'a>(
    arena: &'a Arena,
    text: &'a str,
    start: usize,
) -> (Entry<'a>, usize, Terminator) {
    let bytes = text.as_bytes();
    let mut idx = start + 1;
    let mut segment_start = idx;
    // Most quoted fields can borrow from the source directly.
    // We only allocate this buffer when we need to materialize escapes or mixed segments.
    let mut scratch: Option<AVec<'a, u8>> = None;

    while idx < bytes.len() {
        if bytes[idx] != b'"' {
            idx += 1;
            continue;
        }

        if idx + 1 < bytes.len() && bytes[idx + 1] == b'"' {
            // CSV escape: "" inside quoted field becomes a literal quote.
            append_range(arena, text, &mut scratch, segment_start, idx);
            let buffer = scratch.get_or_insert_with(|| arena.new_vec_with_capacity(1));
            buffer.push(b'"');
            idx += 2;
            segment_start = idx;
            continue;
        }

        let quoted_end = idx;
        idx += 1;

        if idx >= bytes.len() {
            let value = materialize_field(text, scratch, segment_start, quoted_end);
            return (Entry::from_field(value), idx, Terminator::End);
        }

        match bytes[idx] {
            b',' => {
                let value = materialize_field(text, scratch, segment_start, quoted_end);
                return (Entry::from_field(value), idx + 1, Terminator::Comma);
            }
            b'\n' => {
                let value = materialize_field(text, scratch, segment_start, quoted_end);
                return (Entry::from_field(value), idx + 1, Terminator::Newline);
            }
            b'\r' => {
                let value = materialize_field(text, scratch, segment_start, quoted_end);
                let (next, terminator) = parse_terminator(bytes, idx);
                return (Entry::from_field(value), next, terminator);
            }
            _ => {
                // Be permissive with malformed quoted data: keep trailing text in the same field
                // instead of erroring, so callers can still inspect partially-valid CSV.
                append_range(arena, text, &mut scratch, segment_start, quoted_end);
                let tail_start = idx;
                while idx < bytes.len()
                    && bytes[idx] != b','
                    && bytes[idx] != b'\n'
                    && bytes[idx] != b'\r'
                {
                    idx += 1;
                }
                append_range(arena, text, &mut scratch, tail_start, idx);
                let value = materialize_field(text, scratch, 0, 0);
                let (next, terminator) = parse_terminator(bytes, idx);
                return (Entry::from_field(value), next, terminator);
            }
        }
    }

    let value = materialize_field(text, scratch, segment_start, bytes.len());
    (Entry::from_field(value), bytes.len(), Terminator::End)
}

fn parse_number(field: &str) -> f32 {
    if let Ok(x) = field.parse::<i32>() {
        return x as f32;
    }

    field.parse::<f32>().unwrap_or(0.)
}

fn parse_terminator(bytes: &[u8], idx: usize) -> (usize, Terminator) {
    if idx >= bytes.len() {
        return (idx, Terminator::End);
    }

    match bytes[idx] {
        b',' => (idx + 1, Terminator::Comma),
        b'\n' => (idx + 1, Terminator::Newline),
        b'\r' => {
            if idx + 1 < bytes.len() && bytes[idx + 1] == b'\n' {
                (idx + 2, Terminator::Newline)
            } else {
                (idx + 1, Terminator::Newline)
            }
        }
        _ => (idx, Terminator::End),
    }
}

fn append_range<'a>(
    arena: &'a Arena,
    text: &'a str,
    scratch: &mut Option<AVec<'a, u8>>,
    start: usize,
    end: usize,
) {
    if start >= end {
        return;
    }

    // Lazily creates the scratch buffer only for fields that cannot be borrowed as-is.
    let bytes = &text.as_bytes()[start..end];
    let buffer = scratch.get_or_insert_with(|| arena.new_vec_with_capacity(bytes.len()));
    buffer.extend_from_slice(bytes);
}

fn materialize_field<'a>(
    text: &'a str,
    scratch: Option<AVec<'a, u8>>,
    start: usize,
    end: usize,
) -> &'a str {
    match scratch {
        Some(mut buffer) => {
            // Field was rebuilt in scratch; finalize as arena-backed str.
            if start < end {
                buffer.extend_from_slice(&text.as_bytes()[start..end]);
            }
            std::str::from_utf8(buffer.into_bump_slice()).unwrap()
        }
        // Field never needed rewriting, so return a direct borrow from input.
        None => &text[start..end],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_basic_csv() {
        let arena = Arena::new();
        let table = parse(&arena, "name,age\nAlice,31\nBob,28");

        assert_eq!(table.len(), 3);
        assert_eq!(table.get(0).unwrap().get(0).unwrap().as_str(), "name");
        assert_eq!(table.get(1).unwrap().get(1).unwrap().as_num(), 31.0);
        assert_eq!(table.get(2).unwrap().get(1).unwrap().as_num(), 28.0);
    }

    #[test]
    fn parses_quotes_and_escapes() {
        let arena = Arena::new();
        let table = parse(&arena, "\"a,b\",\"x\"\"y\"\n\"hello\nworld\",42");

        assert_eq!(table.len(), 2);
        assert_eq!(table.get(0).unwrap().get(0).unwrap().as_str(), "a,b");
        assert_eq!(table.get(0).unwrap().get(1).unwrap().as_str(), "x\"y");
        assert_eq!(
            table.get(1).unwrap().get(0).unwrap().as_str(),
            "hello\nworld"
        );
        assert_eq!(table.get(1).unwrap().get(1).unwrap().as_num(), 42.0);
    }

    #[test]
    fn parses_escaped_quote_at_start() {
        let arena = Arena::new();
        let table = parse(&arena, "\"\"\"hello\"\"");

        assert_eq!(table.len(), 1);
        assert_eq!(table.get(0).unwrap().len(), 1);
        assert_eq!(table.get(0).unwrap().get(0).unwrap().as_str(), "\"hello\"");
    }

    #[test]
    fn parses_trailing_empty_fields() {
        let arena = Arena::new();
        let table = parse(&arena, "a,b,\n,");

        assert_eq!(table.len(), 2);
        assert_eq!(table.get(0).unwrap().len(), 3);
        assert_eq!(table.get(0).unwrap().get(2).unwrap().as_str(), "");
        assert_eq!(table.get(1).unwrap().len(), 2);
        assert_eq!(table.get(1).unwrap().get(0).unwrap().as_str(), "");
        assert_eq!(table.get(1).unwrap().get(1).unwrap().as_str(), "");
        assert_eq!(table.get(1).unwrap().get(1).unwrap().as_num(), 0.0);
    }

    #[test]
    fn parses_integers_and_floats_to_num() {
        let arena = Arena::new();
        let table = parse(&arena, "10,-3,2.5,foo");

        let row = table.get(0).unwrap();
        assert_eq!(row.get(0).unwrap().as_num(), 10.0);
        assert_eq!(row.get(1).unwrap().as_num(), -3.0);
        assert_eq!(row.get(2).unwrap().as_num(), 2.5);
        assert_eq!(row.get(3).unwrap().as_num(), 0.0);
    }
}
