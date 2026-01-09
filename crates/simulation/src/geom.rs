#[derive(Default, Clone, Copy, PartialEq, PartialOrd, Debug)]
pub(crate) struct V2 {
    pub x: f32,
    pub y: f32,
}

impl V2 {
    pub(crate) fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    pub(crate) fn splat(v: f32) -> Self {
        Self { x: v, y: v }
    }
}
