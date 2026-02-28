use crate::things::ThingId;
use util::{
    arena::{AVec, Arena},
    geom::*,
};

#[derive(Clone, Copy, Default)]
pub(crate) struct Path {
    pub start: V2,
    pub end: V2,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Sprite {
    pub image: &'static str,
    pub bounds: Rect,
    pub layer: u8,
    pub border_highlight: bool,
    pub pulse_intensity: f32,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Label<'a> {
    pub text: &'a str,
    pub pos: V2,
    pub font_size: u16,
    pub highighted: bool,
    pub layer: u8,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Clickbox {
    pub id: ThingId,
    pub bounds: Rect,
}

pub(crate) struct DrawData<'a> {
    pub paths: AVec<'a, Path>,
    pub sprites: AVec<'a, Sprite>,
    pub labels: AVec<'a, Label<'a>>,
    pub clickboxes: AVec<'a, Clickbox>,
}

impl<'a> DrawData<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        Self {
            paths: arena.new_vec_with_capacity(1000),
            sprites: arena.new_vec_with_capacity(1000),
            labels: arena.new_vec_with_capacity(1000),
            clickboxes: arena.new_vec_with_capacity(1000),
        }
    }

    pub fn prepare(&mut self) {
        self.sprites.sort_by_key(|x| x.layer);
    }
}
