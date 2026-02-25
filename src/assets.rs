use macroquad::prelude as mq;
use std::collections::HashMap;
use util::arena::Arena;

use crate::csv;

pub(crate) struct TextureAtlas<'a> {
    texture: mq::Texture2D,
    rects: HashMap<&'a str, mq::Rect>,
}

impl<'a> TextureAtlas<'a> {
    pub fn texture(&self) -> &mq::Texture2D {
        &self.texture
    }

    pub fn get(&self, name: &str) -> mq::Rect {
        self.rects.get(name).copied().unwrap_or_default()
    }
}

pub(crate) async fn load_texture_atlas<'a>(
    arena: &'a Arena,
    scratch: &Arena,
    path: impl AsRef<std::path::Path>,
) -> TextureAtlas<'a> {
    let path = path.as_ref();
    let mut rects = HashMap::new();
    let source_text = std::fs::read_to_string(path.join("atlas.csv")).unwrap_or_default();
    let table = csv::parse(scratch, &source_text);
    for row in table.rows() {
        let name = arena.alloc_str(row[0].as_str());
        let x = row[1].as_num();
        let y = row[2].as_num();
        let w = row[3].as_num();
        let h = row[4].as_num();
        rects.insert(name, mq::Rect::new(x, y, w, h));
    }

    let texture = mq::load_texture(path.join("atlas.png").as_os_str().to_str().unwrap())
        .await
        .unwrap();

    TextureAtlas { texture, rects }
}
