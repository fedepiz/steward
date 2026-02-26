use crate::core::*;
use util::geom::*;

pub mod style {
    use super::*;

    pub const MARGIN: V2 = V2 { x: 5., y: 5. };
    pub const FILL: RGBA = RGBA::new(0.83, 0.69, 0.51, 1.);
    pub const DARK_FILL: RGBA = RGBA::new(0.3, 0.25, 0.18, 1.);
    pub const SHADOW: f32 = 1.2;
    pub const BORDER: RGBA = RGBA::new(0.5, 0.42, 0.31, 1.);
    pub const PULSE: f32 = 0.25;
    pub const ELEM_W: f32 = 45.;
    pub const ELEM_H: f32 = 45.;
}

use self::style::*;

pub struct GuiPlus<'a, 'b, 'c>(&'c mut Frame<'a, 'b>);

impl<'a, 'b, 'c> GuiPlus<'a, 'b, 'c> {
    pub fn wrap(frame: &'c mut Frame<'a, 'b>) -> Self {
        Self(frame)
    }

    pub fn inner(&mut self) -> &mut Frame<'a, 'b> {
        self.0
    }

    pub fn panel(&mut self, body: impl FnOnce(GuiPlus)) {
        self.0.widget(|gui| {
            gui.vertical_growing();
            gui.center_on_growth_axis();
            gui.fill(FILL);
            gui.stroke(BORDER, 4.);
            gui.pad(MARGIN * 4.);
            body(GuiPlus(gui));
        });
    }

    pub fn heading(&mut self, text: &'a str, width: f32) {
        self.0.widget(|gui| {
            gui.margin(MARGIN);
            gui.pixel_size(V2::new(ELEM_W * width, ELEM_H));
            gui.text(text, 36, RGBA::BLACK, [true, true]);
        });
    }

    pub fn button(&mut self, text: &'a str) -> bool {
        self.button_sized(text, 2., 1.)
    }

    pub fn button_sized(&mut self, text: &'a str, w: f32, h: f32) -> bool {
        self.0.widget(|gui| {
            gui.pixel_size(V2::new(ELEM_W * w, ELEM_H * h));
            gui.margin(MARGIN);

            gui.text(text, 22, RGBA::BLACK, [true, true]);
            gui.fingerprint_from_text();

            if gui.interaction().hovered && !gui.interaction().down {
                gui.pulse(PULSE);
            }

            gui.fill(if gui.interaction().down {
                DARK_FILL
            } else {
                FILL
            });
            gui.stroke(BORDER, 4.);
            gui.shadow(SHADOW);
            gui.interaction().clicked
        })
    }
}
