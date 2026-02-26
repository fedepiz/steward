use std::collections::HashMap;

use util::{
    arena::{AVec, Arena},
    geom::*,
};

type Fingerprint = u64;

#[derive(Default)]
pub(crate) struct Gui {
    active_cache: HashMap<Fingerprint, Interaction>,
    passive_cache: HashMap<Fingerprint, Interaction>,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Input {
    pub screen_size: V2,
    pub mouse_pos: V2,
    pub mouse_down: bool,
}

pub(crate) struct Output<'a> {
    pub draw_list: &'a [Draw],
    pub is_mouse_over_ui: bool,
}

impl Gui {
    pub fn frame<'a>(
        &mut self,
        arena: &'a Arena,
        input: Input,
        build: impl for<'b> FnOnce(&mut Frame<'a, 'b>),
    ) -> Output<'a> {
        let (widgets, draw_list) = {
            let mut frame = Frame::new(arena, &self.active_cache);
            build(&mut frame);
            frame.draw(input.screen_size)
        };

        // Update widget cache status
        self.passive_cache.clear();

        // Going backwards over widget
        let mut is_mouse_over_ui = false;
        for widget in widgets.iter().rev() {
            let prev_interaction = self
                .active_cache
                .get(&widget.fingerprint)
                .copied()
                .unwrap_or_default();

            let mut interaction = Interaction::default();
            if !is_mouse_over_ui && widget.bounds.contains(input.mouse_pos) {
                is_mouse_over_ui = true;

                interaction.hovered = true;
                interaction.clicked = input.mouse_down && !prev_interaction.pressed;
                interaction.pressed = input.mouse_down;
            }

            if widget.fingerprint != 0 {
                self.passive_cache.insert(widget.fingerprint, interaction);
            }
        }

        // Flip active and passive cache
        std::mem::swap(&mut self.active_cache, &mut self.passive_cache);

        Output {
            draw_list,
            is_mouse_over_ui,
        }
    }
}

#[derive(Default, Clone, Copy)]
pub struct Interaction {
    pub hovered: bool,
    pub clicked: bool,
    pub pressed: bool,
}

type WidgetId = usize;

#[derive(Default, Clone, Copy)]
pub struct RGBA {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl RGBA {
    pub const RED: RGBA = RGBA {
        r: 1.,
        g: 0.,
        b: 0.,
        a: 1.,
    };

    pub const GREEN: RGBA = RGBA {
        r: 0.,
        g: 1.,
        b: 0.,
        a: 1.,
    };

    pub const BLUE: RGBA = RGBA {
        r: 0.,
        g: 0.,
        b: 1.,
        a: 1.,
    };
}

#[derive(Default)]
struct Widget<'a> {
    id: WidgetId,
    fingerprint: u64,
    logical_size: [LogicalSize; 2],
    margin: V2,
    padding: V2,
    screen_offset: V2,
    growth_axes: V2,
    bounds: Rect,
    fill: RGBA,
    stroke: (RGBA, f32),
    parent: WidgetId,
    children: &'a [WidgetId],
}

#[derive(Default, Clone, Copy)]
struct ActiveWidget {
    id: WidgetId,
    children_begin_offset: usize,
}

pub(crate) struct Frame<'a, 'b> {
    arena: &'a Arena,
    cache: &'b HashMap<Fingerprint, Interaction>,
    widgets: AVec<'a, Widget<'a>>,
    children_stack: AVec<'a, WidgetId>,
    active_stack: AVec<'a, ActiveWidget>,
}

impl<'a, 'b> Frame<'a, 'b> {
    fn new(arena: &'a Arena, cache: &'b HashMap<Fingerprint, Interaction>) -> Self {
        let mut widgets = arena.new_vec_with_capacity(100);
        widgets.push(Widget::default());
        Self {
            arena,
            cache,
            widgets,
            children_stack: arena.new_vec_with_capacity(20),
            active_stack: arena.new_vec_with_capacity(20),
        }
    }

    pub fn widget(&mut self, body: impl FnOnce(&mut Self)) {
        self.start_widget();
        body(self);
        self.end_widget();
    }

    fn start_widget(&mut self) {
        let id = self.widgets.len();
        let widget = Widget {
            id,
            ..Default::default()
        };
        self.widgets.push(widget);
        self.active_stack.push(ActiveWidget {
            id,
            children_begin_offset: self.children_stack.len(),
        });
    }

    fn end_widget(&mut self) {
        if let Some(active_widget) = self.active_stack.pop() {
            let widget = &mut self.widgets[active_widget.id];
            // Extract the list of children
            widget.children = {
                let slice = &self.children_stack[active_widget.children_begin_offset..];
                self.arena.alloc_slice_copy(slice)
            };
            // Pop off elements
            self.children_stack.resize(
                active_widget
                    .children_begin_offset
                    .min(self.children_stack.len()),
                0,
            );
            // Push back the id of the current widget
            self.children_stack.push(widget.id);
        }
    }

    fn current_widget(&self) -> &Widget<'a> {
        match self.active_stack.last() {
            Some(active) => &self.widgets[active.id],
            None => {
                debug_assert!(false);
                // allocate some trash
                self.arena.alloc_default()
            }
        }
    }

    fn current_widget_mut(&mut self) -> &mut Widget<'a> {
        match self.active_stack.last() {
            Some(active) => &mut self.widgets[active.id],
            None => {
                debug_assert!(false);
                // allocate some trash
                self.arena.alloc_default()
            }
        }
    }

    fn draw(mut self, screen_size: V2) -> (&'a [Widget<'a>], &'a [Draw]) {
        self.layout(screen_size);
        let mut draw_list = self.arena.new_vec_with_capacity(self.widgets.len());
        for widget in &self.widgets {
            if widget.bounds.w > 0. && widget.bounds.h > 0. {
                draw_list.push(Draw {
                    bounds: widget.bounds,
                    fill: widget.fill,
                    stroke: widget.stroke,
                });
            }
        }
        (self.widgets.into_bump_slice(), draw_list.into_bump_slice())
    }

    fn layout(&mut self, screen_size: V2) {
        // Sizing pass
        let mut computed_sizes = vec![V2::default(); self.widgets.len()];
        for axis in 0..=1 {
            // Internal-sized pass
            for widget in self.widgets.iter_mut() {
                let logical_size = widget.logical_size[axis];
                let base = match logical_size.kind {
                    LogicalSizeKind::Pixels => logical_size.value,
                    _ => 0.,
                };
                computed_sizes[widget.id][axis] = base + widget.padding[axis] * 2.0;
            }

            // Upwards propagation (child -> parent)
            for widget in self.widgets.iter_mut().rev() {
                let logical_size = widget.logical_size[axis];
                let mut change = 0.;
                match logical_size.kind {
                    LogicalSizeKind::ChildSum => {
                        for &child in widget.children {
                            change += computed_sizes[child][axis];
                        }
                    }
                    LogicalSizeKind::ChildMax => {
                        for &child in widget.children {
                            change = computed_sizes[child][axis].max(change);
                        }
                    }
                    _ => {}
                }
                computed_sizes[widget.id][axis] += change;
            }
        }
        // Final step: write sizes
        for widget in self.widgets.iter_mut() {
            let size = computed_sizes[widget.id];
            widget.bounds.w = size.x;
            widget.bounds.h = size.y;
        }

        // Placement
        let mut positions = vec![V2::default(); self.widgets.len()];
        for widget in &self.widgets {
            // calculate screen offset
            positions[widget.id].x +=
                widget.screen_offset.x * (screen_size.x - widget.bounds.w) / 2.;
            positions[widget.id].y +=
                widget.screen_offset.y * (screen_size.y - widget.bounds.h) / 2.;

            // Place children
            let mut cursor = positions[widget.id] + widget.padding;
            for &child in widget.children {
                positions[child] += cursor;
                cursor += self.widgets[child].bounds.size() * widget.growth_axes;
            }
        }

        for widget in &mut self.widgets {
            widget.bounds = widget.bounds.with_position(positions[widget.id]);
            // Usa safe margin by making sure we can't shrink more thne our size
            let margin = widget.margin.min(widget.bounds.size());
            // Shrink all the bounds to apply 'margins'.
            widget.bounds.x += margin.x;
            widget.bounds.y += margin.y;
            widget.bounds.w -= margin.x * 2.;
            widget.bounds.h -= margin.y * 2.;
        }
    }

    pub fn interaction(&self) -> Interaction {
        let fingerprint = self.current_widget().fingerprint;
        self.cache.get(&fingerprint).copied().unwrap_or_default()
    }

    pub fn fill(&mut self, color: RGBA) {
        self.current_widget_mut().fill = color;
    }

    pub fn stroke(&mut self, color: RGBA, thickness: f32) {
        self.current_widget_mut().stroke = (color, thickness);
    }

    pub fn pixel_size(&mut self, size: V2) {
        let widget = self.current_widget_mut();
        widget.logical_size[0].kind = LogicalSizeKind::Pixels;
        widget.logical_size[0].value = size.x;
        widget.logical_size[1].kind = LogicalSizeKind::Pixels;
        widget.logical_size[1].value = size.y;
    }

    pub fn pad(&mut self, size: V2) {
        self.current_widget_mut().padding = size;
    }

    pub fn margin(&mut self, size: V2) {
        self.current_widget_mut().margin = size;
    }

    pub fn vertical_growing(&mut self) {
        let widget = self.current_widget_mut();
        widget.growth_axes = V2::new(0., 1.);
        widget.logical_size[0].kind = LogicalSizeKind::ChildMax;
        widget.logical_size[1].kind = LogicalSizeKind::ChildSum;
    }

    pub fn fingerprint(&mut self, fingerprint: u64) {
        self.current_widget_mut().fingerprint = fingerprint;
    }
}

#[derive(Clone, Copy, Default)]
struct LogicalSize {
    kind: LogicalSizeKind,
    value: f32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum LogicalSizeKind {
    Pixels,
    ChildSum,
    ChildMax,
}

impl Default for LogicalSizeKind {
    fn default() -> Self {
        Self::Pixels
    }
}

#[derive(Clone, Copy)]
pub struct Draw {
    pub bounds: Rect,
    pub fill: RGBA,
    pub stroke: (RGBA, f32),
}
