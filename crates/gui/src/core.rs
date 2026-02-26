use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};

use util::{
    arena::{AVec, Arena},
    geom::*,
};

use crate::GuiPlus;

type Fingerprint = u64;

#[derive(Default)]
pub struct Gui {
    active_cache: HashMap<Fingerprint, Interaction>,
    passive_cache: HashMap<Fingerprint, Interaction>,
}

#[derive(Clone, Copy)]
pub struct Input {
    pub screen_size: V2,
    pub mouse_pos: V2,
    pub mouse_down: bool,
    pub mouse_pressed: bool,
}

pub struct Output<'a> {
    pub draw_list: &'a [Draw<'a>],
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
                interaction.clicked = input.mouse_pressed && !prev_interaction.down;
                interaction.down =
                    input.mouse_down && (prev_interaction.clicked || prev_interaction.down);
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
    pub down: bool,
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
    pub const fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }

    pub const BLACK: RGBA = RGBA::new(0., 0., 0., 1.);
    pub const WHITE: RGBA = RGBA::new(1., 1., 1., 1.);

    pub const RED: RGBA = RGBA::new(1., 0., 0., 1.);
    pub const GREEN: RGBA = RGBA::new(0., 1., 0., 1.);
    pub const BLUE: RGBA = RGBA::new(0., 0., 1., 1.);
}

#[derive(Default, Clone, Copy)]
pub struct Text<'a> {
    pub string: &'a str,
    pub size: u16,
    pub centering: [bool; 2],
    pub color: RGBA,
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
    children: &'a [WidgetId],
    center_children: [bool; 2],
    grow_to_fill: [bool; 2],
    bounds: Rect,
    text: Text<'a>,
    fill: RGBA,
    stroke: (RGBA, f32),
    shadow: f32,
    pulse: f32,
}

#[derive(Default, Clone, Copy)]
struct ActiveWidget {
    id: WidgetId,
    children_begin_offset: usize,
}

pub struct Frame<'a, 'b> {
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

    pub fn plus<'c>(&'c mut self) -> GuiPlus<'a, 'b, 'c> {
        GuiPlus::wrap(self)
    }

    pub fn widget<R>(&mut self, body: impl FnOnce(&mut Self) -> R) -> R {
        self.start_widget();
        let r = body(self);
        self.end_widget();
        r
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

    fn draw(mut self, screen_size: V2) -> (&'a [Widget<'a>], &'a [Draw<'a>]) {
        let widths = self.calculate_sizes(0);
        let heights = self.calculate_sizes(1);

        for widget in self.widgets.iter_mut() {
            widget.bounds.w = widths[widget.id];
            widget.bounds.h = heights[widget.id];
        }

        self.calculate_positions(screen_size);

        let mut draw_list = self.arena.new_vec_with_capacity(self.widgets.len());
        for widget in &self.widgets {
            if widget.bounds.w > 0. && widget.bounds.h > 0. {
                let mut text = widget.text;
                // Shrink the text to only include 'visible' text
                text.string = text.string.split("##").next().unwrap_or_default();
                draw_list.push(Draw {
                    bounds: widget.bounds,
                    fill: widget.fill,
                    stroke: widget.stroke,
                    text,
                    shadow: widget.shadow,
                    pulse: widget.pulse,
                });
            }
        }
        (self.widgets.into_bump_slice(), draw_list.into_bump_slice())
    }

    fn calculate_sizes(&self, axis: usize) -> Vec<f32> {
        assert!(axis <= 1);
        let mut sizes = vec![0f32; self.widgets.len()];
        // Upwards propagation (child -> parent)
        for widget in self.widgets.iter().rev() {
            let logical_size = widget.logical_size[axis];

            // We are going to iterate over the children anyways to find out if any are in 'grow' mode,
            // so may as well simplify our life and pull all the looping in here
            let mut children_total = 0.0;
            let mut children_max = 0.0;
            let mut num_growable = 0;
            for &child in widget.children {
                let child_size = sizes[child];
                children_total += child_size;
                children_max = child_size.max(children_max);
                if self.widgets[child].grow_to_fill[axis] {
                    num_growable += 1;
                }
            }

            // Get the base size depending on our logical size type
            let base = match logical_size.kind {
                LogicalSizeKind::Pixels => logical_size.value,
                LogicalSizeKind::ChildSum => children_total,
                LogicalSizeKind::ChildMax => children_max,
            };
            // Calculate our actaul size
            let my_size = base + widget.padding[axis] * 2.;
            let mut space_to_fill = base - children_total;

            // If some of my children want to grow, and I have leftover space, let me grow them
            while num_growable > 0 && space_to_fill > 0. {
                let mut smallest_size = std::f32::INFINITY;
                let mut second_smallest_size = std::f32::INFINITY;

                // Find the smallest and second-smallest sizes
                for &child in widget.children {
                    // Consider only growable
                    if !self.widgets[child].grow_to_fill[axis] {
                        continue;
                    }
                    let child_size = sizes[child];
                    smallest_size = child_size.min(smallest_size);
                    if child_size > smallest_size {
                        second_smallest_size = child_size.min(second_smallest_size);
                    }
                }

                // The growth amount is the smallest of
                // 1. The gap between the second smallest child and this child
                // 2. The amount of gap left, equally distributed amongst all children
                let grow_amount =
                    (second_smallest_size - smallest_size).min(space_to_fill / num_growable as f32);

                // Add grwoth amount to each child
                for &child in widget.children {
                    // Consider only growable
                    if !self.widgets[child].grow_to_fill[axis] {
                        continue;
                    }
                    let child_size = &mut sizes[child];
                    if *child_size == smallest_size {
                        *child_size += grow_amount;
                        space_to_fill -= grow_amount;
                    }
                }
            }

            // Finally confirm my size
            sizes[widget.id] = my_size;
        }
        sizes
    }

    fn calculate_positions(&mut self, screen_size: V2) {
        let mut positions = vec![V2::default(); self.widgets.len()];
        for widget in &self.widgets {
            // calculate screen offset
            positions[widget.id].x +=
                widget.screen_offset.x * (screen_size.x - widget.bounds.w) / 2.;
            positions[widget.id].y +=
                widget.screen_offset.y * (screen_size.y - widget.bounds.h) / 2.;

            // Place children
            let unpadded_size = widget.bounds.size() - widget.padding * 2.;
            let mut cursor = positions[widget.id] + widget.padding;
            for &child_id in widget.children {
                let child = &self.widgets[child_id];
                positions[child_id] += cursor;

                if widget.center_children[0] {
                    positions[child_id].x =
                        cursor.x + (unpadded_size.x - child.bounds.w).max(0.) / 2.;
                }
                if widget.center_children[1] {
                    positions[child_id].y =
                        cursor.y + (unpadded_size.y - child.bounds.h).max(0.) / 2.;
                }
                cursor += child.bounds.size() * widget.growth_axes;
            }
        }

        for widget in &mut self.widgets {
            widget.bounds = widget.bounds.with_position(positions[widget.id]);
            // Usa safe margin by making sure we can't shrink more thne our size
            let shrink = (widget.margin * 2.).min(widget.bounds.size());
            // Shrink all the bounds to apply 'margins'.
            widget.bounds.x += shrink.x / 2.;
            widget.bounds.y += shrink.y / 2.;
            widget.bounds.w -= shrink.x;
            widget.bounds.h -= shrink.y;
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

    pub fn shadow(&mut self, value: f32) {
        self.current_widget_mut().shadow = value;
    }

    pub fn pulse(&mut self, value: f32) {
        self.current_widget_mut().pulse = value;
    }

    pub fn text(&mut self, string: &'a str, size: u16, color: RGBA, centering: [bool; 2]) {
        self.current_widget_mut().text = Text {
            string,
            size,
            centering,
            color,
        };
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
        let widget = self.current_widget_mut();
        // The padding needs to be adjusted, as the padding is "net" of the margin
        widget.padding -= widget.margin;
        widget.margin = size;
        widget.padding += widget.margin;
    }

    pub fn grow_to_fill(&mut self, horizontal: bool, vertical: bool) {
        self.current_widget_mut().grow_to_fill = [horizontal, vertical];
    }

    pub fn vertical_growing(&mut self) {
        let widget = self.current_widget_mut();
        widget.growth_axes = V2::new(0., 1.);
        widget.logical_size[0].kind = LogicalSizeKind::ChildMax;
        widget.logical_size[1].kind = LogicalSizeKind::ChildSum;
    }

    pub fn horizontal_growing(&mut self) {
        let widget = self.current_widget_mut();
        widget.growth_axes = V2::new(1., 0.);
        widget.logical_size[0].kind = LogicalSizeKind::ChildSum;
        widget.logical_size[1].kind = LogicalSizeKind::ChildMax;
    }

    pub fn center_on_growth_axis(&mut self) {
        let widget = self.current_widget_mut();
        widget.center_children = [widget.growth_axes.x == 0., widget.growth_axes.y == 0.];
    }

    pub fn fingerprint(&mut self, fingerprint: u64) {
        self.current_widget_mut().fingerprint = fingerprint;
    }

    pub fn fingerprint_from_text(&mut self) {
        let widget = self.current_widget_mut();
        widget.fingerprint = {
            let mut hasher = DefaultHasher::new();
            widget.text.string.hash(&mut hasher);
            hasher.finish()
        };
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
pub struct Draw<'a> {
    pub bounds: Rect,
    pub fill: RGBA,
    pub stroke: (RGBA, f32),
    pub text: Text<'a>,
    pub shadow: f32,
    pub pulse: f32,
}
