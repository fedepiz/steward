use gui::*;
use macroquad::prelude as mq;
use strum::{EnumCount, EnumIter};
use util::{
    arena::{AVec, Arena},
    geom::*,
};

use super::*;

#[derive(Default)]
pub(crate) struct UiData {
    pub selected_entity: ThingId,
    pub open_panels: [bool; Panel::COUNT],
    pub selected_message: ThingId,
}

impl UiData {
    pub fn is_panel_open(&self, panel: Panel) -> bool {
        self.open_panels[panel as usize]
    }

    pub fn toggle_panel(&mut self, panel: Panel) {
        self.open_panels[panel as usize] = !self.is_panel_open(panel);
    }
}

#[derive(Clone, Copy, EnumIter, EnumCount, Debug)]
pub enum Panel {
    Dummy,
    Messages,
}

impl Default for Panel {
    fn default() -> Self {
        Self::Dummy
    }
}

pub(super) fn root<'a>(
    gui: &'a mut Gui,
    arena: &'a Arena,
    things: &Things,
    data: &UiData,
    commands: &mut AVec<Command>,
) -> Output<'a> {
    let player = things.lookup_tag("player");

    gui.frame(
        arena,
        Input {
            screen_size: V2::new(mq::screen_width(), mq::screen_height()),
            mouse_pos: mq::mouse_position().into(),
            mouse_down: mq::is_mouse_button_down(mq::MouseButton::Left),
            mouse_pressed: mq::is_mouse_button_pressed(mq::MouseButton::Left),
        },
        |gui| {
            let mut gui = gui.plus();

            gui.panel(|mut gui| {
                gui.heading("Test Panel", 4.);

                if gui.button("Messages") {
                    commands.push(Command {
                        kind: CommandKind::TogglePanel,
                        panel: Panel::Messages,
                        ..Default::default()
                    });
                }

                if gui.button("Goodbye") {
                    println!("B");
                }

                if gui.button_sized("X##hello", 1.) {
                    println!("X")
                }
            });

            if !data.selected_entity.is_null() {
                let this = &things[data.selected_entity];
                gui.panel(|mut gui| {
                    gui.inner().center_on_growth_axis(false);
                    gui.inner().screen_pos(V2::new(0., 0.5));
                    gui.heading("Selected Entity", 6.);

                    let text = gui.arena().fmt(format_args!("Name: {}", this.name()));
                    gui.line_sized(text, 6.);

                    if this.flag(Flag::IsSettlement) {
                        gui.heading("Entities inside", 6.);

                        // Get entities inside the settlemetn
                        let things_inside = {
                            let iter = things
                                .iter_list(List::AtLocation, this.id())
                                .filter(|&id| id.get(&things).flag(Flag::IsInside))
                                .map(|x| x.get(things));
                            arena.alloc_slice_iter(iter)
                        };

                        // If we are empty
                        if things_inside.is_empty() {
                            gui.line_sized("Empty", 4.);
                        }

                        // For each entity inside
                        for (idx, thing) in things_inside.iter().enumerate() {
                            gui.row(|mut gui| {
                                let name = gui.arena().alloc_str(thing.name());
                                gui.line_sized(name, 4.);
                                if gui.button(
                                    gui.arena().fmt(format_args!("Select##sel_inside_{idx}")),
                                ) {
                                    commands.push(Command::with_thing(
                                        CommandKind::SetSelectedEntity,
                                        thing.id(),
                                    ));
                                }
                            });
                        }
                    }
                });
            }

            if data.is_panel_open(Panel::Messages) {
                gui.panel(|mut gui| {
                    gui.heading("Messages", 10.);

                    gui.inner().screen_pos(V2::new(1., 0.5));
                    gui.inner().center_on_growth_axis(false);

                    if let Some(msg_id) = data.selected_message.as_valid() {
                        let text = render_message(gui.arena(), things, msg_id);
                        gui.multiline(text, 10., 10.);
                        gui.row(|mut gui| {
                            if gui.button("Close") {
                                commands.push(Command::with_thing(
                                    CommandKind::SetSelectedMessage,
                                    ThingId::null(),
                                ));
                            }

                            if gui.button("Delete") {
                                commands.push(Command::with_thing(
                                    CommandKind::SetSelectedMessage,
                                    ThingId::null(),
                                ));
                                commands.push(Command::with_thing(CommandKind::Despawn, msg_id));
                            }
                        });
                    } else {
                        let messages = gui
                            .arena()
                            .alloc_slice_iter(things.iter_list(List::Messages, player));

                        for i in 0..10 {
                            if let Some(msg_id) = messages.get(i).copied() {
                                gui.row(|mut gui| {
                                    let text = render_message(gui.arena(), things, msg_id);
                                    gui.line_sized(text, 8.);

                                    let btn_text =
                                        gui.arena().fmt(format_args!("?##info_msg_{}", i));
                                    if gui.button_sized(btn_text, 1.) {
                                        commands.push(Command::with_thing(
                                            CommandKind::SetSelectedMessage,
                                            msg_id,
                                        ));
                                    }

                                    let btn_text =
                                        gui.arena().fmt(format_args!("X##del_msg_{}", i));
                                    if gui.button_sized(btn_text, 1.) {
                                        commands.push(Command::with_thing(
                                            CommandKind::Despawn,
                                            msg_id,
                                        ));
                                    }
                                });
                            } else {
                                gui.line_sized("", 10.);
                            }
                        }
                        gui.row(|mut gui| {
                            gui.label("Page 1/N");

                            if gui.button_sized("<", 1.) {}
                            if gui.button_sized(">", 1.) {}

                            if gui.button_sized("Delete All", 3.) {
                                commands.push(Command {
                                    kind: CommandKind::DespawnAllInList,
                                    thing: player,
                                    list: List::Messages,
                                    ..Default::default()
                                });
                            }
                        });
                    }
                });
            }
        },
    )
}
