use gui::*;
use macroquad::prelude as mq;
use util::{
    arena::{AVec, Arena},
    geom::*,
};

use super::*;

pub(super) fn root<'a>(
    gui: &'a mut Gui,
    arena: &'a Arena,
    sim: &Simulation,
    response: &Response,
    data: &UiData,
    commands: &mut AVec<Command>,
) -> Output<'a> {
    let player = sim.player();

    let things = &sim.things;

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
                gui.heading("Main Panel", 4.);

                if gui.button("Messages") {
                    commands.push(Command {
                        kind: CommandKind::TogglePanel,
                        panel: Panel::Messages,
                        ..Default::default()
                    });
                }

                if gui.button("Orders") {
                    commands.push(Command {
                        kind: CommandKind::TogglePanel,
                        panel: Panel::Orders,
                        ..Default::default()
                    });
                }
            });

            if let Some(this) = response.selected_entity.get_as_valid(&sim.things) {
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

            if data.is_panel_open(Panel::Orders) {
                gui.panel(|mut gui| {
                    gui.heading("Orders", 10.);
                    gui.inner().screen_pos(V2::new(1., 0.5));
                    gui.inner().center_on_growth_axis(false);

                    match response.selected_entity.get_as_valid(things) {
                        Some(this) => {
                            {
                                let name =
                                    gui.arena().fmt(format_args!("Orders for: {}", this.name()));
                                gui.line_sized(name, 10.);
                            }

                            let order = this.link(Link::Order).get_as_valid(things);
                            let order_name = order
                                .map(|x| render_order_name(gui.arena(), things, x))
                                .unwrap_or("No order");
                            gui.line_sized(order_name, 10.);
                        }
                        None => {
                            gui.line_sized("No entity selected...", 10.);
                        }
                    }
                });
            }

            // The message panel
            if data.is_panel_open(Panel::Messages) {
                gui.panel(|mut gui| {
                    gui.heading("Messages", 10.);

                    gui.inner().screen_pos(V2::new(1., 0.5));
                    gui.inner().center_on_growth_axis(false);

                    // If we have a message selected, we are gonna show the blow-up version of the message.
                    if let Some((msg_id, text)) = response.messages.expanded {
                        gui.multiline(
                            gui.arena().alloc_str(text),
                            10.,
                            UiData::NUM_MESSAGE_PER_PAGE as f32,
                        );
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
                        // Otherwise, show the messages on the selected page

                        for i in 0..UiData::NUM_MESSAGE_PER_PAGE {
                            if let Some(&(msg_id, text)) = response.messages.list.get(i) {
                                gui.row(|mut gui| {
                                    // Each message has text...
                                    gui.line_sized(gui.arena().alloc_str(text), 8.);

                                    // ... a button for expanding the message...
                                    let btn_text =
                                        gui.arena().fmt(format_args!("?##info_msg_{}", i));
                                    if gui.button_sized(btn_text, 1.) {
                                        commands.push(Command::with_thing(
                                            CommandKind::SetSelectedMessage,
                                            msg_id,
                                        ));
                                    }
                                    // ... and a button for deleting the message.
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
                                // Otherwise we have just a blank line
                                gui.line_sized("", 0.);
                            }
                        }

                        // This is the footer for when we are looking at the message list.
                        gui.row(|mut gui| {
                            // Page counter
                            let text = gui.arena().fmt(format_args!(
                                "{}/{}##msg_page_counter",
                                response.messages.current_page, response.messages.number_of_pages
                            ));
                            gui.label(text);

                            // Page selectors
                            if gui.button_sized("<##msg_page_back", 1.) {
                                commands
                                    .push(Command::with_num(CommandKind::ChangeMessagePage, -1.));
                            }
                            if gui.button_sized(">##msg_page_next", 1.) {
                                commands
                                    .push(Command::with_num(CommandKind::ChangeMessagePage, 1.));
                            }

                            // And a button to clear all messages
                            if gui.button_sized("Delete All##msg_delete_all", 3.) {
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
