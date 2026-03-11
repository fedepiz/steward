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
    request: &mut Request,
    data: &UiData,
    commands: &mut AVec<Command>,
) -> Output<'a> {
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

                if gui.button_sized("Messages", 3.) {
                    commands.push(Command {
                        kind: CommandKind::TogglePanel,
                        panel: Panel::Messages,
                        ..Default::default()
                    });
                }

                if gui.button_sized("Orders", 3.) {
                    commands.push(Command {
                        kind: CommandKind::TogglePanel,
                        panel: Panel::Orders,
                        ..Default::default()
                    });
                }

                if gui.button_sized("Communicate", 3.) {
                    commands.push(Command {
                        kind: CommandKind::TogglePanel,
                        panel: Panel::Communications,
                        ..Default::default()
                    });
                }
            });

            if let Some(this) = response.selected_entity.id.get_as_valid(&sim.things) {
                let selected_entity = &response.selected_entity;

                gui.panel(|mut gui| {
                    gui.inner().center_on_growth_axis(false);
                    gui.inner().screen_pos(V2::new(0., 0.5));

                    let text = gui.arena().fmt(format_args!(
                        "Selected Entity $sprite${}",
                        selected_entity.sprite
                    ));
                    gui.heading(text, 8.);

                    gui.row(|mut gui| {
                        gui.line_sized("Name:", 2.);
                        let text = gui
                            .arena()
                            .fmt(format_args!("{}##entity_name", selected_entity.name));
                        gui.label_sized(text, 4.);
                    });

                    let selectable_entity =
                        |gui: &mut GuiPlus,
                         request: &mut Request,
                         title: &'static str,
                         entity: ThingId| {
                            gui.row(|mut gui| {
                                gui.line_sized(title, 2.);
                                let entity = entity.get_as_valid(things);
                                let name = entity.map(|x| x.name).unwrap_or("Vacant");
                                if gui.button_generic(
                                    gui.arena().alloc_str(name),
                                    4.,
                                    entity.is_some(),
                                ) {
                                    request.select_entity =
                                        entity.map(|x| x.id()).unwrap_or_default();
                                }
                            });
                        };

                    if this.flag(Flag::IsPerson) {
                        if let Some(leader) = this.parent(List::Subordinates).as_valid() {
                            selectable_entity(&mut gui, request, "Leader:", leader);
                        }

                        if let Some(location) = things
                            .iter_list_get(List::Possessions, this.id())
                            .find(|x| x.flag(Flag::IsLocation))
                        {
                            selectable_entity(&mut gui, request, "Base:", location.id());
                        }
                    }

                    if this.flag(Flag::IsSettlement) {
                        selectable_entity(
                            &mut gui,
                            request,
                            "Leader:",
                            this.parent(List::Possessions),
                        );
                    }

                    if selected_entity.show_partecipants {
                        gui.heading("Partecipants", 6.);
                        for &(id, name) in &selected_entity.partecipants {
                            gui.row(|mut gui| {
                                let name = gui.arena().alloc_str(name);
                                if gui.button_generic(gui.arena().alloc_str(name), 4., true) {
                                    request.select_entity = id;
                                }
                            });
                        }
                    }

                    if this.flag(Flag::IsSettlement) {
                        gui.heading("Entities inside", 6.);
                        // Get entities inside the settlemetn
                        let things_inside = {
                            let iter = things
                                .iter_list_get(List::AtLocation, this.id())
                                .filter(|x| x.flag(Flag::IsInside));
                            arena.alloc_slice_iter(iter)
                        };

                        // If we are empty
                        if things_inside.is_empty() {
                            gui.line_sized("Empty", 4.);
                        }

                        // For each entity inside
                        for (idx, thing) in things_inside.iter().enumerate() {
                            gui.row(|mut gui| {
                                let name = gui
                                    .arena()
                                    .fmt(format_args!("{}##sel_inside_{idx}", thing.name));
                                if gui.button_sized(name, 4.) {
                                    request.select_entity = thing.id();
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

                    let info = &response.order;

                    match response.selected_entity.id.get_as_valid(things) {
                        Some(this) => {
                            {
                                let name =
                                    gui.arena().fmt(format_args!("Orders for: {}", this.name));
                                gui.line_sized(name, 10.);
                            }

                            {
                                let text = gui.arena().alloc_str(info.name);
                                gui.line_sized(text, 10.);
                            }
                        }
                        None => {
                            gui.line_sized("No entity selected...", 10.);
                        }
                    }
                });
            }

            if data.is_panel_open(Panel::Communications) {
                gui.panel(|mut gui| {
                    gui.heading("Communication", 10.);

                    gui.inner().screen_pos(V2::new(1., 0.5));
                    gui.inner().center_on_growth_axis(false);

                    let info = &response.communication;

                    let mut remove_piece_at_index = None;
                    if response.selected_entity.id.is_valid() {
                        {
                            let name = gui
                                .arena()
                                .fmt(format_args!("To: {}", response.selected_entity.name));
                            gui.line_sized(name, 10.);
                        }

                        for (idx, piece) in info.enqueued_pieces.iter().enumerate() {
                            gui.row(|mut gui| {
                                let text = gui.arena().alloc_str(piece.name);
                                gui.line_sized(text, 9.);
                                let text = gui.arena().fmt(format_args!("X##del_comm_{idx}"));
                                if gui.button_sized(text, 1.) {
                                    remove_piece_at_index = Some(idx);
                                }
                            });
                        }

                        if let Some((_, name)) = info.selected_option {
                            let text = gui.arena().alloc_str(name);
                            gui.line_sized(text, 10.);
                        } else {
                            gui.row(|mut gui| {
                                for &(idx, name) in info.options {
                                    let text = gui.arena().alloc_str(name);
                                    if gui.button(text) {
                                        request.communication.selected_option = Some(idx);
                                    }
                                }
                            });
                        }
                        gui.row(|mut gui| {
                            if gui.button_generic("Send", 2., info.ready_to_send) {
                                request.communication.send = true;
                            }
                        });
                    } else {
                        gui.line_sized("No entity selected...", 10.);
                    }

                    if let Some(idx) = remove_piece_at_index {
                        request.communication.enqueued_pieces.remove(idx);
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
                                request.despawns.push(msg_id);
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
                                        request.despawns.push(msg_id);
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
                                request.messages.delete_all = true;
                            }
                        });
                    }
                });
            }
        },
    )
}
