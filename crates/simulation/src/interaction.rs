use bumpalo::Bump;
use slotmap::Key;
use util::string_pool::{SpanHandle, StringPool};

use crate::{
    Simulation,
    entities::{Entity, EntityId},
    objects::ObjectsBuilder,
};

#[derive(Default)]
pub(crate) struct Interactions {
    choice_defs: &'static [Choice<'static>],
    subject: EntityId,
    target: EntityId,
    state: String,
    // String pool for one reply
    string_pool: StringPool,
    description: SpanHandle,
    options: Vec<ChoiceOption>,
}

#[derive(Default, Clone, Copy)]
struct ChoiceOption {
    index: usize,
    text: SpanHandle,
}

const ENTRY_POINT: &'static str = "entry_point";
const OVER: &'static str = "";

type AVec<'a, T> = bumpalo::collections::Vec<'a, T>;

impl Interactions {
    pub fn new() -> Self {
        let mut this = Self::default();
        this.choice_defs = init_choices();
        this
    }

    pub(crate) fn has_interaction(&self) -> bool {
        !self.subject.is_null() || !self.target.is_null()
    }

    pub(crate) fn begin_interaction(
        &mut self,
        arena: &Bump,
        sim: &mut Simulation,
        subject: EntityId,
        target: EntityId,
    ) {
        self.subject = subject;
        self.target = target;
        self.state.clear();
        self.state.push_str(ENTRY_POINT);

        self.advance_interaction(arena, sim);
    }

    pub(crate) fn pick_choice(&mut self, arena: &Bump, sim: &mut Simulation, choice: usize) {
        if !self.has_interaction() {
            return;
        }

        let choice = self
            .options
            .get(choice)
            .map(|x| x.index)
            .unwrap_or_default();

        if let Some(choice) = self.choice_defs.get(choice) {
            let next_state = (choice.execute)(sim, arena, self.subject, self.target);
            self.state.clear();
            self.state.push_str(next_state);
        }

        if self.state.as_str() == OVER {
            self.mark_as_over();
        }

        // If the picked choice is out of bounds, we'll just advance the interaction
        // This will happen if say we have no valid choices
        std::mem::swap(&mut self.subject, &mut self.target);
        self.advance_interaction(arena, sim);
    }

    fn mark_as_over(&mut self) {
        self.state.clear();
        self.subject = EntityId::null();
        self.target = EntityId::null();
    }

    fn advance_interaction(&mut self, arena: &Bump, sim: &mut Simulation) {
        if !self.has_interaction() {
            return;
        }

        {
            // At first, the subject is not the player
            assert!(!sim.entities[self.subject].is_player);

            self.string_pool.clear();
            self.description = SpanHandle::default();
            self.options.clear();

            let first_valid_choice = self
                .get_valid_choices(arena, sim, self.subject, self.target, Speaker::NPC)
                .next();

            let (_, choice) = match first_valid_choice {
                Some(x) => x,
                None => {
                    self.mark_as_over();
                    return;
                }
            };

            let next_state = (choice.execute)(sim, arena, self.subject, self.target);
            self.state.clear();
            self.state.push_str(next_state);

            self.description = (choice.describe)(
                sim,
                arena,
                &sim.entities[self.subject],
                &sim.entities[self.target],
                &mut self.string_pool,
            );
        }

        std::mem::swap(&mut self.subject, &mut self.target);

        // Player side
        {
            // Then it must be the player's turn
            assert!(sim.entities[self.subject].is_player);

            let choices = {
                let iter =
                    self.get_valid_choices(arena, sim, self.subject, self.target, Speaker::Player);
                AVec::from_iter_in(iter, arena)
            };

            self.options.clear();
            self.options
                .extend(choices.into_iter().map(|(index, choice)| {
                    let text = (choice.describe)(
                        &sim,
                        arena,
                        &sim.entities[self.subject],
                        &sim.entities[self.target],
                        &mut self.string_pool,
                    );
                    ChoiceOption { index, text }
                }));
        }
    }

    fn get_valid_choices(
        &self,
        arena: &Bump,
        sim: &Simulation,
        subject: EntityId,
        target: EntityId,
        speaker_type: Speaker,
    ) -> impl Iterator<Item = (usize, &'static Choice<'static>)> {
        self.choice_defs
            .iter()
            .enumerate()
            .filter(move |(_, choice)| {
                choice.speaker == speaker_type
                    && choice.id.starts_with(&self.state)
                    && (choice.filter)(sim, arena, &sim.entities[subject], &sim.entities[target])
            })
    }

    pub(crate) fn as_object(&self, ctx: &mut ObjectsBuilder) {
        ctx.spawn(|ctx| {
            ctx.tag("interaction");
            ctx.str("description", self.string_pool.get(self.description));
            ctx.list("options", |ctx| {
                if self.options.is_empty() {
                    ctx.spawn(|ctx| {
                        ctx.str("text", "Ok");
                    });
                } else {
                    for option in &self.options {
                        ctx.spawn(|ctx| {
                            let text = self.string_pool.get(option.text);
                            ctx.str("text", text);
                        });
                    }
                }
            });
        });
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Speaker {
    Player,
    NPC,
}

struct Choice<'a> {
    id: &'a str,
    speaker: Speaker,
    filter: fn(&Simulation, &Bump, &Entity, &Entity) -> bool,
    execute: fn(&mut Simulation, &Bump, EntityId, EntityId) -> &'a str,
    describe: fn(&Simulation, &Bump, &Entity, &Entity, &mut StringPool) -> SpanHandle,
}

fn init_choices() -> &'static [Choice<'static>] {
    &[
        Choice {
            id: ENTRY_POINT,
            speaker: Speaker::NPC,
            filter: |_, _, _, _| true,
            execute: |_, _, _, _| "test",
            describe: |_, _, _, _, pool| pool.push_str("This is a test interaction"),
        },
        Choice {
            id: "test_a",
            speaker: Speaker::Player,
            filter: |_, _, _, _| true,
            execute: |_, _, _, _| OVER,
            describe: |_, _, _, _, pool| pool.push_str("Choice A"),
        },
        Choice {
            id: "test_a",
            speaker: Speaker::Player,
            filter: |_, _, _, _| true,
            execute: |_, _, _, _| "test_b_1",
            describe: |_, _, _, _, pool| pool.push_str("Choice B"),
        },
        Choice {
            id: "test_b_1",
            speaker: Speaker::NPC,
            filter: |_, _, _, _| true,
            execute: |_, _, _, _| "test_a",
            describe: |_, _, _, _, pool| pool.push_str("And then what?"),
        },
    ]
}
