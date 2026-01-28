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
        let choice = self
            .options
            .get(choice)
            .map(|x| x.index)
            .unwrap_or_default();

        if let Some(choice) = self.choice_defs.get(choice) {
            let (next_state, speaker) = (choice.execute)(sim, arena, self.subject, self.target);

            self.state.clear();
            self.state.push_str(next_state);
            if matches!(speaker, Speaker::Flip) {
                std::mem::swap(&mut self.subject, &mut self.target);
            }
        }
        // If the picked choice is out of bounds, we'll just advance the interaction
        // This will happen if say we have no valid choices
        self.advance_interaction(arena, sim);
    }

    fn advance_interaction(&mut self, arena: &Bump, sim: &mut Simulation) {
        if !self.has_interaction() {
            return;
        }
        let is_player = sim.entities[self.subject].is_player;

        let max_choices = if is_player { usize::MAX } else { 1 };
        let valid_choices = self
            .choice_defs
            .iter()
            .enumerate()
            .filter(|(_, choice)| {
                choice.id.starts_with(&self.state)
                    && (choice.filter)(
                        &sim,
                        arena,
                        &sim.entities[self.subject],
                        &sim.entities[self.target],
                    )
            })
            .take(max_choices);
        let valid_choices = AVec::from_iter_in(valid_choices, arena);

        if valid_choices.is_empty() {
            self.subject = EntityId::null();
            self.target = EntityId::null();
            return;
        }

        if !is_player {
            // Reset the output
            self.string_pool.clear();
            self.description = SpanHandle::default();
            self.options.clear();
            self.state.clear();

            // SAFETY: We have a valid_choice check earlier against empty
            let (_, choice) = valid_choices.get(0).unwrap();
            let (next_state, speaker) = (choice.execute)(sim, arena, self.subject, self.target);

            self.description = (choice.describe)(
                &sim,
                arena,
                &sim.entities[self.subject],
                &sim.entities[self.target],
                &mut self.string_pool,
            );
            self.state.push_str(next_state);
            if matches!(speaker, Speaker::Flip) {
                std::mem::swap(&mut self.subject, &mut self.target);
            }
        } else {
            self.options.clear();
            self.options
                .extend(valid_choices.into_iter().map(|(index, choice)| {
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
                        let text = self.string_pool.get(option.text);
                        ctx.str("text", text);
                    }
                }
            });
        });
    }
}

#[derive(Clone, Copy)]
enum Speaker {
    Keep,
    Flip,
}

struct Choice<'a> {
    id: &'a str,
    filter: fn(&Simulation, &Bump, &Entity, &Entity) -> bool,
    execute: fn(&mut Simulation, &Bump, EntityId, EntityId) -> (&'a str, Speaker),
    describe: fn(&Simulation, &Bump, &Entity, &Entity, &mut StringPool) -> SpanHandle,
}

fn init_choices() -> &'static [Choice<'static>] {
    &[
        Choice {
            id: ENTRY_POINT,
            filter: |_, _, _, _| true,
            execute: |_, _, _, _| ("test", Speaker::Flip),
            describe: |_, _, _, _, pool| pool.push_str("Test"),
        },
        Choice {
            id: "test_a",
            filter: |_, _, _, _| true,
            execute: |_, _, _, _| (OVER, Speaker::Keep),
            describe: |_, _, _, _, pool| pool.push_str("Choice A"),
        },
        Choice {
            id: "test_a",
            filter: |_, _, _, _| true,
            execute: |_, _, _, _| (OVER, Speaker::Keep),
            describe: |_, _, _, _, pool| pool.push_str("Choice B"),
        },
    ]
}
