# AGENTS.md

Guidance for autonomous coding agents working in this repository.

## Scope

- This file applies to the entire repo rooted at `steward/`.
- Follow direct user instructions first, then this file.
- Keep changes focused and minimal.
- Prefer consistency with existing code over personal style preferences.

## Project Snapshot

- Language: Rust (edition `2024`).
- Root crate: `steward` (binary crate).
- Workspace member crate: `crates/util`.
- Rendering/runtime library: `macroquad`.
- Custom arena utilities live in `util::arena` and are used heavily.

## Repository Layout

- `src/main.rs`: app entrypoint and frame loop.
- `src/things.rs`: core entity storage and iteration logic.
- `src/csv.rs`: CSV parsing utilities and tests.
- `crates/util/src/arena.rs`: bump arena + arena vector/string aliases.
- `crates/util/src/bitset.rs`: fixed-size bitset helper and tests.

## Build Commands

- Build entire workspace:
  - `cargo build`
- Build release:
  - `cargo build --release`
- Build only util crate:
  - `cargo build -p util`
- Typecheck without building artifacts:
  - `cargo check`
- Typecheck util crate only:
  - `cargo check -p util`
- Run the app:
  - `cargo run`

## Lint & Format Commands

- Format all Rust code:
  - `cargo fmt --all`
- Check formatting without writing:
  - `cargo fmt --all -- --check`
- Run Clippy across common targets:
  - `cargo clippy --all-targets --all-features`
- Strict Clippy (treat warnings as errors for CI-like pass):
  - `cargo clippy --all-targets --all-features -- -D warnings`

## Test Commands

- Run all tests in workspace:
  - `cargo test`
- Run tests with compact output:
  - `cargo test -q`
- Run tests for util crate only:
  - `cargo test -p util`
- Run tests in binary crate only:
  - `cargo test -p steward`

## Running a Single Test (Important)

- Run one exact test in current package:
  - `cargo test csv::tests::parses_basic_csv -- --exact`
- Run one exact test from util crate:
  - `cargo test -p util iter_single_word_indices -- --exact`
- Run all tests in a module namespace:
  - `cargo test csv::tests::`
- Run one test and show stdout/stderr:
  - `cargo test csv::tests::parses_quotes_and_escapes -- --exact --nocapture`

## Coding Style: High-Level

- Use idiomatic Rust, but align with existing local patterns first.
- Favor simple control flow with early returns.
- Keep functions focused and composable.
- Prefer explicit invariants and clear data flow over clever abstractions.
- Avoid unnecessary allocations, especially in per-frame or parser hot paths.

## Imports & Module Organization

- Keep imports minimal and used.
- Prefer grouped imports by origin when practical:
  - std (if present), external crates, then local crate modules.
- Use local aliases only when they improve readability (e.g. `macroquad::prelude as mq`).
- Avoid wildcard imports unless already established and justified.

## Formatting Conventions

- Use `rustfmt` defaults (no repo-specific rustfmt config currently).
- Preserve readable line breaks in long `if`/`while` conditions.
- Keep trailing commas in multiline literals/struct initializers.
- Maintain one logical action per line in imperative sections.

## Types, Ownership, and Lifetimes

- Prefer `pub(crate)` over `pub` unless external visibility is truly required.
- Derive traits intentionally (`Clone`, `Copy`, `Default`, `Debug`, etc.) when needed.
- Use references/slices for borrowed data (`&str`, `&[T]`) instead of owned clones.
- Use explicit lifetimes where arena-backed borrows require clarity.
- Prefer fixed-size numeric types (`u8`, `u32`, `f32`) consistent with surrounding code.

## Arena Allocation Guidelines

- The codebase already uses bump allocation patterns for transient data.
- When adding temporary collections, prefer `Arena::new_vec*` over `Vec` when lifetimes allow.
- Keep table/row/cell collections arena-backed if they are tied to arena lifetime.
- Avoid accidental heap fallback in performance-sensitive parsing/render paths.
- Reset/reuse arenas (`Arena::reset`) in frame-style loops instead of reallocating.

## Naming Conventions

- Types/enums/traits: `UpperCamelCase`.
- Functions/methods/variables/modules: `snake_case`.
- Constants: `UPPER_SNAKE_CASE`.
- Test names: descriptive snake_case, usually behavior-oriented.
- Prefer concise names, but keep domain intent obvious (`free_list_head`, `hovered_id`).

## Error Handling & Assertions

- Use `assert!` for internal invariants and impossible states.
- Prefer graceful returns for expected invalid input/state transitions.
- Avoid panics for recoverable parsing or gameplay-state conditions.
- In app bootstrap code, `unwrap` may be acceptable when failure is fatal and obvious.
- In reusable logic, prefer returning values/options instead of panicking.

## Comments & Documentation

- Add comments for intent, invariants, and non-obvious behavior.
- Avoid comment noise that restates obvious syntax.
- Keep comments short and accurate; update when logic changes.
- For parser/state-machine code, comment transitions and permissive behaviors.

## Testing Conventions

- Keep unit tests close to implementation via `#[cfg(test)] mod tests`.
- Validate both happy paths and edge cases.
- For parsing code, include malformed/partial-input tests when behavior is permissive.
- Assert on both structural output and value-level semantics.
- Prefer deterministic tests without timing or external I/O dependencies.

## Agent Workflow Expectations

- Before editing, inspect nearby code and mirror established style.
- Make surgical changes first; refactor broadly only when requested.
- Run relevant tests for touched areas at minimum.
- If you cannot run a command, state what was not verified.
- Do not introduce unrelated formatting churn.

## Cursor/Copilot Rules Status

- Checked for Cursor rules in `.cursor/rules/` and `.cursorrules`: none found.
- Checked for GitHub Copilot instructions in `.github/copilot-instructions.md`: none found.
- No additional repository-local agent instruction files were detected.

## Practical Defaults for This Repo

- Prefer `cargo test` as the final verification step for most changes.
- For focused iteration, use exact single-test commands first, then full suite.
- For style checks before handoff, run `cargo fmt --all -- --check` and Clippy.
- Keep arena-oriented design intact unless user explicitly requests otherwise.
