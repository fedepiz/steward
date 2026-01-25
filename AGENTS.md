# GEMINI.md

## Project Overview

This project is a 2D simulation or game developed in Rust. It utilizes the `macroquad` library for graphics and input, and `egui` for the graphical user interface. The project is structured as a Cargo workspace with two main crates:

*   **`simulation`**: This crate contains the core logic of the simulation, managing entities, objects, and their interactions.
*   **`util`**: This crate provides utility data structures and algorithms, such as `bucket_sort`, `span`, and `string_pool`, which are likely used to optimize the simulation.

The main application initializes a window and runs a simulation loop. It handles user input for camera controls (WASD for movement, Q/E for zoom) and allows interaction with the simulation through mouse clicks. The `egui` interface displays information about the simulation state.

## Building and Running

To build and run the project, use the following standard Cargo commands:

*   **Build:**
    ```bash
    cargo build
    ```

*   **Run:**
    ```bash
    cargo run
    ```

*   **Test:**
    ```bash
    cargo test
    ```

## Development Conventions

*   **Formatting**: The project follows standard Rust formatting conventions. Use `cargo fmt` to format the code.
*   **Linting**: Use `cargo clippy` to check for common mistakes and code style issues.
*   **Dependencies**: Dependencies are managed in the `Cargo.toml` file. Use `cargo update` to update dependencies.
*   **Workspace**: The project is organized as a Cargo workspace. The main application is in the root `src` directory, and the `simulation` and `util` crates are located in the `crates` directory.

## Coding Style & Philosophy

*   **ZII (Zero is Initialization) Style:** Avoid using `Option<T>` or `Result<T>` for the main data flow of an API. A function like `tick()` should always return a concrete `Response` value, even if it's a default or "empty" one. Error states should be tracked separately within the struct and exposed via a different method (e.g., `has_critical_error()`).

*   **Performance-Conscious Design:** Be mindful of performance, especially regarding memory. Prefer patterns that avoid many small, frequent allocations, such as using a `StringPool` for view-model data.

*   **Prefer PODs when possible:** PODs are structs that could be Copy, ie, they store no allocating structures and own no memory. PODs are easy to manipulate and operate with. If possible, prefer ZII PODs.
    If the POD logically would contain some kind of complex structure (ie, Strings, Vec<..>, etc), it's ok to lift those out into a separate container which manages the PODs, and replace the values with handles.

*   **Flat Control Flow:** When performing an action based on conditions, separate the logic from the action. Use `if/else` expressions to determine *what* to do, then have a single, non-nested call that performs the action.

    *   **Prefer this style:**
        ```rust
        let (text, color) = if sim.has_crashed() {
            ("Error", RED)
        } else if is_paused {
            ("Paused", WHITE)
        } else {
            ("", WHITE)
        };

        if !text.is_empty() {
            draw_billboard(text, color);
        }
        ```

    *   **Avoid this style:**
        ```rust
        if sim.has_crashed() {
            draw_billboard("Error", RED);
        } else if is_paused {
            draw_billboard("Paused", WHITE);
        }
        ```

*   **Surgical Changes:** When implementing a feature or fix, only change the code that is directly necessary. Avoid refactoring or moving unrelated code. Changes should be focused and minimal.

*   **Commenting:** When entities write their code, they should add comments, explaining the PURPOSE of groups of statements and expressions when it would not be immediately clear
