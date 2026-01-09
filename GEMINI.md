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
