# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Steward is a Rust game development project using Macroquad (game engine) with egui (immediate-mode GUI). Currently in early prototype stage.

## Build Commands

```bash
# Build and run
cargo run

# Build with optimizations
cargo build --release

# Run release build
cargo run --release

# Check for compilation errors (faster than full build)
cargo check

# Format code
cargo fmt

# Lint with clippy
cargo clippy
```

## Tech Stack

- **Language**: Rust (Edition 2024)
- **Game Framework**: Macroquad 0.4.4
- **UI Framework**: egui 0.31 via egui-macroquad 0.17
- **Memory Allocation**: bumpalo (with collections feature)
- **Entity Storage**: slotmap

## Architecture

**Entry Point**: `src/main.rs`
- `main()` - Configures Macroquad window (1600x900, high DPI)
- `async fn amain()` - Main event loop with egui integration

**Pattern**: Async event loop with immediate-mode UI. Each frame:
1. Process egui UI
2. Handle input (Escape to exit)
3. Draw egui output
4. Wait for next frame

## Dependencies Notes

Macroquad bundles audio (quad-snd), image loading, and math (glam). These are available without additional dependencies.
