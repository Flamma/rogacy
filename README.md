# Rogacy - A Roguelike Game

This is a simple roguelike game built as an experiment to explore game development using AI assistance. The entire project was created with the help of OpenCode AI using the Qwen3 Code model.

## About the Game

Rogacy is a text-based roguelike where you control a character (@) navigating through dungeon maps filled with walls (#) and various entities (represented by different characters). The game features:

- Simple movement using WASD keys
- Collision detection with walls
- Functional programming approach with immutable game state
- Text-based rendering

## Features

- Player movement (WASD keys)
- Wall collision detection
- Simple entity rendering
- Functional design (immutable data structures)
- Comprehensive unit test coverage

## Technical Details

The game is written in Scala and follows functional programming principles. All game state is immutable, with functions returning new state objects rather than modifying existing ones.

## Installation and Running

To run the game:

```bash
sbt run
```

To run tests:

```bash
sbt test
```

## Controls

- W: Move up
- A: Move left
- S: Move down
- D: Move right
- Q: Quit game

## Development

This project was created entirely through AI assistance using OpenCode with the Qwen3 Code model. The goal was to experiment with building a complete game using only AI guidance.

## License

This project is licensed under the GNU General Public License v3.0 (GPL 3) - see the LICENSE file for details. This ensures that the game remains free and open source software.