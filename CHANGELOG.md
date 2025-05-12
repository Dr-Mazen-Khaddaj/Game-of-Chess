# Changelog

## [Unreleased]
- Restructured project into modular components (`Types`, `GameLogic`, `GameController`, `GameInit`, `UI.*`)
- Enabled `GHC2021`, set GHC warnings, and updated Cabal build settings
- Bumped `base` to `^>=4.21.0.0`
- Introduced Fourmolu for formatting (`.fourmolu.yaml`, version 0.18.0.0)

### Planned
- Enhance core game logic
- Refactor data structures for clarity and efficiency
- Refactor monadic design
- Further modularization
- Enhanced terminal UI
- AI opponent

## [v0.1.1] - 2025-04-29
- Updated `.cabal` file with modern `base` and Cabal versions
- Minor fix in `Main.hs` for forward compatibility
- ✅ Recommended GHC version: 9.6.7

## [v0.1.0] - 2023-01-22
- Basic chess game runs in the terminal.
- Players can move pieces manually.
- Basic piece movement logic implemented.
- ✅ Recommended GHC version: 8.10.7
