# StarHS

StarHS is a Haskell implementation of the core ideas from the Star calculus, based on the paper *Structuring Arrays with Algebraic Shapes* by Bachurski, Mycroft, and Orchard.

The project explores how arrays can be structured using algebraic shapes, allowing array regions to be represented and accessed in a more expressive way than standard index-based arrays.

## Overview

StarHS implements the main components needed to represent Star-style arrays in Haskell, including:

- `Shape` types for describing array structure
- `Index` types for accessing elements
- `StarArray` for storing values with an associated shape
- Helper functions for constructing arrays
- Bounds checking for valid indexing
- Integration with Haskell's `Data.Array` system

The aim of the project is to provide a practical Haskell implementation of the theoretical ideas described in the Star paper.

## Features

- Algebraic representation of array shapes
- Custom index types for structured access
- Safe indexing with bounds checks
- Array construction helpers
- Basic Star-style array operations
- Designed to be understandable and extendable

## Project Structure

```text
StarHS/
├── src/          # Main Haskell source files
├── examples/          # Contains Conway's Game of Life implementation using Star
└── starhs.cabal
