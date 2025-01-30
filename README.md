# Project Euler Solutions in Haskell

This repository contains solutions to Project Euler problems implemented in Haskell using Stack.

## Project Structure
- `src/Problems/`: Contains the solution modules for each problem
- `src/Utils/`: Contains utility modules for common operations
- `data/`: Contains input data files for problems

## Building the Project
```bash
stack build
```

## Running Solutions
After building, you can run individual solutions using:
```bash
stack exec project-euler-exe -- [problem-number]
```

For example, to run Problem 96 (Sudoku):
```bash
stack exec project-euler-exe -- 96
```

## Requirements
- Stack (Haskell build tool)
- GHC (Glasgow Haskell Compiler)

## Features
- Pure functional implementations
- Reusable mathematical utilities
- Clean and modular code structure

## Note
Project Euler recommends not sharing solutions for problems beyond #100 publicly. Please be mindful of this when contributing or sharing solutions.
