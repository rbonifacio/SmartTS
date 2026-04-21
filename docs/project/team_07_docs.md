# SmartTS Project 7 — `@view` Decorator

## Overview

This project adds support for the `@view` decorator to SmartTS. A `@view` method is a read-only method that can inspect contract state without changing persisted storage.

The implementation was completed end-to-end, covering the language representation, parsing, static checking, interpretation, CLI usage, sample contract, and automated tests.

## What Changed

### `lib/SmartTS/AST.hs`

The AST was extended so SmartTS can represent `@view` methods explicitly. This allows the rest of the compiler and runtime pipeline to recognize when a method is read-only.

### `lib/SmartTS/Parser.hs`

The parser was updated to recognize the `@view` decorator syntax, for example:

```smartts
@view get(): int {
  return storage.count;
}
```

### `lib/SmartTS/TypeCheck.hs`

The type checker was updated to enforce the main rule of the project:

- `@view` methods may read from `storage`
- `@view` methods may **not** write to `storage`

If a `@view` method tries to mutate state, the checker rejects it with an error such as:

```text
@view method cannot modify storage.
```

### `lib/SmartTS/Interpreter.hs`

The interpreter was extended to support view execution in read-only mode.

This means a `@view` call:

- evaluates the method body
- returns the computed value
- does **not** persist changes to the repository state

During this phase, a bug in record field replacement logic (`insertOrReplace`) was also fixed to ensure storage updates remain correct.

### `app/Main.hs`

The CLI was adapted so users can call views directly from the terminal using flags such as:

- `--view`
- `--view-name`

This made it possible to originate a contract instance and then query its views without mutating its stored state.

### `samples/Counter.smartts`

The sample contract was updated to include a real `@view` method:

```smartts
@view get(): int {
  return storage.count;
}
```

This sample was used to validate the full flow from parsing to CLI execution.

### `test/Main.hs`

The test suite was extended with specific tests for `@view`, including:

- parsing of `@view` methods
- rejection of storage writes inside `@view`
- successful execution of a view
- guarantee that view calls do not mutate repository state
- verification that entrypoints can still mutate state and views can read the updated value

## Installation

These steps assume Ubuntu or another Debian-based Linux distribution.

### 1. Install GHC and Cabal

```bash
sudo apt update
sudo apt install ghc cabal-install
```

### 2. Enter the project directory

```bash
cd ~/projects/compiladores/SmartTS
```

### 3. Update Cabal package metadata

```bash
cabal update
```

## Build and Test

Compile the project:

```bash
cabal build
```

Run the automated tests:

```bash
cabal test
```

## Execution

### 1. Clean the local repository state

```bash
rm -rf repo
```

### 2. Originate the sample contract

```bash
cabal run smart-ts -- --originate --repo repo --source samples/Counter.smartts --args '{"initialCount":5}'
```

Expected output format:

```text
Originated contract at address: KT1...
```

### 3. Call the `get` view

Replace `SEU_ENDERECO` with the address returned by the previous command.

```bash
cabal run smart-ts -- --view --repo repo --address SEU_ENDERECO --view-name get --args '{}'
```

Expected output:

```text
5
```

## End-to-End Validation Performed

The following validations were completed successfully during development:

- parsing a contract containing `@view`
- type checking a valid read-only view
- rejecting an invalid view that writes to storage
- originating a contract instance with CLI
- calling `@view get()` through CLI and receiving the expected result
- confirming that the repository remains unchanged after a view call
- running `cabal build` successfully
- running `cabal test` successfully

## Final Result

The SmartTS implementation now supports `@view` methods across the full toolchain:

- language representation
- parser
- type checker
- interpreter
- CLI
- sample contract
- tests

This completes the main scope of Project 7.