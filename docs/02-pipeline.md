# 2 — The SmartTS Pipeline

This document explains how a SmartTS source file goes from plain text to a
deployed, callable contract. There are four stages, each one transforming or
validating the program before passing it to the next.

---

## The Four Stages

```
Source text (.smartts)
        │
        ▼
  ┌──────────┐
  │  Parser  │  lib/SmartTS/Parser.hs
  └──────────┘
        │  Contract AST
        ▼
  ┌──────────────┐
  │ Type Checker │  lib/SmartTS/TypeCheck.hs
  └──────────────┘
        │  Well-typed Contract AST
        ▼
  ┌─────────────┐
  │ Interpreter │  lib/SmartTS/Interpreter.hs
  └─────────────┘
        │
        ▼
  Repository state (state.json)
```

---

## Stage 1 — Parser

**Input:** The contents of a `.smartts` file as a `String`.

**Output:** Either a parse error, or a `Contract` value — a Haskell data
structure that represents the contract's structure (its name, storage fields,
and method declarations). At this stage no type information has been computed.

**What it does:** The parser reads the source text and recognises the SmartTS
grammar: the `contract` keyword, the `storage` block, method decorators,
parameters, types, and the method bodies. If the input does not match the
grammar (for example, a missing semicolon or an unknown keyword), parsing
fails immediately and the pipeline stops.

The parser does **not** check whether variable names are declared, whether
types are compatible, or whether the right method kinds exist — those are
handled in the next stage.

---

## Stage 2 — Type Checker

**Input:** The `Contract` AST produced by the parser.

**Output:** Either a `Left String` type error (the first problem found), or
`Right ()` — confirmation that the contract is well-typed.

**What it does:** The type checker walks every method in the contract and
verifies that expressions and statements are consistent with the declared
types:

- No duplicate storage fields or duplicate parameter names within a method.
- Every variable referenced in a method body is either a parameter or a
  declared local.
- Every expression has a type consistent with how it is used.
- Assignment targets are valid: `storage` fields are always assignable; local
  variables must be `var` (not `val` or a parameter).
- `return` expressions match the method's declared return type.
- `if` and `while` conditions have type `bool`.

Once the type checker succeeds, the contract is guaranteed to be free of the
errors listed above. The interpreter can skip redundant checks at runtime.

---

## Stage 3 — Interpreter

**Input:** The well-typed `Contract` AST, a set of JSON arguments (for the
called method), and the current repository state.

**Output:** Either a `Left String` runtime error, or `Right` with an updated
repository state and an optional return value.

**What it does:** The interpreter executes the requested method. It walks the
method's statement tree, evaluating expressions and performing actions. Two
operations are supported:

- **Originate** — runs the `@originate` method to initialise storage and
  registers a new contract instance at a fresh address.
- **Call** — runs an `@entrypoint` method against the current stored state,
  then writes the updated state back.

The interpreter is a **tree-walking** interpreter: it recurses directly over
the AST rather than compiling to bytecode first.

---

## Stage 4 — Persistence

**Input:** The updated `RepositoryState` from the interpreter.

**Output:** A `state.json` file written to the repository directory, plus a
copy of the contract source saved under `contracts/`.

**What it does:** The CLI serialises the in-memory repository state to JSON
using the `aeson` library and writes it to disk. On the next command, the
state is loaded from disk and the contract sources are re-parsed and
type-checked before execution.

---

## Where Each Stage Runs

The CLI (`app/Main.hs`) glues the stages together. The flow for each command
is:

### `--originate`

```
1. Read source file
2. Parse → Contract
3. Type-check Contract
4. Decode --args JSON
5. Load state.json (if it exists)
6. Rebuild RepositoryState from state.json
   (re-parse, re-type-check, re-decode storage for every instance)
7. originateWithJsonArgs → new address + updated RepositoryState
8. Save state.json
9. Save contracts/<Name>.smartts
10. Print address
```

### `--call`

```
1. Load state.json
2. Rebuild RepositoryState from state.json
3. Look up the address → ContractInstance
4. Read contracts/<Name>.smartts
5. Parse → Contract
6. Type-check Contract
7. Decode --args JSON
8. callEntrypointWithJsonArgs → optional return value + updated RepositoryState
9. Save state.json
10. Print return value (if any)
```

---

## Why Type-Check on Every Command?

The contract source on disk could have changed between an originate and a
call. Re-running the type checker on every command catches this early, before
the interpreter touches any state. The same check also validates that
persisted storage JSON still matches the contract's declared `storage` type —
so a renamed field is caught immediately rather than silently producing wrong
values at runtime.

---

**What to read next →** [03-ast.md](03-ast.md)
