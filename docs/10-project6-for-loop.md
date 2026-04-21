# 10 — Project 6 Implementation Plan (`for` Loop)

This document describes the implementation of **Project 6** in SmartTS with a
clean-code approach: clear responsibilities, minimal coupling, predictable
semantics, and focused test coverage.

---

## Objective

Add a traditional three-part `for` loop to SmartTS:

```typescript
for (init; condition; step) {
  // body
}
```

The feature touches the full execution pipeline:

1. AST representation
2. Parser grammar
3. Type checker rules
4. Interpreter semantics
5. Tests

---

## Scope and Syntax

Implemented syntax:

```typescript
for (var i: int = 0; i < 10; i = i + 1) { ... }
for (val i: int = 0; i < 10; ) { ... }
for (x = 0; x < 10; x = x + 1) { ... }
for (; x < 10; ) { ... }
```

Design choices:

- `condition` is mandatory and must be `bool`.
- `init` supports:
  - mutable declaration (`var`)
  - immutable declaration (`val`)
  - assignment
  - empty clause
- `step` supports:
  - assignment
  - empty clause

---

## Detailed Changes

### AST

File:

- `lib/SmartTS/AST.hs`

Changes:

- Added `ForStmt ForInit Expr ForStep Stmt` to `Stmt`.
- Added `ForInit`:
  - `ForInitNone`
  - `ForInitVar Name Type Expr`
  - `ForInitVal Name Type Expr`
  - `ForInitAssign LValue Expr`
- Added `ForStep`:
  - `ForStepNone`
  - `ForStepAssign LValue Expr`

### Parser

File:

- `lib/SmartTS/Parser.hs`

Changes:

- Added reserved word: `for`.
- Added `parseForStmt`.
- Added `parseForInit` and `parseForStep`.
- Registered `parseForStmt` in `parseStmt` chain.

Grammar:

```
forStmt := "for" "(" forInit ";" expr ";" forStep ")" stmt
forInit := varDeclNoSemi | valDeclNoSemi | assignmentNoSemi | ε
forStep := assignmentNoSemi | ε
```

### Type Checker

File:

- `lib/SmartTS/TypeCheck.hs`

Changes:

- Added `checkStmt` case for `ForStmt`.
- Added `checkForInit` and `checkForStep`.
- Enforced:
  - `condition : bool`
  - assignment constraints in `init` and `step`
  - loop-local declarations from `init` do not escape after the loop

### Interpreter

File:

- `lib/SmartTS/Interpreter.hs`

Changes:

- Added `execStmt` case for `ForStmt`.
- Added helpers:
  - `execForInit`
  - `execForStep`
  - `sanitizeLocals`
- Implemented runtime semantics equivalent to:

```text
init;
while (condition) {
  body;
  step;
}
```

### Tests

File:

- `test/Main.hs`

Added parser tests:

- `For statement (var init, condition, step)`
- `For statement (empty init and empty step)`
- `For statement (assignment init)`

Added type-check tests:

- `For loop with local counter is well-typed`
- `For condition must be bool`
- `For step cannot assign to val loop variable`
- `Loop variable is out of scope after for`
- `For initializer assignment to outer mutable local is well-typed`

---

## Verification

Executed command:

```bash
cabal test
```

Result:

- All tests passed after the `for` loop changes.

---

## Acceptance Checklist

- [x] AST updated with `for` representation
- [x] Parser accepts `for (init; cond; step) body`
- [x] Type checker validates condition and mutability constraints
- [x] Interpreter executes loop and preserves expected scope behavior
- [x] Parser and type-checker tests added for success/failure paths
