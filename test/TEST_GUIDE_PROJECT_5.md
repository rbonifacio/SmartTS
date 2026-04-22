# Project 5 - Test Guide

This document describes every test file added for Project 5 (`fail_with` and `require`) and explains how to run them.

---

## Test files

### `test/FailWithRequireTests.hs`

Haskell unit tests using the [Tasty](https://hackage.haskell.org/package/tasty) framework. They are compiled as part of the existing `smart-ts-test` suite and run with `cabal test`.

The module exports one `TestTree` called `failWithRequireTests`, which is wired into `test/Main.hs`. It is organised in three groups:

#### Parser (8 tests)

| Test | What it checks |
|---|---|
| `fail_with(int)` produces `FailWith` node | AST shape |
| `fail_with` payload can be an expression | `FailWith (Add ...)` |
| `require(cond, payload)` produces `RequireStmt` | AST shape |
| `require` with `&&` condition | Complex condition parsing |
| Multiple `require` produce multiple `RequireStmt` nodes | Statement sequence |
| `fail_with` inside `if-then` | Nested context |
| `fail_with` is a reserved word | Cannot be used as identifier |
| `require` is a reserved word | Cannot be used as identifier |

#### Type Checker (10 tests)

| Test | What it checks |
|---|---|
| `fail_with` accepted where `int` expected | `never <: int` |
| `fail_with` accepted where `bool` expected | `never <: bool` |
| `fail_with` accepted where `unit` expected | `never <: unit` |
| All branches return or fail - no false positive | Multi-path subtyping |
| Ill-typed `fail_with` payload rejected | Payload is type-checked |
| `require` with `bool` condition accepted | Happy path |
| `require` condition must be `bool` | Rejects `int` condition |
| `require` payload may be any well-typed expression | Payload freedom |
| Multiple `require` in sequence accepted | Sequential guards |
| `require` inside `while` body accepted | Nested context |

#### Interpreter (8 tests)

| Test | What it checks |
|---|---|
| `fail_with` taken branch returns `FailWith` value | Abort mechanics |
| `fail_with` not taken - normal return | Non-abort path |
| `require` passing - result returned | Guard passes |
| `require` failing - `FailWith` with correct payload | Guard fails |
| `require` failing - storage NOT modified | Storage semantics |
| First `require` passes, second fails - correct payload | Sequencing |
| `require` inside `if-else` - only taken branch | Conditional context |
| `fail_with` payload expression fully evaluated | Payload evaluation |

### `samples/FailWithRequire.smartts`

A self-contained SmartTS contract that exercises every feature added in Project 5. It can be used for manual end-to-end testing with the CLI.

---

## Running the unit tests

```bash
cabal test
```

This runs the single test suite `smart-ts-test`, which includes both the original tests and the new `failWithRequireTests` group. Expected output (abbreviated):

```
Test suite smart-ts-test: RUNNING...
SmartTS
  Parser Tests
    ...existing tests...
  Type checker
    ...existing tests...
  Project 5: fail_with and require
    Parser
      fail_with(int) produces FailWith node:                          OK
      fail_with payload can be an expression:                         OK
      require(cond, payload) produces RequireStmt:                    OK
      require with && condition parses correctly:                     OK
      multiple requires parse as multiple RequireStmt nodes:          OK
      fail_with inside if-then branch parses:                         OK
      fail_with is a reserved word - cannot be used as identifier:    OK
      require is a reserved word - cannot be used as identifier:      OK
    Type Checker
      fail_with has type never - accepted where int expected:         OK
      fail_with has type never - accepted where bool expected:        OK
      fail_with has type never - accepted where unit expected:        OK
      all branches return or fail - no missing-return false positive: OK
      fail_with payload is type-checked - rejects ill-typed payload:  OK
      require with bool condition is well-typed:                      OK
      require condition must be bool - rejects int condition:         OK
      require payload may be any well-typed expression:               OK
      multiple requires in sequence are well-typed:                   OK
      require inside while body is well-typed:                        OK
    Interpreter
      fail_with taken branch - returns FailWith value:                OK
      fail_with not taken - normal return value:                      OK
      require passing - execution continues, result returned:         OK
      require failing - returns FailWith with correct payload:        OK
      require failing - storage is NOT modified:                      OK
      first require passes, second fails - correct payload:           OK
      require inside if-else - only fires on the taken branch:        OK
      fail_with payload expression is fully evaluated before abort:   OK
All 84 tests passed.
```

---

## Manual end-to-end testing with the CLI

The CLI has two commands. The exact flag signatures are:

```
smart-ts --originate --repo <dir> --source <file.smartts> --args '<json>'
smart-ts --call      --repo <dir> --address <KT1...> --entrypoint <name> --args '<json>'
```

Key points:
- `--repo` takes a **directory** name, not a file. The CLI creates `<dir>/state.json` and `<dir>/contracts/` inside it automatically.
- `--originate` and `--call` are standalone flags with no value. The contract path goes after `--source` and the entrypoint name goes after `--entrypoint`.
- On **Windows PowerShell**, JSON strings inside double-quoted arguments need their inner quotes escaped with backslash. The commands below show both forms.

### 1. Build

```bash
cabal build
```

### 2. Originate the sample contract

**Linux / macOS:**
```bash
cabal run smart-ts -- --originate --repo myrepo --source samples/FailWithRequire.smartts --args '{}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --originate --repo myrepo --source samples/FailWithRequire.smartts --args '{}'
```

Expected output:
```
Originated contract at address: KT1<hash>_0
```

Copy the full address. Replace `<ADDRESS>` with it in every command below.

---

### 3. Test `fail_with` - abort path

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint getValueOrFail --args '{"shouldFail": true}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint getValueOrFail --args '{\"shouldFail\": true}'
```

Expected: `{"FAILWITH":99}` (controlled abort, payload = 99)

---

### 4. Test `fail_with` - normal path

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint getValueOrFail --args '{"shouldFail": false}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint getValueOrFail --args '{\"shouldFail\": false}'
```

Expected: `0` (counter value after originate)

---

### 5. Test `require` passing

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint increment --args '{"by": 5}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint increment --args '{\"by\": 5}'
```

Expected: `5` (both requires passed, counter incremented)

---

### 6. Test `require` failing - non-positive argument

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint increment --args '{"by": -1}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint increment --args '{\"by\": -1}'
```

Expected: `{"FAILWITH":2}` (second require fires: `require(by > 0, 2)`)

---

### 7. Test `require` failing - contract disabled

First disable the contract:

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint setEnabled --args '{"value": false}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint setEnabled --args '{\"value\": false}'
```

Then try to increment:

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint increment --args '{"by": 5}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint increment --args '{\"by\": 5}'
```

Expected: `{"FAILWITH":1}` (first require fires: `require(storage.enabled, 1)`)

---

### 8. Test multiple requires - second fails

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint rangeAdd --args '{"x": 5, "y": 3}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint rangeAdd --args '{\"x\": 5, \"y\": 3}'
```

Expected: `{"FAILWITH":101}` (second require fires: `require(y > x, 101)` since 3 is not greater than 5)

---

### 9. Test computed payload

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint failWithExpr --args '{"v": 4}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint failWithExpr --args '{\"v\": 4}'
```

Expected: `{"FAILWITH":41}` (payload = 4 * 10 + 1 = 41)

---

## Interpreting results

| Output | Meaning |
|---|---|
| A JSON value (`0`, `true`, ...) | Normal return - no abort occurred |
| `{"FAILWITH": N}` | Controlled abort - `fail_with(N)` or a failing `require(..., N)` fired |
| `Call failed: ...` | Unrecoverable error (wrong address, missing entrypoint, etc.) |
| `Parse error: ...` | Syntax problem in the `.smartts` source |
| `Type error: ...` | Static type constraint violated |

A `{"FAILWITH": N}` result is **not** a bug - it is the correct and expected outcome when a guard fails.

---

### 10. Test `checkOrFail` - `never <: bool` (abort path)

This entrypoint shows that `fail_with` is valid where a `bool` return is expected, not just `int`.

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint checkOrFail --args '{"condition": false}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint checkOrFail --args '{\"condition\": false}'
```

Expected: `{"FAILWITH":0}` (else branch taken, `fail_with(0)` fires; type `never` satisfied the `bool` return)

Then verify the normal path returns an actual `bool`:

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint checkOrFail --args '{"condition": true}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint checkOrFail --args '{\"condition\": true}'
```

Expected: `true`

---

### 11. Test `conditionalGuard` - `require` only fires on the taken branch

**Case A — flag is false, require is never evaluated, counter resets to 0:**

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint conditionalGuard --args '{"flag": false, "v": -5}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint conditionalGuard --args '{\"flag\": false, \"v\": -5}'
```

Expected: `0` (else branch taken, `require` never evaluated despite `v = -5` which would fail it)

**Case B — flag is true and v is positive, require passes:**

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint conditionalGuard --args '{"flag": true, "v": 3}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint conditionalGuard --args '{\"flag\": true, \"v\": 3}'
```

Expected: `3` (then branch taken, `require(v > 0, 500)` passes, counter incremented by 3)

**Case C — flag is true and v is negative, require fires:**

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint conditionalGuard --args '{"flag": true, "v": -5}'
```

**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint conditionalGuard --args '{\"flag\": true, \"v\": -5}'
```

Expected: `{"FAILWITH":500}` (then branch taken, `require(v > 0, 500)` fails)

### Observation
Note that steps 5 and 7 share state — after step 5 the counter is 5, and after step 7's disable the contract stays disabled for subsequent calls. If you want to re-enable it before step 8 to have a clean counter, run:

**Linux / macOS:**
```bash
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint setEnabled --args '{"value": true}'
```
**Windows PowerShell:**
```powershell
cabal run smart-ts -- --call --repo myrepo --address <ADDRESS> --entrypoint setEnabled --args '{\"value\": true}'
```