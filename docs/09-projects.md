# 9 — SmartTS Evolution Project

This document proposes a single extension project for the SmartTS language.
It gathers every feature that is still missing from the original list of
projects into one coherent roadmap. Each feature touches the whole pipeline —
parser, type checker, interpreter and LLTZ code generator — and requires you to
reason carefully about syntax, typing rules, runtime semantics and the
corresponding Michelson constructs.

## Where we are

Three of the original projects are already part of `main`. Use them as
reference implementations: each one shows, in a single commit, every place in
the pipeline that a new feature must touch.

| Feature | Commit | What to look at |
|---------|--------|-----------------|
| `for` loop | `70dcfe2` | A new statement with its own scope, lowered to LLTZ `For`. |
| `string` type | `a76f79c` | A new base type, literals, an overloaded operator (`+`) and a reserved builtin (`length`). |
| `map<K, V>` type | `9924ead` | A parameterised type, a comparability constraint on keys, contextual typing (`empty_map`), a new assignment target (`m[k] = v`) and a JSON encoding. |

The remaining features — `nat`, `option<T>`, `pair<T, U>`, enums, `list<T>`,
`fail_with`/`require`, `@view` and `@test` — make up the project described below.

---

## The project — Completing the SmartTS core

The project is organised in three parts. Implement and submit each feature as a
separate, self-contained pull request (see [Increments](#increments)). The
concrete syntax below is a suggestion: discuss any change in the Google
Classroom before implementing it.

### Part A — Types

#### A.1 `nat`

A non-negative integer type, the Michelson primitive for token balances.

- Syntax: the type `nat`; literals written with a suffix, e.g. `5n`.
- Builtins: `abs(i)` (`int → nat`), `is_nat(i)` (`int → option<nat>`, requires
  A.2) and `int(n)` (`nat → int`).
- Typing: arithmetic on two `nat` operands yields `nat`, except subtraction,
  which yields `int`. Mixing `int` and `nat` without a conversion is a type error.
- Runtime: non-negativity is an invariant that cannot be checked statically;
  decoding a negative JSON number into a `nat` must fail.
- LLTZ: `TNat`, `CNat`, `PrimAbs`, `PrimIsNat` and `PrimCastInt`.

#### A.2 `option<T>` and `match_option`

Optional values, the canonical way to represent a missing value in Michelson.

- Syntax: the type `option<T>`, the constructors `Some(e)` and `None<T>`, and a statement

  ```
  match_option (e) {
    Some(x) => { ... }
    None => { ... }
  }
  ```

- Typing: `x` is bound only inside the `Some` branch, with type `T`.
- Runtime/JSON: `None` is encoded as `null`.
- LLTZ: `TOption`, `PrimSome`, `PrimNone`, `IfNone` with a `LambdaBinder` for `x`.

#### A.3 `pair<T, U>`

A structural two-element product modelled on Michelson's `Pair`.

- Syntax: the type `pair<T, U>`, the constructor `Pair(a, b)`, the builtins
  `fst(p)` and `snd(p)`, and a destructuring declaration
  `val (a, b): pair<int, bool> = p;`, which creates two bindings at once.
- LLTZ: `TTuple`, `TupleExpr`/`PrimPair`, `Proj` (or `PrimCar`/`PrimCdr`),
  and `LetTupleIn` for destructuring.

#### A.4 Enums and `match`

User-defined enumerations, a restricted form of Michelson's `or` type.

- Syntax: a contract-level declaration `enum Color { Red, Green, Blue }`,
  values written `Color.Red`, and a statement

  ```
  match (c) {
    Red => { ... }
    Green => { ... }
    Blue => { ... }
  }
  ```

- Typing: enums are nominal and live in a type registry separate from the
  symbol table. Variants can be compared with `==`/`!=`. `match` must be
  exhaustive: a missing or duplicated case is a compile-time error.
- LLTZ: `TOr` with `unit` leaves, `Inj` for variants and `Match` for dispatch.

#### A.5 `list<T>` and `for_each`

An immutable linked list.

- Syntax: the type `list<T>`, literals `[e1, e2, ...]` (the empty literal `[]`
  takes its type from the context, as `empty_map` does), the builtins
  `cons(x, xs)`, `head(xs)` and `tail(xs)`, and a statement

  ```
  for_each (x in xs) { ... }
  ```

  Reuse the existing `length` builtin for the size of a list instead of adding
  a new name: Michelson's `SIZE` already works on strings, lists and maps.
- Typing: `x` is bound only inside the loop body.
- Runtime: `head` and `tail` of an empty list cannot be ruled out statically.
  They are runtime errors (a failure with a payload, see B.1), not internal
  interpreter bugs.
- LLTZ: `TList`, `PrimNil`, `PrimCons`, `IfCons` (with `PrimFailwith` on the
  empty branch), `PrimSize` and `ForEach`.

### Part B — Failures

#### B.1 `fail_with(e)`

An unconditional abort carrying a payload, modelling Michelson's `FAILWITH`.

- Typing: `fail_with` never produces a value, so it is valid in any return-type
  context (for example, as the only statement of a branch in a method that
  returns `int`).
- Runtime: the call fails and the CLI reports the payload encoded as JSON.
  No storage change is persisted.
- LLTZ: `PrimFailwith`.

#### B.2 `require(cond, e)`

A conditional guard: `require(c, e)` behaves as `if (!c) { fail_with(e); }`.
Implement it by desugaring, so that the logic is not duplicated in the type
checker, interpreter and code generator.

### Part C — Method kinds and tooling

#### C.1 `@view`

Read-only methods, related to Tezos on-chain views.

- Syntax: the decorator `@view`, and a CLI mode
  `smart-ts --view <name> --repo <dir> --address <KT1...> --args '{}'`.
- Typing: thread a read-only flag through the type-checker environment. A view
  must not write to `storage`, neither directly (`storage.x = ...`) nor
  indirectly by calling a method that writes to it. Combining `@view` with
  another method decorator is an error.
- Runtime: the CLI runs the view and never writes `state.json`. `--call` must not
  run views, and `--view` must not run entrypoints.

#### C.2 `@test`, `assert` and `--test`

First-class tests embedded in contracts.

- Syntax: the decorator `@test`, an `assert(cond)` statement and a CLI mode
  `smart-ts --test --source <contract.smartts>`.
- Runtime: `--test` originates the contract in a temporary repository and runs
  every `@test` method in isolation. Some tests may pass and others fail, and
  the command prints a human-readable report and exits with a non-zero status if
  any test fails.
- LLTZ: test methods and `assert` are not part of the deployed contract. Exclude
  them from code generation.


## Increments

Submit one pull request per feature (A.1 to C.2). The features are largely
independent, but a few depend on others:

- `is_nat` (A.1) returns an `option<nat>`, so it needs A.2.
- `head`/`tail` of an empty list (A.5) and `require` (B.2) fail through
  `fail_with` (B.1).
- `assert` (C.2) can be desugared into `require` (B.2).


