# SmartTS type checker

This document describes the **static type checker** implemented in [`lib/SmartTS/TypeCheck.hs`](../lib/SmartTS/TypeCheck.hs). It reflects the **current** behavior of `typeCheckContract`; the language may evolve.

## Role and scope

- **Entry point:** `typeCheckContract :: Contract -> Either String ()`.
- **Style:** The surface language is **fully annotated** (parameter types, return types, `storage` fields, `var` / `val` types). The checker **infers expression types** and **checks** them against those annotations (and against the storage record). It does **not** perform Hindley–Milner generalization or type-class resolution.
- **Per-method environment:** Each method body is checked in an environment containing:
  - **`storage`** — type `TRecord` built from the contract’s `storage: { … }` field list (in source order).
  - **Parameters** — read-only bindings from the method signature.
  - **Return type** — from the method’s `: Type` annotation.
  - **Locals** — introduced by `var` / `val` while scanning a statement sequence (see scoping below).

Errors are returned as `Left String` with a short English message.

---

## Contract-level rules

### Duplicate names

1. **Storage** — No two fields in `storage: { … }` may share the same name.
2. **Parameters** — Within a single method, no two formal parameters may share the same name.

Every method in the contract is checked independently (including `@originate`, `@entrypoint`, and `@private`). There is no cross-method analysis beyond sharing the same storage type.

---

## Type language (checked forms)

The checker uses the same `Type` AST as the parser:

| Form | Meaning |
|------|---------|
| `int` | `TInt` |
| `bool` | `TBool` |
| `unit` | `TUnit` |
| `{ f1: T1, f2: T2, … }` | `TRecord [(f1,T1),(f2,T2),…]` — field order **matters** for equality |

### Type equality (`typesEqual`)

- `int`, `bool`, `unit` match only themselves.
- Two records match iff they have the **same length**, and for each index `i`, the **names** are equal and the field types are recursively equal.

There is **no** width subtyping, **no** row polymorphism, and **no** implicit coercion between primitives.

---

## Scoping and bindings

### Initial environment (start of a method)

- All formal parameters are in scope as **parameters** (immutable for assignment purposes).
- `storage` is not a normal variable: the expression `storage` has type `TRecord` of the contract storage.

### Statement sequences (`…; …`)

- The checker walks a `SequenceStmt` **left to right**, threading an environment.
- A `var` or `val` extends the map for all **following** statements in that same sequence.
- **`if` and `while` branches** are checked in the **outer** environment: bindings introduced only inside a branch do **not** appear in the outer sequence after the construct (branches do not export locals).

### Duplicate locals

- Declaring `var x` or `val x` is rejected if `x` is already bound as a **`var` or `val`** in the current environment.
- Declaring `var x` or `val x` is **allowed** if `x` is only a **parameter** so far: the new binding **shadows** the parameter for following statements (consistent with runtime lookup order: locals before parameters).

### Name resolution

- **`Var x`** — must refer to a parameter or a local in the current environment.
- Unknown names are rejected.

---

## Expression typing rules

In the judgments below, `Γ` is the usual implicit environment (storage type + bindings).

### Literals

| Expression | Type |
|------------|------|
| Integer literal | `int` |
| `true` / `false` | `bool` |
| `()` | `unit` |

### `storage`

- Type: the contract’s storage record type `TRecord (contractStorage)`.

### Variables

- `x` has the type declared for that parameter or local.

### Field access

- **Syntax:** `e.f` (parsed as `FieldAccess`).
- **Requirement:** `e` must have a **record** type that **declares** field `f`.
- **Result type:** the type of `f` in that record.
- Applies to `storage.f`, locals of record type, nested `e.f.g`, etc., as long as each step is a typed record with the corresponding field.

### Record literals

- `{ k1: e1, k2: e2, … }` has type `{ k1: T1, k2: T2, … }` where each `ei` is checked and has type `Ti`.
- Field order in the **inferred** type follows the **source order** of the literal.

### Boolean operators

| Operator | Operand types | Result |
|----------|---------------|--------|
| `!e` | `e : bool` | `bool` |
| `e1 && e2`, `e1 \|\| e2` | both `bool` | `bool` |

### Arithmetic (`int` only)

| Operator | Operand types | Result |
|----------|---------------|--------|
| `+`, `-`, `*`, `/`, `%` | both `int` | `int` |

### Ordered comparisons (`int` only)

| Operator | Operand types | Result |
|----------|---------------|--------|
| `<`, `<=`, `>`, `>=` | both `int` | `bool` |

### Equality

- **`==` and `!=`** — Both operands are type-checked independently; their types must be **exactly equal** according to `typesEqual` (including record field names, order, and nested types).
- Result type: **`bool`**.
- **Note:** This is **stricter** than the interpreter’s runtime `==`, which can compare arbitrary evaluated values.

---

## Statement rules

### `return e`

- `e` must have the method’s declared return type.

### `var x: T = e` / `val x: T = e`

- No duplicate local `x` (see scoping).
- `e` must have type **`T`** (exact match, no subtyping).

### Assignment `lhs = e`

1. **Assignability of `lhs`:**
   - **`storage` or `storage.f.…`** — root is assignable.
   - **`x` or `x.f.…`** — root variable `x` must be a **`var`** (mutable local). **Not** allowed: parameters, unknown names, or **`val`** (including “assigning through” a `val` for field update).
2. **Types:** `typeOfLValue(lhs)` must equal `inferExpr(e)` under `typesEqual`.

### `if (cond) then else`

- `cond` must have type **`bool`**.
- `then` and optional `else` branches are checked as nested statements; environment is **not** extended into the outer sequence from inside a branch.

### `while (cond) body`

- `cond` must have type **`bool`**.
- `body` is checked; same scoping as `if` (no export of inner locals).

---

## What the checker does **not** verify

- **Termination** or **exhaustive return** — A method may fall off the end without `return`; that is not rejected.
- **Division by zero** — Still a **runtime** error in the interpreter.
- **Reserved identifier `storage` as a `var` name** — Not special-cased in the checker (parser may still allow it; worth tightening later).
- **Polymorphism, overloading, subtyping, type inference** for omitted annotations — Not implemented.

---

## Relationship to the interpreter and CLI

- The **CLI** runs `typeCheckContract` after parsing a contract file used for **originate** or **call**.
- When **loading `state.json`**, each instance’s storage JSON is decoded with `jsonToExprByType` against that contract’s storage record **after** the contract source type-checks; see the main [README](../README.md).
- The **interpreter** assumes well-typed programs and typed storage for normal expression shapes; some impossible cases become `error` rather than `Left` (see `SmartTS.Interpreter` module header).

---

## References

- Implementation: [`lib/SmartTS/TypeCheck.hs`](../lib/SmartTS/TypeCheck.hs)
- Surface syntax types: [`lib/SmartTS/AST.hs`](../lib/SmartTS/AST.hs) (`data Type`, `Stmt`, `Expr`, `LValue`)
