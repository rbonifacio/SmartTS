# 5 — The Type Checker

The type checker is the second stage of the pipeline. It reads the `Contract`
AST produced by the parser and either reports the first type error it finds,
or returns `Right ()` — confirmation that the contract is well-typed.

---

## Three Errors the Type Checker Catches

Before diving into how it works, here are three concrete examples of what the
type checker prevents.

### Error 1: Return type mismatch

```typescript
contract C {
  storage: { x: int };

  @originate
  init(): int {
    return true;  // int expected, bool given
  }
}
```

Error: `return value has wrong type: expected int, inferred bool.`

### Error 2: Arithmetic on a non-integer

```typescript
contract C {
  storage: { x: int };

  @originate
  init(): int {
    return 1 + true;  // both operands must be int
  }
}
```

Error: `right operand of arithmetic operator has wrong type: expected int, inferred bool.`

### Error 3: Assignment to an immutable val

```typescript
contract C {
  storage: { x: int };

  @originate
  init(): int {
    val v: int = 1;
    v = 2;  // val cannot be reassigned
    return 0;
  }
}
```

Error: `Cannot assign to immutable val 'v' (or through it for field updates).`

---

## How the Type Checker Works

The entry point is:

```haskell
typeCheckContract :: Contract -> Either String ()
```

It runs three checks in order:

1. **Duplicate storage fields** — no two fields in `storage: { … }` may share
   a name.
2. **Duplicate parameters** — within each method, no two formal parameters may
   share a name.
3. **Method bodies** — every method is checked independently with
   `checkMethod`.

### Tracking names with `TcEnv`

To know the type of a variable at any point in a method body, the type checker
keeps an environment:

```haskell
data TcEnv = TcEnv
  { envStorageType :: Type          -- the contract's full storage record type
  , envBindings    :: Map Name TcBinding
  , envReturnType  :: Type          -- the method's declared return type
  }
```

Each binding records whether the name is a parameter, a mutable local (`var`),
or an immutable local (`val`):

```haskell
data BindingKind = Param | LocalMutable | LocalImmutable
```

The environment for a method starts with all parameters bound as `Param`. As
the checker walks the statement sequence, `var` and `val` declarations extend
the map. The updated environment is threaded through `checkStmt`, so
declarations in earlier statements are visible to later ones in the same
sequence.

---

## Scoping Rules

### Statement sequences

`checkStmt` for `SequenceStmt` uses `foldM`, threading the environment from
left to right:

```
stmt1 → extended env → stmt2 → extended env → stmt3 → …
```

A `var x` or `val x` declaration in `stmt1` is visible in `stmt2` and beyond.

### Branches do not export locals

`if` and `while` branches are checked with the **outer** environment, and the
outer environment is **not** updated from inside a branch:

```typescript
if (flag) {
  var inner: int = 10;  // inner is visible only inside this branch
}
// inner is NOT in scope here
```

### Shadowing parameters

Declaring `var x` or `val x` is rejected if `x` is already a **local** in
the current environment. But if `x` is only a **parameter**, the declaration
is allowed and the new local **shadows** the parameter for subsequent
statements. This mirrors the runtime lookup order: locals are checked before
parameters.

---

## Expression Typing Rules

The core function is:

```haskell
inferExpr :: TcEnv -> Expr -> Either String Type
```

Summary of the rules:

| Expression | Type |
|------------|------|
| Integer literal | `int` |
| `true` / `false` | `bool` |
| `()` | `unit` |
| `storage` | the contract's storage record type |
| `x` (variable) | the type bound to `x` in the environment |
| `e.f` | the type of field `f` in the record type of `e` |
| `{ k1: e1, … }` | `{ k1: T1, … }` where each `Ti` is inferred from `ei` |
| `!e` | `bool` (requires `e : bool`) |
| `e1 && e2`, `e1 \|\| e2` | `bool` (requires both operands `bool`) |
| `+`, `-`, `*`, `/`, `%` | `int` (requires both operands `int`) |
| `<`, `<=`, `>`, `>=` | `bool` (requires both operands `int`) |
| `==`, `!=` | `bool` (requires both operands to have the **same** type) |

### Type equality

Two types are equal if and only if:

- They are the same primitive (`int`, `bool`, or `unit`), **or**
- They are both records of the same length, and at each position `i`, the
  field names are equal and the field types are recursively equal.

There is no width subtyping, no row polymorphism, and no implicit coercion.

---

## Statement Checking Rules

| Statement | Rule |
|-----------|------|
| `return e` | Infer type of `e`; must match `envReturnType`. |
| `var x: T = e` | No duplicate local `x`; infer type of `e`; must equal `T`; add `LocalMutable` binding. |
| `val x: T = e` | Same as `var`, but binding is `LocalImmutable`. |
| `lv = e` | Check `lv` is assignable; compute type of `lv`; infer type of `e`; must match. |
| `if (cond) then [else]` | Cond must be `bool`; check each branch with outer env; env unchanged after. |
| `while (cond) body` | Cond must be `bool`; check body with outer env; env unchanged after. |

### Assignability

Before type-checking an assignment's right-hand side, the checker verifies
the left-hand side is writable:

- `LStorage` or any `LField` rooted at `LStorage` — always assignable.
- `LVar x` or `LField (LVar x) …` — `x` must be a `LocalMutable` binding.
  Parameters (`Param`) and immutable locals (`LocalImmutable`) are rejected.

---

## What the Type Checker Does Not Verify

- **Termination** — a method may fall off the end without a `return`. That is
  not checked; it will produce a `Nothing` return value at runtime.
- **Division by zero** — still a runtime error.
- **Exactly one `@originate`** — the parser and interpreter enforce this; the
  type checker checks each method independently without cross-method analysis.
- **Polymorphism, type inference** — the language is fully annotated by
  design; the checker infers expression types but all declarations carry
  explicit annotations.

---

## Key Design Decision: First Error Only

The type checker stops and reports the first error it encounters. It does not
collect multiple errors and report them all at once. This is appropriate for
a language where contracts are short and errors are typically fixed one at a
time.

---

**What to read next →** [06-interpreter.md](06-interpreter.md)
