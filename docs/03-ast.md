# 3 — The Abstract Syntax Tree (AST)

This document explains how a SmartTS contract is represented in memory after
parsing, and what each data type in the AST means.

---

## What is an AST?

When the parser reads source text like `storage.count + by`, it does not keep
the text as a string. Instead it builds a **tree** of data structures that
captures the program's structure:

```
      Add
     /   \
FieldAccess  Var
 /       \     \
StorageExpr  "count"  "by"
```

Each node in the tree is a Haskell value. Leaf nodes are literals and names;
branch nodes are operations. This tree is called the *Abstract Syntax Tree*
(AST). The type checker and interpreter work entirely with this tree.

All AST types are defined in `lib/SmartTS/AST.hs`.

---

## The Main Types

### `Contract` — the whole program

```haskell
data Contract = Contract
  { contractName    :: Name
  , contractStorage :: Storage
  , contractMethods :: [MethodDecl]
  }
```

A SmartTS program is a single contract. `Storage` is an ordered list of
`(Name, Type)` pairs — the declared persistent fields. `contractMethods`
holds all method declarations, regardless of kind.

### `MethodDecl` — one method

```haskell
data MethodDecl = MethodDecl
  { methodKind       :: MethodKind
  , methodName       :: Name
  , methodArgs       :: [FormalParameter]
  , methodReturnType :: ReturnType
  , methodBody       :: MethodBody
  }
```

`MethodBody` is a type alias for `Stmt`. Every method body is a single
statement — usually a `SequenceStmt` (a block).

### `MethodKind` — the decorator

```haskell
data MethodKind = Originate | EntryPoint | Private
```

Corresponds to `@originate`, `@entrypoint`, and `@private`. A method without
a decorator defaults to `Private`.

### `FormalParameter` — one parameter

```haskell
data FormalParameter = FormalParameter Name Type
```

Parameters are read-only: they cannot appear as assignment targets.

---

## Types

```haskell
data Type
  = TInt
  | TBool
  | TUnit
  | TRecord [(Name, Type)]
```

`TRecord` carries an **ordered** list of `(name, type)` pairs. Order matters
for type equality: `{ a: int, b: bool }` and `{ b: bool, a: int }` are
different types.

---

## Expressions

```haskell
data Expr
  = CInt  Int              -- integer literal, e.g. 42
  | CBool Bool             -- boolean literal, true or false
  | Unit                   -- the unit value ()
  | StorageExpr            -- the keyword `storage`
  | Var Name               -- variable reference
  | FieldAccess Expr Name  -- e.f
  | Record [(Name, Expr)]  -- { f1: e1, f2: e2, … }
  | Not  Expr              -- !e
  | And  Expr Expr         -- e1 && e2
  | Or   Expr Expr         -- e1 || e2
  | Add  Expr Expr         -- e1 + e2
  | Sub  Expr Expr         -- e1 - e2
  | Mul  Expr Expr         -- e1 * e2
  | Div  Expr Expr         -- e1 / e2
  | Mod  Expr Expr         -- e1 % e2
  | Eq   Expr Expr         -- e1 == e2
  | Neq  Expr Expr         -- e1 != e2
  | Lt   Expr Expr         -- e1 < e2
  | Lte  Expr Expr         -- e1 <= e2
  | Gt   Expr Expr         -- e1 > e2
  | Gte  Expr Expr         -- e1 >= e2
```

A few things to note:

- `StorageExpr` is the bare `storage` keyword. Reading `storage.count` parses
  as `FieldAccess StorageExpr "count"` — the same `FieldAccess` constructor
  used for any other field read.
- `Record` carries the fields in **source order**, which the type checker
  preserves. `{ a: 1, b: 2 }` and `{ b: 2, a: 1 }` produce different `Expr`
  values and different inferred types.

---

## Statements

```haskell
data Stmt
  = VarDeclStmt Name Type Expr         -- var x: T = e;
  | ValDeclStmt Name Type Expr         -- val x: T = e;
  | AssignmentStmt LValue Expr         -- lv = e;
  | IfStmt Expr Stmt (Maybe Stmt)      -- if (cond) then [else]
  | WhileStmt Expr Stmt                -- while (cond) body
  | ReturnStmt Expr                    -- return e;
  | SequenceStmt [Stmt]                -- { s1; s2; … }
```

`SequenceStmt` is the representation of a `{ … }` block. The parser always
wraps a method body in one, even if the body has only a single statement.

---

## LValues

```haskell
data LValue
  = LStorage             -- the keyword `storage` on the left-hand side
  | LVar Name            -- a local variable name
  | LField LValue Name   -- lv.f  — a field path
```

`LField` is recursive, so `storage.pos.x` parses as:

```
LField
  (LField
    LStorage
    "pos")
  "x"
```

The interpreter flattens this path and navigates the nested record to perform
the update.

---

## Helper Functions

`AST.hs` also exposes a few helpers used by the interpreter and CLI:

| Function | What it does |
|----------|-------------|
| `findMethods k c` | All methods of kind `k` in contract `c` |
| `findOriginatorMethod c` | The single `@originate` method; crashes if missing or duplicated |
| `isEntryPointMethod m` | `True` if `methodKind m == EntryPoint` |
| `isOriginateMethod m` | `True` if `methodKind m == Originate` |

---

## Why One `Expr` Type for Both Parse and Runtime

Unlike some language implementations that use separate *untyped* and *typed*
AST variants, SmartTS uses a **single `Expr` type** throughout. After the type
checker validates the program, the interpreter runs on the same `Contract`
value that came out of the parser — it does not transform the tree into a new
representation.

The trade-off is that some cases in the interpreter become `interpretBug`
(internal error) rather than user-facing `Left` — they represent situations
that are provably impossible after type checking but that the Haskell type
system cannot eliminate on its own. For example, the interpreter case for
`CBool True` inside `evalInt` is unreachable after a successful type check,
but Haskell requires the case to be present.

---

**What to read next →** [04-parser.md](04-parser.md)
