# 6 — The Interpreter

The interpreter is the third stage of the pipeline. It takes the well-typed
`Contract` AST and executes the requested method, producing an updated
repository state and an optional return value.

---

## A Worked Example: Evaluating `storage.count + by`

After parsing and type checking, `storage.count + by` becomes this tree:

```
        Add
       /   \
FieldAccess  Var "by"
   /      \
StorageExpr  "count"
```

The interpreter evaluates this tree by calling `evalExpr` recursively:

```
evalExpr(Add)
  ├── evalExpr(FieldAccess StorageExpr "count")
  │     ├── evalExpr(StorageExpr)  →  Record [("count", CInt 10), ("enabled", CBool True)]
  │     └── lookup "count"        →  CInt 10
  └── evalExpr(Var "by")          →  CInt 3
  →  CInt 10 + CInt 3  =  CInt 13
```

Final result: `CInt 13`. This "walk the tree and compute a value at each node"
approach is called a **tree-walking interpreter**.

---

## Runtime State: `Runtime`

Every method executes inside a `Runtime` record:

```haskell
data Runtime = Runtime
  { rtStorage :: Maybe Expr        -- current storage value (Nothing in originate until written)
  , rtParams  :: Map Name Expr     -- parameter bindings (read-only)
  , rtLocals  :: Map Name Binding  -- local var/val bindings
  }
```

`rtStorage` holds the entire storage as a single `Expr` — always a `Record`
once initialised. Reading `storage.count` evaluates `StorageExpr` to this
record, then navigates to the `"count"` field. Writing `storage.count = 13`
replaces the `"count"` field inside the record and stores the updated record
back in `rtStorage`.

### Bindings

```haskell
data Binding = Binding
  { bindingMutable :: Bool  -- True for var, False for val
  , bindingValue   :: Expr
  }
```

At runtime, every local is a `Binding`. The `bindingMutable` flag mirrors the
`var` / `val` distinction. After type checking, assigning to an immutable
binding is an `interpretBug` — it cannot happen in a well-typed program.

---

## Executing Statements

`execStmt :: Runtime -> Stmt -> Either String (Maybe Expr, Runtime)`

The first element of the result is the return value (`Nothing` if execution
continues normally, `Just v` if a `return` was reached). The updated `Runtime`
reflects any storage mutations and new local bindings.

| Statement | What happens |
|-----------|-------------|
| `SequenceStmt ss` | Execute each statement left to right; short-circuit on `Just v`. |
| `ReturnStmt e` | Evaluate `e`; return `Just v`. |
| `VarDeclStmt n _ e` | Evaluate `e`; insert mutable binding for `n`. |
| `ValDeclStmt n _ e` | Evaluate `e`; insert immutable binding for `n`. |
| `AssignmentStmt lv e` | Evaluate `e`; navigate `lv` and write the value. |
| `IfStmt cond thn mel` | Evaluate `cond`; execute the matching branch. |
| `WhileStmt cond body` | Loop: evaluate `cond`, execute `body` until cond is false or return. |

### How `return` propagates

Statements return `(Maybe Expr, Runtime)`. A `return e` produces `(Just v,
rt)`. `SequenceStmt` checks after each step and stops immediately if it sees
`Just v`:

```
execSequence rt (s:ss) = do
  (ret, rt') <- execStmt rt s
  case ret of
    Just v  -> Right (Just v, rt')   -- short-circuit
    Nothing -> execSequence rt' ss   -- keep going
```

A `while` loop does the same inside its iteration loop.

---

## Mutating Storage via LValues

Assignments to `storage` fields are the primary way a contract persists state.
The interpreter navigates nested field paths using `assignLValue`:

```
storage.pos.x = 10
```

This flattens to root `LStorage` + path `["pos", "x"]`. The interpreter:

1. Reads the current storage record.
2. Navigates to `"pos"`, gets the nested record.
3. Sets `"x"` to `CInt 10` in that nested record.
4. Writes the updated `"pos"` record back into storage.
5. Writes the updated storage back into `rtStorage`.

The same mechanism works for `var` locals of record type: `myRecord.enabled =
false` navigates and updates the local's `Binding` value.

---

## Originate vs Call

Two high-level functions in `Interpreter.hs` correspond to the two CLI
commands:

### `originateWithJsonArgs`

```haskell
originateWithJsonArgs
  :: RepositoryState
  -> Contract
  -> String          -- source text (for address generation)
  -> Value           -- JSON args
  -> Either String (Address, RepositoryState)
```

1. Finds the `@originate` method.
2. Decodes JSON args by parameter name and type.
3. Runs the method body with an empty `rtStorage`.
4. Requires that `rtStorage` is `Just s` after execution (the originate method
   must write storage).
5. Generates a new address from the source hash and the current repo size.
6. Inserts the new `ContractInstance` into the repository map.

### `callEntrypointWithJsonArgs`

```haskell
callEntrypointWithJsonArgs
  :: RepositoryState
  -> Contract
  -> Address
  -> Name            -- entrypoint name
  -> String          -- source text (for hash verification)
  -> Value           -- JSON args
  -> Either String (Maybe Expr, RepositoryState)
```

1. Looks up the address in the repository.
2. Verifies the source hash embedded in the address matches the file on disk
   (see [07-cli.md](07-cli.md)).
3. Checks the contract name matches.
4. Finds the named `@entrypoint`.
5. Decodes JSON args.
6. Runs the method body with the current stored `Expr` as `rtStorage`.
7. Requires that `rtStorage` is still `Just s` after execution.
8. Writes the updated storage back into the repository map.
9. Returns the method's return value (if any).

---

## JSON Serialisation and Deserialisation

Storage values are serialised to JSON when writing `state.json` and
deserialised when loading it.

### `Expr → JSON` (`exprToJson`)

| SmartTS value | JSON |
|---------------|------|
| `CInt n` | number |
| `CBool b` | boolean |
| `Record fields` | object |
| `Unit` | null |

### `JSON → Expr` (`jsonToExprByType`)

The typed decoder uses the contract's declared storage type to guide
decoding. A `TRecord` type causes the decoder to expect a JSON object with
matching field names; each field value is decoded recursively against its
declared type. This ensures that persisted storage is always structurally
valid before the interpreter touches it.

---

## Key Design Decision: `interpretBug` vs `Left`

After a successful type check, certain cases in the interpreter are
impossible. For example, `evalInt` expects a `CInt`; it will never see a
`CBool` in a well-typed program. Rather than returning `Left "internal error"`,
these cases call `interpretBug`, which throws a Haskell `error` with a
message that says "please report". This makes the distinction clear:

- `Left String` — a user-visible error (bad JSON, unknown address, etc.)
- `interpretBug` — an impossible case that indicates a bug in the SmartTS
  implementation itself

---

**What to read next →** [07-cli.md](07-cli.md)
