# 8 — Testing

This document explains how the SmartTS test suite is organised and how to add
new tests.

---

## Overview

SmartTS has a single test executable (`test/Main.hs`) built with
[Tasty](https://hackage.haskell.org/package/tasty) and
[HUnit](https://hackage.haskell.org/package/HUnit). Run it with:

```bash
cabal test
```

The suite is grouped into two top-level test groups:

| Group | What it tests |
|-------|---------------|
| `Parser Tests` | Parsing of contracts, storage, methods, expressions, statements, and error cases |
| `Type checker` | Type-checking acceptance and rejection of valid and invalid contracts |

---

## Test Helpers

Three helpers reduce boilerplate in every test:

```haskell
-- Parse a string; apply a function to the resulting Contract.
parseSuccess :: String -> (Contract -> Assertion) -> Assertion

-- Parse a string; assert that parsing fails.
parseFailure :: String -> Assertion

-- Parse and type-check a string; assert that both succeed.
typeCheckSuccess :: String -> Assertion

-- Parse and type-check a string; assert that type checking fails.
typeCheckFailure :: String -> Assertion
```

Use `parseSuccess` when you need to inspect the resulting AST. Use
`typeCheckSuccess` / `typeCheckFailure` when you only care about whether the
contract is well-typed.

---

## Parser Tests

### Contract tests

Verify that the top-level contract structure parses correctly: name, storage
fields, number of methods.

```haskell
testCase "Simple contract with storage and method" $
  parseSuccess
    "contract MyContract { storage: { x: int }; \
    \@originate init(): int { return 0; } }"
    $ \contract ->
      case contract of
        Contract "MyContract" [("x", TInt)]
                 [MethodDecl Originate "init" [] TInt _] -> return ()
        _ -> assertFailure $ "Unexpected: " ++ show contract
```

### Expression tests

Verify AST shapes for literals, operators, field access, and record literals.
Use pattern matching on the deeply nested result:

```haskell
testCase "Chained addition" $
  parseSuccess "contract Test { storage: { x: int }; \
               \@entrypoint test(): int { return 1 + 2 + 3; } }"
    $ \contract ->
      case contract of
        Contract _ _ [MethodDecl _ _ _ _
          (SequenceStmt [ReturnStmt expr])] ->
            case expr of
              Add (Add (CInt 1) (CInt 2)) (CInt 3) -> return ()
              _ -> assertFailure $ "Expected left-assoc: " ++ show expr
        _ -> assertFailure "Unexpected structure"
```

### Statement tests

Verify `if`/`else`, `while`, `var`/`val` declarations, storage reads and
writes, field assignments (`x.a = …`), and nested field assignments
(`x.a.b = …`).

### Error tests

Verify that invalid inputs are rejected:

```haskell
testCase "Missing contract keyword" $
  parseFailure "MyContract { storage: { x: int }; ... }"

testCase "Missing return type" $
  parseFailure "contract Test { storage: { x: int }; \
               \@entrypoint test() { return 0; } }"
```

---

## Type Checker Tests

Each test provides a complete (parseable) contract and asserts acceptance or
rejection:

```haskell
testCase "Minimal well-typed contract" $
  typeCheckSuccess
    "contract C { storage: { x: int }; @originate init(): int { return 0; } }"

testCase "Return type mismatch" $
  typeCheckFailure
    "contract C { storage: { x: int }; @originate init(): int { return true; } }"

testCase "Cannot assign to val" $
  typeCheckFailure
    "contract C { storage: { x: int }; \
    \@originate init(): int { val v: int = 1; v = 2; return 0; } }"
```

The test also verifies JSON storage decoding directly via
`contractInstanceFromStorageValue`:

```haskell
testCase "Persisted storage decodes against contract storage type" $
  parseSuccess
    "contract C { storage: { n: int, b: bool }; \
    \@originate init(): unit { return (); } }"
    $ \c ->
      case contractInstanceFromStorageValue c
             (object ["n" .= (1 :: Int), "b" .= True]) of
        Left err -> assertFailure err
        Right (ContractInstance _ st) ->
          case st of
            Record [("n", CInt 1), ("b", CBool True)] -> return ()
            _ -> assertFailure $ "unexpected storage: " ++ show st
```

---

## Adding a New Test

### Adding a parser test

**Step 1** — Decide which group it belongs to (`contractTests`,
`storageTests`, `methodTests`, `expressionTests`, `statementTests`, or
`errorTests`).

**Step 2** — Add a `testCase` to that group:

```haskell
, testCase "Modulo expression" $
    parseSuccess
      "contract Test { storage: { x: int }; \
      \@entrypoint mod(): int { return 10 % 3; } }"
      $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _
            (SequenceStmt [ReturnStmt (Mod (CInt 10) (CInt 3))])] -> return ()
          _ -> assertFailure $ "Expected Mod, got: " ++ show contract
```

**Step 3** — Run `cabal test` to verify it passes.

### Adding a type checker test

**Step 1** — Open `test/Main.hs` and find `typeCheckTests`.

**Step 2** — Add a `testCase`:

```haskell
, testCase "Equality on records requires identical field order" $
    typeCheckFailure
      "contract C { storage: { x: int }; \
      \@originate init(): bool { return { a: 1, b: 2 } == { b: 2, a: 1 }; } }"
```

**Step 3** — Run `cabal test`.

---

## Naming Conventions

- Test names: plain English description of the scenario being tested, e.g.
  `"Chained field access (x.a.b)"`, `"Cannot assign to val"`.
- No prefix conventions are enforced; keep names short and descriptive.

---

## Worked Example: Testing a New Storage Mutation

Suppose you add a `@private` method and want to test that field assignment
through a local var is parsed and type-checks correctly.

**Step 1** — Add the parser test in `statementTests`:

```haskell
, testCase "Field assignment through local record var" $
    parseSuccess
      "contract Test { storage: { x: { a: int } }; \
      \@private helper(): int { var t: { a: int } = x; t.a = 7; return t.a; } }"
      $ \contract ->
        case contract of
          Contract _ _ [MethodDecl Private "helper" [] TInt
            (SequenceStmt
              [ VarDeclStmt "t" (TRecord [("a", TInt)]) (Var "x")
              , AssignmentStmt (LField (LVar "t") "a") (CInt 7)
              , ReturnStmt (FieldAccess (Var "t") "a")
              ])] -> return ()
          _ -> assertFailure $ "Got: " ++ show contract
```

**Step 2** — Add the type checker test in `typeCheckTests`:

```haskell
, testCase "Field assignment through local var is well-typed" $
    typeCheckSuccess
      "contract C { storage: { x: { a: int } }; \
      \@private helper(): int { var t: { a: int } = x; t.a = 7; return t.a; } }"
```

**Step 3** — Run `cabal test` and confirm both pass.

---

**What to read next →** [README.md](../README.md)
