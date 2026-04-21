# Technical Documentation — `viewTests` in SmartTS

## Overview

This documentation describes, in a detailed and organized way, the role of the `viewTests` test group within the **SmartTS** project's test suite, with a focus on **Project 7 — `@view` Decorator**.

The goal of this documentation is to explain:

- where `viewTests` fits into the testing architecture;
- why it was included in the project;
- what each test validates;
- which system behavior is being guaranteed;
- what is expected from the `@view` decorator;
- how the parser, type checker, and interpreter participate in this validation;
- the academic and practical relevance of this test group.

---

## 1. Project context

**SmartTS** is a TypeScript-inspired language for describing Tezos-style smart contracts. The project's implementation is organized into three major stages:

1. **Parser** — interprets the contract syntax;
2. **Type Checker** — validates semantic and typing rules;
3. **Interpreter** — executes contracts and simulates their runtime behavior.

**Project 7** introduces the `@view` decorator, whose purpose is to represent **read-only methods**.

Conceptually, a method marked with `@view`:

- can inspect the contract state;
- can return values derived from that state;
- **cannot modify `storage`**;
- must be safe to query without altering the contract's persisted state.

This means the `@view` feature is not just a syntax change. It directly affects:

- the language **grammar**;
- the **typing rules**;
- the **interpreter's behavior**.

For this reason, adding `@view` requires a dedicated test suite.

---

## 2. Where `viewTests` appears in the suite

The snippet below shows the main structure of the tests:

```haskell
tests :: TestTree
tests =
  testGroup
    "SmartTS"
    [ testGroup
        "Parser Tests"
        [ contractTests
        , storageTests
        , methodTests
        , expressionTests
        , statementTests
        , errorTests
        ]
    , typeCheckTests
    , viewTests
    ]
```

### Structural interpretation

The test suite is organized as a tree (`TestTree`) using the **Tasty** library. In this tree:

- `Parser Tests` groups tests related to syntactic analysis;
- `typeCheckTests` groups semantic and typing validations;
- `viewTests` was added as a new group specialized in the `@view` feature.

### Practical consequence

By including `viewTests` in the main test tree, the project now automatically runs the `@view` tests together with the others when executing, for example:

```bash
cabal test
```

In other words, `viewTests` became an official part of the system's validation strategy.

---

## 3. Main purpose of `viewTests`

The `viewTests` group was created to ensure that the `@view` feature is correct across all relevant layers of the compiler/interpreter.

More specifically, it validates four fundamental properties:

1. **The parser accepts methods annotated with `@view`;**
2. **The type checker prevents `storage` mutation inside `@view`;**
3. **Executing a `view` returns values correctly without changing the repository;**
4. **A `view` can read state that was updated by an `@entrypoint`, while still causing no mutation.**

These four properties cover the feature end to end.

---

## 4. Full `viewTests` code

```haskell
viewTests :: TestTree
viewTests =
  testGroup
    "@view Tests"
    [ testCase "Parser accepts @view method" $
        parseSuccess
          "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view get(): int { return storage.count; } }"
          $ \contract ->
              assertBool
                "Expected a method named get"
                (any (\m -> methodName m == "get") (contractMethods contract))

    , testCase "Type checker rejects storage mutation inside @view" $
        typeCheckFailureWithMessage
          "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view bad(): unit { storage.count = 10; return (); } }"
          "@view method cannot modify storage."

    , testCase "@view returns value without changing repository" $
        parseSuccess
          "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view get(): int { return storage.count; } }"
          $ \contract ->
              case typeCheckContract contract of
                Left err -> assertFailure $ "Type check failed: " ++ err
                Right () ->
                  case originateWithJsonArgs mempty contract
                        "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view get(): int { return storage.count; } }"
                        (object ["n" .= (5 :: Int)]) of
                    Left err -> assertFailure $ "Originate failed: " ++ err
                    Right (addr, repo1) ->
                      case callViewWithJsonArgs repo1 contract addr "get"
                            "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view get(): int { return storage.count; } }"
                            (object []) of
                        Left err -> assertFailure $ "View failed: " ++ err
                        Right (ret, repo2) -> do
                          assertEqual "View return value" (Just (CInt 5)) ret
                          assertEqual "Repository must not change after @view" repo1 repo2

    , testCase "Entrypoint mutates storage and @view reads updated value" $
        parseSuccess
          "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @entrypoint inc(by: int): int { storage.count = storage.count + by; return storage.count; } @view get(): int { return storage.count; } }"
          $ \contract ->
              case typeCheckContract contract of
                Left err -> assertFailure $ "Type check failed: " ++ err
                Right () ->
                  case originateWithJsonArgs mempty contract
                        "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @entrypoint inc(by: int): int { storage.count = storage.count + by; return storage.count; } @view get(): int { return storage.count; } }"
                        (object ["n" .= (5 :: Int)]) of
                    Left err -> assertFailure $ "Originate failed: " ++ err
                    Right (addr, repo1) ->
                      case callEntrypointWithJsonArgs repo1 contract addr "inc"
                            "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @entrypoint inc(by: int): int { storage.count = storage.count + by; return storage.count; } @view get(): int { return storage.count; } }"
                            (object ["by" .= (2 :: Int)]) of
                        Left err -> assertFailure $ "Entrypoint failed: " ++ err
                        Right (ret1, repo2) -> do
                          assertEqual "Entrypoint return value" (Just (CInt 7)) ret1
                          case callViewWithJsonArgs repo2 contract addr "get"
                                "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @entrypoint inc(by: int): int { storage.count = storage.count + by; return storage.count; } @view get(): int { return storage.count; } }"
                                (object []) of
                            Left err -> assertFailure $ "View failed: " ++ err
                            Right (ret2, repo3) -> do
                              assertEqual "View sees updated value" (Just (CInt 7)) ret2
                              assertEqual "View still must not mutate repository" repo2 repo3
    ]
```

---

## 5. Conceptual explanation of the components used

Before analyzing the tests individually, it is important to understand the role of the main elements used in the code.

### 5.1 `TestTree`

`TestTree` represents a tree of tests. In Tasty, the entire suite is modeled this way to allow hierarchical grouping.

### 5.2 `testGroup`

`testGroup` creates a logical grouping of tests.

Example:

```haskell
testGroup "@view Tests" [ ... ]
```

This creates a section named `@view Tests` within the suite.

### 5.3 `testCase`

`testCase` defines a specific unit test with its own name.

Example:

```haskell
testCase "Parser accepts @view method" $ ...
```

### 5.4 `parseSuccess`

`parseSuccess` is a helper that:

1. tries to parse a contract string;
2. explicitly fails if the parser rejects the contract;
3. if parsing succeeds, passes the resulting contract to the test assertion.

In short, it guarantees that the contract is syntactically valid.

### 5.5 `typeCheckContract`

`typeCheckContract` performs semantic and type validation on an already parsed contract.

It checks, for example:

- consistency between declared types and returned expressions;
- restrictions on writing to `storage`;
- safety rules introduced by the `@view` feature.

### 5.6 `typeCheckFailureWithMessage`

This helper verifies not only that type checking fails, but also that the produced error message is exactly the expected one.

This increases the precision of the test: instead of accepting “any error,” it requires the correct semantic error.

### 5.7 `originateWithJsonArgs`

This function simulates the **origination** of a contract, that is, the creation of a concrete contract instance in the repository with initial arguments.

### 5.8 `callEntrypointWithJsonArgs`

Executes an `@entrypoint` method, that is, a method that may alter the contract state.

### 5.9 `callViewWithJsonArgs`

Executes an `@view` method, whose expected behavior is:

- inspect the current contract state;
- return a value;
- not alter the persisted repository.

### 5.10 `repo1`, `repo2`, `repo3`

These names represent different versions of the repository throughout execution.

- `repo1`: state after origination;
- `repo2`: state after a later call;
- `repo3`: state after another call.

Comparing these values is how the tests prove whether state mutation happened or not.

---

## 6. Detailed analysis of each test

## 6.1 Test 1 — Parser accepts `@view` method

```haskell
testCase "Parser accepts @view method" $
  parseSuccess
    "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view get(): int { return storage.count; } }"
    $ \contract ->
        assertBool
          "Expected a method named get"
          (any (\m -> methodName m == "get") (contractMethods contract))
```

### Purpose

To ensure that the parser correctly recognizes a method decorated with `@view`.

### What it checks

The contract contains:

- a `storage` with a field `count: int`;
- an `@originate init(...)` method for initialization;
- an `@view get(): int` method that returns `storage.count`.

After parsing, the test traverses the contract's method list (`contractMethods`) and verifies that there is indeed a method named `get`.

### Why this matters

Introducing `@view` changes the language. Therefore, the parser must be able to recognize this new decorator without error.

### Expected behavior

This test should pass whenever the grammar has been correctly updated to accept `@view`.

### What failure means

If this test fails, there is strong evidence that:

- the parser does not recognize `@view`;
- or the method is not being properly recorded in the AST.

---

## 6.2 Test 2 — Type checker rejects storage mutation inside `@view`

```haskell
testCase "Type checker rejects storage mutation inside @view" $
  typeCheckFailureWithMessage
    "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view bad(): unit { storage.count = 10; return (); } }"
    "@view method cannot modify storage."
```

### Purpose

To ensure that the type checker enforces the main semantic restriction of `@view`: **no writes to storage**.

### What the contract does

The method `bad()` is marked as `@view`, but it tries to execute:

```haskell
storage.count = 10;
```

This is an explicit state mutation.

### Expected behavior

The contract must be rejected by the type checker with the message:

```text
@view method cannot modify storage.
```

### Why this matters

Without this test, the `@view` feature could exist only in name while still allowing writes to `storage`, which would break the purpose of the feature.

### Academic value

This test clearly shows how a high-level language property (“read-only method”) is transformed into a static rule enforced by the type checker.

---

## 6.3 Test 3 — `@view` returns value without changing repository

```haskell
testCase "@view returns value without changing repository" $
  parseSuccess
    "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @view get(): int { return storage.count; } }"
    $ \contract ->
        case typeCheckContract contract of
          Left err -> assertFailure $ "Type check failed: " ++ err
          Right () ->
            case originateWithJsonArgs mempty contract ... (object ["n" .= (5 :: Int)]) of
              Left err -> assertFailure $ "Originate failed: " ++ err
              Right (addr, repo1) ->
                case callViewWithJsonArgs repo1 contract addr "get" ... (object []) of
                  Left err -> assertFailure $ "View failed: " ++ err
                  Right (ret, repo2) -> do
                    assertEqual "View return value" (Just (CInt 5)) ret
                    assertEqual "Repository must not change after @view" repo1 repo2
```

### Purpose

To ensure, at runtime, that `@view`:

1. returns the correct value;
2. does not alter the persisted state.

### Execution flow

#### Step 1 — Parse

The contract is parsed successfully.

#### Step 2 — Type check

The contract must be considered semantically valid.

#### Step 3 — Origination

The call:

```haskell
originateWithJsonArgs ... (object ["n" .= (5 :: Int)])
```

creates a contract instance initializing `count = 5`.

#### Step 4 — View execution

The call:

```haskell
callViewWithJsonArgs repo1 contract addr "get" ... (object [])
```

executes the `get()` view on the already originated contract.

#### Step 5 — Assertions

Two assertions are made:

```haskell
assertEqual "View return value" (Just (CInt 5)) ret
assertEqual "Repository must not change after @view" repo1 repo2
```

This guarantees that:

- the view correctly read the value `5`;
- the execution produced no mutation in the repository.

### What this test means

This is the test that most clearly captures the expected behavior of a view: **read without modifying**.

### What failure means

If it fails, then at least one of the following is true:

- the view is not returning the correct value;
- the view is changing state, which it should not;
- the integration between parser, type checker, and interpreter for this feature is inconsistent.

---

## 6.4 Test 4 — Entrypoint mutates storage and `@view` reads updated value

```haskell
testCase "Entrypoint mutates storage and @view reads updated value" $
  parseSuccess
    "contract Counter { storage: { count: int }; @originate init(n: int): unit { storage = { count: n }; return (); } @entrypoint inc(by: int): int { storage.count = storage.count + by; return storage.count; } @view get(): int { return storage.count; } }"
    $ \contract ->
        case typeCheckContract contract of
          Left err -> assertFailure $ "Type check failed: " ++ err
          Right () ->
            case originateWithJsonArgs mempty contract ... (object ["n" .= (5 :: Int)]) of
              Left err -> assertFailure $ "Originate failed: " ++ err
              Right (addr, repo1) ->
                case callEntrypointWithJsonArgs repo1 contract addr "inc" ... (object ["by" .= (2 :: Int)]) of
                  Left err -> assertFailure $ "Entrypoint failed: " ++ err
                  Right (ret1, repo2) -> do
                    assertEqual "Entrypoint return value" (Just (CInt 7)) ret1
                    case callViewWithJsonArgs repo2 contract addr "get" ... (object []) of
                      Left err -> assertFailure $ "View failed: " ++ err
                      Right (ret2, repo3) -> do
                        assertEqual "View sees updated value" (Just (CInt 7)) ret2
                        assertEqual "View still must not mutate repository" repo2 repo3
```

### Purpose

To ensure the correct coexistence between:

- methods that **modify** state (`@entrypoint`);
- methods that **only read** state (`@view`).

### Modeled scenario

The `Counter` contract has:

- `count` as a persisted storage value;
- an `@entrypoint inc(by: int)` that adds `by` to `count`;
- an `@view get()` that returns the current value of `count`.

### Execution flow

#### Step 1 — Origination

The contract starts with `count = 5`.

#### Step 2 — Entrypoint call

`inc(by = 2)` is executed.

Expected result:

- return value: `7`;
- new persisted state: `count = 7`.

#### Step 3 — View call

`get()` is called after the mutation.

Expected result:

- the view must see `7`;
- the repository after the view must remain equal to the repository before the view.

### Two important guarantees of this test

#### Guarantee 1 — `@entrypoint` alters the state

The assertion:

```haskell
assertEqual "Entrypoint return value" (Just (CInt 7)) ret1
```

shows that the increment was effectively applied.

#### Guarantee 2 — `@view` reads the updated state but does not modify it

The final assertions show that:

- the view observes the updated value `7`;
- the view call does not cause new persistence or mutation.

### Importance of this test

This is the most realistic scenario in the group, because it demonstrates the clear separation between:

- **write operations**;
- **read operations**.

This exact separation is what justifies the existence of the `@view` decorator.

---

## 7. What `viewTests` proves about the system

Taken together, the four tests show that the implementation of `@view` is coherent across all phases of the pipeline.

### 7.1 In the parser

`@view` is accepted as a valid part of the language syntax.

### 7.2 In the type checker

`@view` receives a special semantic restriction: **it cannot modify `storage`**.

### 7.3 In the interpreter

A view:

- executes correctly;
- returns correct values;
- does not alter the repository.

### 7.4 In component integration

The feature works consistently from start to finish:

- the contract is written;
- it is parsed;
- it is type-checked;
- it is originated;
- it is mutated by an entrypoint when allowed;
- it is queried by a view without later mutation.

---

## 8. Why this test group was included

The inclusion of `viewTests` is not just desirable: it is necessary.

Whenever a new language feature is introduced, especially one with semantic impact, it must be demonstrated as correct through automated tests.

In the case of `@view`, the need is even greater because the feature defines a safety property:

> view methods must not alter state.

Without a group like `viewTests`, the system would be vulnerable to regressions such as:

- the parser stopping accepting `@view` after a future refactor;
- the type checker forgetting to block writes to storage;
- the interpreter starting to persist improper changes after a view call;
- the view failing to correctly reflect the updated contract state.

Thus, `viewTests` also serves as a **regression prevention mechanism**.

---

## 9. What is expected when all tests pass

When `viewTests` passes completely, the technical interpretation is the following:

- the language recognizes `@view` correctly;
- `@view` is treated as a read-only method;
- semantic checking is enforcing the correct rule;
- the interpreter respects the difference between reading and writing;
- views return the correct value without modifying state;
- entrypoints may modify state and views can observe that modification afterward.

In project terms, this means the feature was successfully integrated into SmartTS.

---

## 10. What a failure in `viewTests` may indicate

Depending on which test fails, the interpretation changes.

### Failure in test 1

Likely a parser or AST issue related to `@view`.

### Failure in test 2

Likely an issue in the type checker semantic rule: improper mutations are being accepted in read-only methods.

### Failure in test 3

Likely an issue in the interpreter or repository persistence mechanism for view calls.

### Failure in test 4

Likely an inconsistency in the integration between entrypoints and views, especially in state update and later state reading.

---

## 11. Its relevance

From a technical perspective, `viewTests` is an excellent example of how an abstract programming language property is refined into verifiable requirements.

The idea of a “read-only method” appears here at three levels:

1. **Syntactic level** — the language must recognize `@view`;
2. **Static semantic level** — the type checker must restrict illegal behavior;
3. **Operational level** — the runtime must execute the query without persistent side effects.

This separation is valuable because it highlights the difference between:

- defining a feature in the language;
- formally guaranteeing its expected behavior through tests.

---

## 12. Conclusion

The `viewTests` group was included to validate the implementation of the `@view` decorator in a complete, professional, and reliable way.

It does not merely test whether the feature exists, but whether it respects its central semantics:

- **`@view` must be accepted by the language;**
- **`@view` must not allow `storage` mutation;**
- **`@view` must return correct values;**
- **`@view` must observe the current contract state;**
- **`@view` must not alter the repository after the query.**

Therefore, `viewTests` plays a critical role in the project:

> it formalizes, through automated tests, the expected behavioral contract of the `@view` feature.

In other words, this test group turns the conceptual idea of “safe, non-mutating query” into a concrete system guarantee.

---

## 13. In summary

`viewTests` is the test group responsible for validating the `@view` feature in SmartTS. It ensures that `@view` methods are accepted by the parser, rejected by the type checker when they attempt to modify `storage`, executed as read-only operations by the interpreter, and able to correctly inspect the updated contract state without causing mutation in the repository.
