# 9 — SmartTS Evolution Projects

This document proposes ten self-contained extension projects for the SmartTS
language. Each project touches all three pipeline stages — parser, type
checker, and interpreter — and requires you to reason carefully about syntax,
typing rules, and runtime semantics. They are designed to have a similar degree
of complexity so that any project is a fair choice regardless of which one you
pick.

## Assignment of the projets to the groups

The allocation of projects to groups was performed randomly, using a reproducible procedure (i.e., with a fixed random seed). 

| Project    | Students |
|------------|--------|
| Projeto 2  | Renan Guilherme Siqueira de Araújo<br>Heitor Brayner Prado|
| Projeto 7  | Emanoel Thyago Cordeiro dos Santos<br>Abhner Adriel Cristovao Silva<br>Emmanuel Silva Nascimento<br>Davi Vicente Magnata<br>Igor Fragoso Peixoto Lopes de Melo |
| Projeto 5  | João Marcelo de Souza Motta<br>Thiago Henrique Tomais de Oliveira<br>Vlademir José Montenegro de Melo |
| Projeto 4  | Juliana Talita<br>João Marrocos<br>Luana Queiroz<br>Pedro Elias<br>Welton Felix |

The following projects were not assigned.

   * Project 1
   * Project 3
   * Project 6
   * Project 8
   
---

## Project 1 — `nat`, `string`, AND `option<T>` Types

Add two new scalar types: `nat` for non-negative integers (a Michelson
primitive for token balances) and `string` for text values. Both require only
new base-type variants in the AST; neither introduces composite structure.
Add optional values with `Some` and `None` constructors and a dedicated
`match_option` statement for safe unwrapping. In Michelson, `option` is the
canonical way to represent missing or nullable values.

### Learning objectives
- Add two independent base types end-to-end, experiencing how each pipeline stage must be touched even for modest additions.
- Implement a runtime invariant (`nat` non-negativity) that cannot be verified statically.
- Relate `nat` to Tezos token balances and `string` to contract metadata and error messages.
- Implement a polymorphic type constructor — a type that takes a type argument.
- Understand how pattern matching binds names in a branch-local scope.
- Appreciate why `option` is preferred over nullable values in statically typed languages.

---

## Project 2 — `pair<T, U>` and Enum Types

Add a two-element product type modelled on Michelson's `Pair`, with a
constructor expression, `fst`/`snd` accessor builtins, and a destructuring
declaration form. Add user-defined enumeration types whose variants can be used as values,
compared for equality, and dispatched on with a `match` statement. Enums are a
restricted form of Michelson's `or` (sum) type, which encodes the set of
available entrypoints in a deployed contract.


### Learning objectives
- Add a parameterised composite type that is structural rather than nominal.
- Implement destructuring as a declaration that simultaneously creates two bindings.
- Learn how nominal types are represented in a type registry that is separate from the symbol table.
- Implement exhaustiveness checking: a missing case is a compile-time error, not a silent fallthrough.

---
## Project 3 — `list<T>` Type

Add an immutable linked-list type with literal syntax, built-in operations
(`cons`, `head`, `tail`, `size`), and a `for_each` iteration statement.

### Learning objectives
- Implement the first collection type, where the element type is a parameter.
- Understand why `head` and `tail` cannot be made statically safe without tracking list emptiness in the type system (a non-trivial extension beyond this project).
- Relate `list`, `cons`, and `for_each` to Michelson's `LIST`, `CONS`, and `ITER` instructions.

---

## Project 4 — `map<K, V>` Type

Add a finite key–value map type with built-in operations for insertion, lookup,
removal, and membership testing. Finite maps are a fundamental Tezos data
structure: Michelson provides both `map` (for small collections) and `big_map`
(for large, lazily-loaded ones). We will focus only on maps.

### Learning objectives
- Implement a parameterised collection type whose internal representation uses an ordered map from the host language.
- Enforce a *comparability* constraint on the key type — a restriction the type checker must check before the interpreter runs.
- Relate `map<K, V>` to Michelson's `MAP` and `BIG_MAP` types, and understand why Tezos needs both.

---
## Project 5 — `fail_with` and `require`

Add two contract-abortion forms: `fail_with(expr)` (unconditional abort with a
payload) and `require(condition, expr)` (conditional guard). Both model
Michelson's `FAILWITH` instruction, which is the primary error-handling
mechanism in deployed Tezos contracts.

### Learning objectives
- Implement a statement that is valid in any return-type context — the type-theoretic concept of an expression that never produces a value.
- Model Tezos's `FAILWITH`, and understand why it carries a payload rather than a simple error code.
- See how `require` can be desugared at the interpreter level to avoid duplicating logic.

---

## Project 6 — `for` Loop

Add a traditional three-part `for` statement as a more expressive alternative
to `while`, useful for counted iterations common in smart-contract arithmetic.


### Learning objectives
- Model scoping rules that are tighter than a plain block: the loop variable is out of scope after the `for`.
- Understand how desugaring a `for` into a `while` can simplify the interpreter at the cost of a less informative AST.
- Practice adding a new `Stmt` constructor without breaking existing parser and interpreter cases.

---
## Project 7 — `@view` Decorator

Add a `@view` decorator for read-only methods. The type checker enforces that
`@view` bodies never write to `storage`, making them safe to query without
altering contract state.

### Learning objectives
- Understand how a method-level property is enforced by threading a boolean flag through the type-checker environment.
- See how the same interpreter code can serve two execution modes (mutating and read-only) by varying only the persistence step.
- Relate `@view` to Tezos on-chain views, which let external contracts read state without triggering a transaction.

---

## Project 8 — `@test` Methods and `--test` Mode

Add first-class test declarations using a `@test` decorator, an `assert`
statement, and a `--test` CLI mode that originates a contract and exercises
its entrypoints.

### Learning objectives
- See how a contract language can embed a testing discipline directly in its syntax.
- Implement a new execution mode that reuses the existing originate and call infrastructure.
- Handle partial failure — some tests pass, others fail — and produce a human-readable report.

---

## Instructions

All projects require changes to the parser, the type checker, and the
interpreter. A good starting point for any of them is:

1. Write two or three SmartTS contracts that exercise the new feature.
2. Decide how the concrete syntax of the language will be impacted, and discuss this 
   using the Google Classroom. 
3. Decide how the new syntax will be represented as new constructors in `Expr`,
   `Stmt`, or `Type` in `lib/SmartTS/AST.hs`.
4. Add the AST nodes and adjust `lib/SmartTS/Parser.hs` to produce them.
5. Extend `lib/SmartTS/TypeCheck.hs` to validate the new nodes.
6. Extend `lib/SmartTS/Interpreter.hs` to execute them.
7. Write unit tests in `test/` and add end-to-end example contracts in
   `samples/`.

For projects that also touch the CLI (`@view`, `@test`), update `app/Main.hs`
as well. 


## Deadlines

### First Milestone: 

   * Review the concrete syntax (grammar) and the AST and Parser components.
   * Deadline: 19/04
   
### Second Milestone: 

   * Review the implementation of the type checker and interpreter
   * Deadline: 11/05

### Third Milestone: 

   * Review the implementation of the type code generator
   * Deadline: 30/06

The outcomes of the projects must be submitted via pull-requests.
