# SmartTS

SmartTS is a small, TypeScript-inspired language for describing **Tezos-style
smart contracts**. It is designed as a learning and prototyping tool: the entire
pipeline — parser, type checker, and interpreter — is intentionally small so
you can read and understand every part of it. A complete SmartTS contract looks
like this:

```typescript
contract Counter {
  storage: {
    count: int,
    enabled: bool
  };

  @originate
  init(initialCount: int): unit {
    storage.count = initialCount;
    storage.enabled = true;
    return ();
  }

  @entrypoint
  increment(by: int): int {
    if (storage.enabled) {
      storage.count = storage.count + by;
    }
    return storage.count;
  }
}
```

---

## Quick Start

```bash
cabal build          # compile the project
cabal test           # run the test suite
```

Deploy a contract and call an entrypoint (note the `--` before SmartTS flags):

```bash
# originate
cabal run smart-ts -- \
  --originate --repo ./tezos-sandbox \
  --source ./samples/Counter.smartts \
  --args '{"initialCount": 10}'

# call (replace KT1... with the address printed above)
cabal run smart-ts -- \
  --call --repo ./tezos-sandbox \
  --address KT1... --entrypoint increment \
  --args '{"by": 3}'
```

---

## CLI Overview

The binary accepts two commands:

```
smart-ts --originate --repo <dir> --source <file.smartts> --args '<json>'
smart-ts --call      --repo <dir> --address <KT1...> --entrypoint <name> --args '<json>'
```

**Important:** when using `cabal run`, every SmartTS flag must come after a
bare `--`:

| Wrong | Right |
|-------|-------|
| `cabal run smart-ts --originate ...` | `cabal run smart-ts -- --originate ...` |

See [docs/07-cli.md](docs/07-cli.md) for full CLI reference, address format,
and persistence details.

---

## Project Layout

```
lib/SmartTS/
├── AST.hs          # data types for the contract representation
├── Parser.hs       # Megaparsec grammar
├── TypeCheck.hs    # static type checker
└── Interpreter.hs  # tree-walking interpreter and repository state

app/
└── Main.hs         # CLI (--originate / --call) and JSON persistence

test/
└── Main.hs         # HUnit test suite

samples/
└── Counter.smartts # example contract

tezos-sandbox/      # local "chain" (state.json + contracts/)
docs/               # this documentation
```

---

## Documentation

Read the documents in order for a complete picture of the project, or jump
directly to the topic you need.

| # | File | What you will learn |
|---|------|---------------------|
| 1 | [Language reference](docs/01-language.md) | What you can write in SmartTS: contracts, types, methods, expressions |
| 2 | [Pipeline overview](docs/02-pipeline.md) | How source code travels from text to a running contract |
| 3 | [The AST](docs/03-ast.md) | How a SmartTS contract is represented in memory |
| 4 | [The parser](docs/04-parser.md) | How source text is turned into an AST |
| 5 | [The type checker](docs/05-type-checker.md) | How type errors are caught before execution |
| 6 | [The interpreter](docs/06-interpreter.md) | How a type-checked contract is executed |
| 7 | [The CLI and persistence](docs/07-cli.md) | How contracts are deployed, called, and saved to disk |
| 8 | [Testing](docs/08-testing.md) | How the test suite is organised and how to add tests |
