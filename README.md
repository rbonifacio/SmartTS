# SmartTS

**SmartTS** is a small, TypeScript-inspired language for describing **Tezos-style** smart contracts. You write `.smartts` files with explicit types, `storage` fields, and methods marked with `@originate`, `@entrypoint`, or `@private`. This repository provides a **parser**, an **interpreter**, and a **CLI** that can **originate** contracts into a local “chain” and **call** entrypoints while persisting state in a single JSON file.

> This is a learning / prototyping tool, not a production Michelson compiler.

Contract addresses embed a prefix of **SHA-256** over the UTF-8 source; hashing uses the pure Haskell **[SHA](https://hackage.haskell.org/package/SHA)** package (no `cryptonite` / FFI).

## Features (language)

- `contract Name { storage: { ... }; ... }` with typed fields (`int`, `bool`, `unit`, record types).
- Methods: `@originate`, `@entrypoint`, `@private`; parameters are read-only; return types are explicit.
- **Storage** is updated by assigning to `storage.<field>` (not by returning storage from originate).
- Locals: `var` (mutable) and `val` (immutable).
- Statements: `if` / `while`, blocks, `return`, arithmetic and comparisons, `;`-separated sequences.

## Requirements

- [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/) (e.g. via [GHCup](https://www.haskell.org/ghcup/)).

## Build

```bash
cabal build
```

Run the CLI (after build):

```bash
cabal run smart-ts -- --help   # shows usage if flags are wrong; see below for real commands
```

#### Important: `cabal run` needs `--` before SmartTS flags

Everything after the **first** `--` is passed to `smart-ts`. If you omit it, Cabal tries to parse `--originate`, `--repo`, etc. itself and your program either gets **no** arguments or the wrong ones.

| Wrong | Right |
|--------|--------|
| `cabal run smart-ts --originate --repo ...` | `cabal run smart-ts -- --originate --repo ...` |

If you **install** the binary (`cabal install exe:smart-ts` or copy from `dist-newstyle/...`), you can run it directly with no extra `--`:

```bash
smart-ts --originate --repo ./tezos-sandbox --source ./samples/Counter.smartts --args '{"initialCount": 10}'
```

Run tests:

```bash
cabal test
```

## CLI overview

| Command | Purpose |
|--------|---------|
| **Originate** | Deploy a new contract instance: copies source under `<repo>/contracts/`, runs `@originate`, appends to `state.json`. |
| **Call** | Run an `@entrypoint` on an existing address; `--args` is a JSON object keyed by **parameter names**. |

### Originate

```text
smart-ts --originate --repo <dir> --source <file.smartts> --args '<json>'
```

With **`cabal run`**, prefix the whole line with `cabal run smart-ts -- ` (note the **`--`** before `--originate`).

- Writes `<repo>/state.json` (map of addresses → contract name + storage).
- Saves a copy of the source at `<repo>/contracts/<ContractName>.smartts` (needed later for `--call`).

### Call

```text
smart-ts --call --repo <dir> --address <KT1...> --entrypoint <name> --args '<json>'
```

- Loads the instance from `state.json`, reads `<repo>/contracts/<ContractName>.smartts`, runs the entrypoint with current storage, then saves storage back.

### Addresses and source integrity

New deployments use addresses shaped like **`KT1` + 16 hex characters + `_` + instance number**, where the 16 hex digits are the start of **SHA-256(UTF-8 contract file)**. On **`--call`**, the tool recomputes that hash from the file on disk and **rejects** the call if it does not match the address (so you cannot silently swap in different code under the same address). Older repos that still use name-based addresses like `KT1Counter_0` skip this check.

## Project layout

| Path | Role |
|------|------|
| `lib/SmartTS/AST.hs` | Abstract syntax |
| `lib/SmartTS/Parser.hs` | Megaparsec grammar |
| `lib/SmartTS/Interpreter.hs` | Evaluation and repository state |
| `app/Main.hs` | CLI (`--originate` / `--call`) |
| `samples/Counter.smartts` | Example contract |

## Example scenario: `Counter`

The sample contract `samples/Counter.smartts` defines:

- **Storage:** `count: int`, `enabled: bool`
- **`@originate` `init(initialCount: int)`** — initializes storage from `initialCount`
- **`@entrypoint` `increment(by: int)`** — adds `by` to `count` when `enabled`
- **`@entrypoint` `setEnabled(value: bool)`** — toggles the flag

### 1. Create a repo directory

```bash
mkdir -p ./tezos-sandbox
```

### 2. Originate `Counter`

From the SmartTS project root (adjust paths if needed):

```bash
cabal run smart-ts -- \
  --originate \
  --repo ./tezos-sandbox \
  --source ./samples/Counter.smartts \
  --args '{"initialCount": 10}'
```

The tool prints a new address, e.g. **`Originated contract at address: KT1a1b2c3d4e5f678_0`** — the middle part is derived from the source hash; the final `_0` is the instance index in that repo. Copy the printed address into the next steps.

### 3. Call `increment`

Use the **exact** address from the previous step:

```bash
cabal run smart-ts -- \
  --call \
  --repo ./tezos-sandbox \
  --address KT1a1b2c3d4e5f678_0 \
  --entrypoint increment \
  --args '{"by": 3}'
```

If the entrypoint returns a value, it is printed as **one line of JSON** (here the new `count`). You should see **`13`** as a JSON number.

### 4. Call `setEnabled` (optional)

```bash
cabal run smart-ts -- \
  --call \
  --repo ./tezos-sandbox \
  --address KT1a1b2c3d4e5f678_0 \
  --entrypoint setEnabled \
  --args '{"value": false}'
```

Then `increment` no longer changes `count` until you set `enabled` back to `true`.

### What gets persisted

- **`tezos-sandbox/state.json`** — instances keyed by address (contract name + storage as JSON).
- **`tezos-sandbox/contracts/Counter.smartts`** — copy of the source used when resolving calls.

## License

See [LICENSE](LICENSE).
