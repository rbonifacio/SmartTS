# 7 — The CLI and Persistence

This document covers the CLI in detail: flag syntax, what each command does,
how contracts are addressed, and how state is saved to and loaded from disk.

---

## Building and Running

```bash
cabal build          # compile
cabal test           # run tests
```

After building, the binary is in `dist-newstyle/…/smart-ts`. You can run it
directly or via `cabal run`. **When using `cabal run`, you must separate
SmartTS flags from Cabal flags with `--`:**

```bash
# Wrong — Cabal tries to interpret --originate itself
cabal run smart-ts --originate --repo ./tezos-sandbox ...

# Right — everything after -- goes to smart-ts
cabal run smart-ts -- --originate --repo ./tezos-sandbox ...
```

If you install the binary (`cabal install exe:smart-ts`), no `--` is needed.

---

## The Two Commands

### Originate

Deploy a new contract instance:

```
smart-ts --originate
  --repo    <directory>       # where state.json and contracts/ live
  --source  <file.smartts>    # the contract source to deploy
  --args    '<json object>'   # arguments for the @originate method
```

**What happens:**

1. The source file is parsed and type-checked.
2. The `--args` JSON is decoded using the `@originate` method's parameter
   types.
3. Existing state is loaded from `<repo>/state.json` (if it exists).
4. The `@originate` method runs. Its storage assignments produce the initial
   storage value.
5. A new address is generated (see [Addresses](#addresses) below).
6. The updated state is written back to `<repo>/state.json`.
7. A copy of the source is saved at `<repo>/contracts/<ContractName>.smartts`.
8. The new address is printed to stdout.

**Example:**

```bash
cabal run smart-ts -- \
  --originate \
  --repo ./tezos-sandbox \
  --source ./samples/Counter.smartts \
  --args '{"initialCount": 10}'
# → Originated contract at address: KT1b3cdccc8947ba2f9_0
```

---

### Call

Invoke an entrypoint on an existing contract instance:

```
smart-ts --call
  --repo        <directory>   # same repo used for originate
  --address     <KT1...>      # address printed by originate
  --entrypoint  <name>        # name of the @entrypoint method
  --args        '<json>'      # arguments for the entrypoint
```

**What happens:**

1. State is loaded from `<repo>/state.json`.
2. The address is looked up in the state.
3. The contract source at `<repo>/contracts/<ContractName>.smartts` is read,
   parsed, and type-checked.
4. The source hash is verified against the address (see below).
5. The `--args` JSON is decoded against the entrypoint's parameter types.
6. The entrypoint runs with the current stored state.
7. Updated storage is written back to `state.json`.
8. If the method returns a value, it is printed as one line of JSON. Otherwise
   `"Call completed."` is printed.

**Example:**

```bash
cabal run smart-ts -- \
  --call \
  --repo ./tezos-sandbox \
  --address KT1b3cdccc8947ba2f9_0 \
  --entrypoint increment \
  --args '{"by": 3}'
# → 13
```

---

## Addresses

Every new deployment gets an address shaped like:

```
KT1<16 hex chars>_<instance index>
```

The 16 hex characters are the **first 16 characters of the SHA-256 hash**
of the UTF-8 source text. The instance index counts how many contracts are
already in that repository (`0` for the first, `1` for the second, etc.).

For example, if the source hashes to `b3cdccc8947ba2f9…`, the first
deployment in the repo gets address `KT1b3cdccc8947ba2f9_0`.

### Source integrity check

When calling an entrypoint, the CLI recomputes the SHA-256 of the contract
source on disk and compares the first 16 hex characters to those embedded in
the address. If they do not match, the call is rejected:

```
Contract source on disk does not match the code hash in the address
(embedded b3cdccc8947ba2f9...; file hashes to a1b2c3d4e5f67890...).
Restore the original source or originate a new instance.
```

This prevents silently calling a different contract under the same address.
If you change the source, originate a new instance to get a new address that
reflects the updated code.

**Legacy addresses** of the form `KT1ContractName_0` (used before this scheme
was introduced) skip the hash check and are accepted unconditionally.

---

## Repository Layout

A repository directory contains:

```
<repo>/
├── state.json          # all contract instances (addresses → name + storage)
└── contracts/
    ├── Counter.smartts # copy of each deployed contract's source
    └── …
```

### `state.json` format

```json
{
  "instances": {
    "KT1b3cdccc8947ba2f9_0": {
      "contractName": "Counter",
      "storage": {
        "count": 10,
        "enabled": true
      }
    }
  }
}
```

Each instance records its contract name (used to find the source file) and
its current storage as a JSON value.

---

## Repository Validation on Startup

Every command re-validates the entire repository before running. For each
instance in `state.json`:

1. The contract source at `contracts/<Name>.smartts` must exist.
2. The source must parse without errors.
3. The name declared in the source must match the `contractName` in
   `state.json`.
4. The source must pass type checking.
5. The persisted storage JSON must decode against the contract's declared
   `storage` type.

If any of these checks fails, the command exits with an error message before
touching any state.

---

## Exit Codes

| Situation | Exit code |
|-----------|-----------|
| Success | `0` |
| Parse error, type error, or runtime error | `1` (via `die`) |
| Wrong flags or missing required flag | `1` (via `die`) |

---

**What to read next →** [08-testing.md](08-testing.md)
