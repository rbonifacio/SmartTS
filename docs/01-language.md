# 1 — The SmartTS Language

This document describes SmartTS from a **user** perspective: what you can
write, what the rules are, and what a complete contract looks like. No
knowledge of the implementation is needed here.

---

## Types

SmartTS has four types:

| Type | Description | Example values |
|------|-------------|----------------|
| `int` | Integer | `0`, `42`, `-7` |
| `bool` | Boolean | `true`, `false` |
| `unit` | No value; used as a return type for side-effect-only methods | `()` |
| `{ f1: T1, f2: T2, … }` | Record — a named collection of fields | `{ count: 1, enabled: true }` |

Records can be nested: `{ pos: { x: int, y: int }, active: bool }` is a
record whose `pos` field is itself a record.

---

## Contract Structure

A SmartTS program is a single **contract declaration**. There are no
top-level functions or scripts. The contract has a name, a `storage` block,
and one or more methods:

```typescript
contract Name {
  storage: {
    field1: Type,
    field2: Type
  };

  @originate
  methodName(param: Type): ReturnType {
    // body
  }
}
```

### The `storage` block

`storage` declares the contract's persistent fields. Every field must have a
name and a type. Fields are separated by commas:

```typescript
storage: {
  count: int,
  enabled: bool
};
```

The semicolon after the closing `}` is required.

---

## Methods

Every method has a **decorator**, a name, a parameter list, a return type, and
a body.

### Decorators

| Decorator | Meaning |
|-----------|---------|
| `@originate` | Called once to deploy the contract and initialise storage. |
| `@entrypoint` | Can be called by the CLI after deployment. |
| `@private` | Internal helper; cannot be called from outside. |

A contract must have **exactly one** `@originate` method. It may have any
number of `@entrypoint` and `@private` methods.

### Parameters

Parameters are read-only. Each parameter has a name and a type, separated by a
colon:

```typescript
@entrypoint
increment(by: int): int {
  // `by` is read-only here
}
```

### Return type

Every method must declare a return type. Use `unit` when the method does not
return a meaningful value:

```typescript
@originate
init(n: int): unit {
  storage.count = n;
  return ();
}
```

---

## Locals

Inside a method body, you can declare local variables with `var` (mutable) or
`val` (immutable). Both require an explicit type and an initialiser:

```typescript
var x: int = 10;     // x can be reassigned later
val y: bool = true;  // y cannot be reassigned
```

There are no uninitialised variables in SmartTS.

---

## Statements

### Variable declaration

```typescript
var counter: int = 0;
val limit: int = 100;
```

### Assignment

```typescript
counter = counter + 1;
```

The left-hand side can be a local `var`, a storage field, or a nested field
path:

```typescript
storage.count = 42;           // top-level storage field
storage.pos.x = 10;           // nested storage field
myRecord.enabled = false;     // field of a local var
```

Parameters and `val` locals cannot be assigned.

### Block

A block groups multiple statements between `{` and `}`. Statements within a
block are executed in order, each terminated by `;`:

```typescript
{
  var x: int = 1;
  var y: int = 2;
  storage.sum = x + y;
}
```

### If / else

```typescript
if (storage.enabled) {
  storage.count = storage.count + by;
} else {
  storage.count = storage.count;
}
```

The `else` branch is optional. Both branches must be blocks.

### While loop

```typescript
while (i < 10) {
  storage.count = storage.count + 1;
  i = i + 1;
}
```

### Return

```typescript
return storage.count;
```

Methods with return type `unit` return `()`:

```typescript
return ();
```

---

## Expressions

### Arithmetic

| Operator | Meaning | Operand types | Result |
|----------|---------|---------------|--------|
| `+` | Addition | `int`, `int` | `int` |
| `-` | Subtraction | `int`, `int` | `int` |
| `*` | Multiplication | `int`, `int` | `int` |
| `/` | Division | `int`, `int` | `int` |
| `%` | Remainder | `int`, `int` | `int` |

Division and remainder by zero are **runtime errors**.

### Comparison

| Operator | Meaning | Operand types | Result |
|----------|---------|---------------|--------|
| `<` | Less than | `int`, `int` | `bool` |
| `<=` | Less than or equal | `int`, `int` | `bool` |
| `>` | Greater than | `int`, `int` | `bool` |
| `>=` | Greater than or equal | `int`, `int` | `bool` |
| `==` | Equal | any, same type | `bool` |
| `!=` | Not equal | any, same type | `bool` |

`==` and `!=` work on any pair of values with the **same type**. Comparing an
`int` to a `bool` is a type error.

### Boolean

| Operator | Meaning | Example |
|----------|---------|---------|
| `!e` | Logical not | `!storage.enabled` |
| `e1 && e2` | Logical and | `x > 0 && x < 10` |
| `e1 \|\| e2` | Logical or | `x < 0 \|\| x > 100` |

### Operator Precedence

From highest (evaluated first) to lowest (evaluated last):

| Priority | Operators |
|----------|-----------|
| 1 (highest) | unary `!` |
| 2 | `*`, `/`, `%` |
| 3 | `+`, `-` |
| 4 | `==`, `!=`, `<=`, `>=`, `<`, `>` |
| 5 | `&&` |
| 6 (lowest) | `\|\|` |

All binary operators at the same level are **left-associative**:
`1 - 2 - 3` means `(1 - 2) - 3`.

Use parentheses to override precedence: `(a + b) * c`.

### Field access

Use `.` to read a field from a record or from storage:

```typescript
storage.count         // field of storage
pos.x                 // field of a local record variable
storage.pos.x         // nested field of storage
{ a: 1, b: true }.a   // field of a record literal — evaluates to 1
```

### Record literals

```typescript
{ count: 10, enabled: true }
{ x: a + 1, y: b }
```

The inferred type follows source order: `{ a: int, b: bool }` and
`{ b: bool, a: int }` are **different** types.

---

## Accessing and Mutating Storage

Inside any method, `storage` refers to the contract's persistent state. You
can **read** a field:

```typescript
return storage.count;
```

And you can **write** a field:

```typescript
storage.count = storage.count + by;
```

The `@originate` method starts with an uninitialised storage, so it is
expected to write every field before returning.

---

## Complete Example: Counter

The canonical example in `samples/Counter.smartts`:

```typescript
contract Counter {
  storage: {
    count: int,
    enabled: bool
  };

  @originate
  init(initialCount: int): unit {
    var start: int = initialCount;
    val isEnabled: bool = true;
    storage.count = start;
    storage.enabled = isEnabled;
    return ();
  }

  @entrypoint
  increment(by: int): int {
    if (storage.enabled) {
      storage.count = storage.count + by;
    } else {
      storage.count = storage.count;
    }
    return storage.count;
  }

  @entrypoint
  setEnabled(value: bool): unit {
    storage.enabled = value;
    return ();
  }
}
```

Notice:
- `init` uses both `var` and `val` to initialise storage.
- `increment` reads `storage.enabled` in an `if` condition and mutates
  `storage.count`.
- `setEnabled` takes a `bool` parameter and writes it directly to storage.
- All methods that return `unit` end with `return ();`.

---

**What to read next →** [02-pipeline.md](02-pipeline.md)
