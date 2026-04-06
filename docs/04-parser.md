# 4 — The Parser

This document explains how the SmartTS parser turns source text into a
`Contract` AST. We start with a concrete example, then introduce the tools
the parser uses.

---

## A Worked Example: Parsing `storage.count + by`

Consider the expression `storage.count + by`. The parser needs to produce:

```
        Add
       /   \
FieldAccess  Var "by"
   /      \
StorageExpr  "count"
```

Here is roughly what happens, step by step:

1. `parseExpr` calls `makeExprParser parseTerm operators`.
2. `makeExprParser` calls `parseTerm` for the left operand.
3. `parseTerm` calls `parseAtomOrStorage` and gets `StorageExpr` back.
4. `parseTerm` then tries to consume `.` field accesses. It sees `.count` and
   folds it into `FieldAccess StorageExpr "count"`.
5. Back in `makeExprParser`: the remaining input starts with `+`, which
   matches the `Add` operator. It calls `parseTerm` again for the right side.
6. `parseTerm` → `parseAtomOrStorage` → `parseAtom` → `parseVar` returns
   `Var "by"`. No `.` follows, so the term is just `Var "by"`.
7. `makeExprParser` builds `Add (FieldAccess StorageExpr "count") (Var "by")`.

The same logic handles arbitrarily nested expressions like
`storage.pos.x * (a + b)`.

---

## What is a Parser Combinator?

SmartTS uses **Megaparsec** — a Haskell parser combinator library — to build
the parser. A combinator is a small function that recognises one piece of
syntax and can be composed into parsers for larger constructs.

Every Megaparsec parser follows the same contract:

- **Input:** a `String` (the remaining source text)
- **Output:** either a successful parse `(remainingInput, value)` or an error

`remainingInput` is the part of the input not yet consumed. This is how
parsers chain: one parser hands its leftover input to the next.

---

## Key Megaparsec Combinators Used

| Combinator | What it does | Example use |
|------------|-------------|-------------|
| `symbol "x"` | Match the exact string `"x"`, skip trailing whitespace | `symbol "@originate"` |
| `reserved "x"` | Same as `symbol` in this codebase | `reserved "contract"` |
| `many p` | Apply `p` zero or more times; collect results | Parse all methods in a contract |
| `sepBy p sep` | Parse `p`, then `sep p` repeatedly | Comma-separated fields |
| `optional p` | Try `p`; return `Just result` or `Nothing` | Optional `else` branch |
| `between a b p` | Parse `a`, then `p`, then `b`; return `p`'s result | `braces`, `parens` |
| `<\|>` | Try the left parser; if it fails, try the right | `parseBool <\|> parseInt` |
| `makeExprParser term ops` | Build an expression parser from a term parser and an operator table | Expression parsing |

---

## Operator Precedence

The expression `1 + 2 * 3` must parse as `1 + (2 * 3)`, not `(1 + 2) * 3`.
SmartTS delegates this to `makeExprParser` from the
`Control.Monad.Combinators.Expr` library by providing an operator table:

```haskell
operators :: [[Operator Parser Expr]]
operators =
  [ [ Prefix (Not <$ symbol "!") ]                        -- highest
  , [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")
    , InfixL (Mod <$ symbol "%") ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-") ]
  , [ InfixN (Eq  <$ symbol "==")
    , InfixN (Neq <$ symbol "!=")
    , InfixN (Lte <$ symbol "<=")
    , InfixN (Gte <$ symbol ">=")
    , InfixN (Lt  <$ symbol "<")
    , InfixN (Gt  <$ symbol ">") ]
  , [ InfixL (And <$ symbol "&&") ]
  , [ InfixL (Or  <$ symbol "||") ]                       -- lowest
  ]
```

Each inner list is one **precedence level**. Lists earlier in the outer list
bind tighter. `InfixL` means left-associative; `InfixN` means
non-associative (you cannot chain `a == b == c`); `Prefix` means a unary
prefix operator.

---

## Field Access: the `parseTerm` Loop

Field access (`e.f`) is handled **outside** the operator table. After parsing
any atom, `parseTerm` greedily consumes as many `.name` suffixes as it finds:

```haskell
parseTerm :: Parser Expr
parseTerm = do
  base   <- parseAtomOrStorage
  fields <- many (symbol "." *> parseName)
  return (foldl FieldAccess base fields)
```

`foldl FieldAccess base ["pos", "x"]` produces
`FieldAccess (FieldAccess base "pos") "x"` — left-associative, which is the
natural reading of `storage.pos.x`.

---

## LValue Parsing: the Same Pattern for Assignments

Assignments need to parse the left-hand side into an `LValue` rather than an
`Expr`. The grammar is identical (`base.f.g`), but the base is either
`storage` → `LStorage` or an identifier → `LVar n`:

```haskell
parseLValue :: Parser LValue
parseLValue = do
  base   <- parseAssignableBase      -- LStorage or LVar
  fields <- many (symbol "." *> parseName)
  return (foldl LField base fields)
```

---

## Contract Grammar (top-down)

The grammar is structured top-down. Each level calls the next:

```
parseProgram
  └── parseContract
        ├── reserved "contract" + parseName
        ├── parseStorage
        │     └── braces (sepBy parseStorageField ",")
        │           └── parseName ":" parseType
        └── many parseMethod
              ├── many parseMethodKind  (@originate / @entrypoint / @private)
              ├── parseName
              ├── parseFormalParameters
              │     └── parens (sepBy parseFormalParameter ",")
              │           └── parseName ":" parseType
              ├── ":" parseType         (return type)
              └── parseBlock
                    └── braces (many parseStmt)
                          └── parseStmt
                                ├── parseIfStmt
                                ├── parseWhileStmt
                                ├── parseVarDeclStmt
                                ├── parseValDeclStmt
                                ├── parseReturn
                                ├── parseAssignment
                                └── parseBlock (nested blocks)
```

---

## Identifiers and Reserved Words

The `identifier` parser ensures that reserved words cannot be used as variable
or method names. After reading a name, it checks against `reservedWords`:

```haskell
reservedWords =
  [ "contract", "storage", "int", "bool", "unit"
  , "return", "if", "else", "while", "var", "val"
  , "true", "false"
  ]
```

If the parsed name is in this list, the parser fails with "reserved word: …".
This prevents `var if: int = 0` from parsing as a valid declaration.

---

## Key Design Decisions

**No separate lexer.** The parser works directly on characters using
Megaparsec. A separate tokenisation pass would add indirection without
significant benefit at this scale.

**`makeExprParser` for expressions.** Building a recursive-descent expression
parser by hand (one function per precedence level) is correct but verbose.
`makeExprParser` encodes the same precedence table in a concise declarative
form and handles associativity automatically.

**Field access outside the operator table.** The `.` accessor is not a binary
operator in the usual sense — it does not take two arbitrary expressions, only
a name on the right. Handling it in `parseTerm` with a suffix loop keeps the
operator table clean and ensures `a.b.c` always left-associates without
special precedence rules.

---

**What to read next →** [05-type-checker.md](05-type-checker.md)
