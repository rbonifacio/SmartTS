{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for Project 5: fail_with and require.
--
-- Covers:
--   * Parser  – new syntax produces correct AST nodes
--   * Type checker – bottom type (never), subtyping, require validation
--   * Interpreter – runtime abort, desugaring, storage-rollback semantics
module FailWithRequireTests (failWithRequireTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as M

import SmartTS.AST
import SmartTS.Parser        (parseContractFromString)
import SmartTS.TypeCheck     (typeCheckContract)
import SmartTS.Interpreter
  ( ContractInstance (..)
  , RepositoryState
  , originateWithJsonArgs
  , callEntrypointWithJsonArgs
  , instanceStorage
  )

-- ---------------------------------------------------------------------------
-- Top-level export
-- ---------------------------------------------------------------------------

failWithRequireTests :: TestTree
failWithRequireTests =
  testGroup
    "Project 5: fail_with and require"
    [ p5ParserTests
    , p5TypeCheckTests
    , p5InterpreterTests
    ]

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

parseOk :: String -> (Contract -> Assertion) -> Assertion
parseOk src f = case parseContractFromString src of
  Left err -> assertFailure $ "Parse failed: " ++ show err
  Right c  -> f c

parseFails :: String -> Assertion
parseFails src = case parseContractFromString src of
  Left _  -> return ()
  Right _ -> assertFailure "Expected parse failure but got success"

tcOk :: String -> Assertion
tcOk src = parseOk src $ \c ->
  case typeCheckContract c of
    Left err -> assertFailure $ "Type check failed: " ++ err
    Right () -> return ()

tcFails :: String -> Assertion
tcFails src = parseOk src $ \c ->
  case typeCheckContract c of
    Left _   -> return ()
    Right () -> assertFailure "Expected type error but checking succeeded"

-- | Originate a contract from source using empty JSON args.
originate :: String -> IO (String, RepositoryState)
originate src = do
  c <- parseIO src
  tcIO c
  case originateWithJsonArgs M.empty c src (object []) of
    Left err           -> assertFailureIO $ "Originate error: " ++ err
    Right (addr, repo) -> return (addr, repo)

parseIO :: String -> IO Contract
parseIO src = case parseContractFromString src of
  Left err -> assertFailureIO $ "Parse error: " ++ show err
  Right c  -> return c

tcIO :: Contract -> IO ()
tcIO c = case typeCheckContract c of
  Left err -> assertFailureIO $ "Type error: " ++ err
  Right () -> return ()

-- | assertFailure lifted to IO a so it can be used before a return.
assertFailureIO :: String -> IO a
assertFailureIO msg = assertFailure msg >> error "unreachable"

-- ---------------------------------------------------------------------------
-- 1. Parser tests
-- ---------------------------------------------------------------------------

p5ParserTests :: TestTree
p5ParserTests =
  testGroup "Parser"
  [ testCase "fail_with(int) produces FailWith node" $
      parseOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(): int { return fail_with(42); } }"
        $ \c -> case c of
            Contract _ _
              [MethodDecl EntryPoint "f" [] TInt
                (SequenceStmt [ReturnStmt (FailWith (CInt 42))])] ->
                  return ()
            _ -> assertFailure $ "Unexpected AST: " ++ show c

  , testCase "fail_with payload can be an expression" $
      parseOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(v: int): int { return fail_with(v + 1); } }"
        $ \c -> case c of
            Contract _ _
              [MethodDecl EntryPoint "f" [FormalParameter "v" TInt] TInt
                (SequenceStmt
                  [ReturnStmt (FailWith (Add (Var "v") (CInt 1)))])] ->
                      return ()
            _ -> assertFailure $ "Unexpected AST: " ++ show c

  , testCase "require(cond, payload) produces RequireStmt" $
      parseOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(v: int): int { require(v > 0, 99); return v; } }"
        $ \c -> case c of
            Contract _ _
              [MethodDecl EntryPoint "f" [FormalParameter "v" TInt] TInt
                (SequenceStmt
                  [ RequireStmt (Gt (Var "v") (CInt 0)) (CInt 99)
                  , ReturnStmt (Var "v")
                  ])] -> return ()
            _ -> assertFailure $ "Unexpected AST: " ++ show c

  , testCase "require with && condition parses correctly" $
      parseOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(a: int, b: int): int \
        \   { require((a > 0) && (b > 0), 1); return a + b; } }"
        $ \c -> case c of
            Contract _ _
              [MethodDecl EntryPoint "f" _ TInt
                (SequenceStmt (RequireStmt {} : _))] -> return ()
            _ -> assertFailure $ "Unexpected AST: " ++ show c

  , testCase "multiple requires parse as multiple RequireStmt nodes" $
      parseOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(a: int, b: int): int \
        \   { require(a > 0, 1); require(b > 0, 2); return a + b; } }"
        $ \c -> case c of
            Contract _ _
              [MethodDecl EntryPoint "f" _ TInt
                (SequenceStmt
                  [ RequireStmt (Gt (Var "a") (CInt 0)) (CInt 1)
                  , RequireStmt (Gt (Var "b") (CInt 0)) (CInt 2)
                  , ReturnStmt _
                  ])] -> return ()
            _ -> assertFailure $ "Unexpected AST: " ++ show c

  , testCase "fail_with inside if-then branch parses" $
      parseOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(b: bool): int \
        \   { if (b) { return fail_with(1); } else { return 0; } } }"
        $ \c -> case c of
            Contract _ _ [MethodDecl EntryPoint "f" _ TInt _] -> return ()
            _ -> assertFailure $ "Unexpected AST: " ++ show c

  , testCase "fail_with is a reserved word - cannot be used as identifier" $
      parseFails
        "contract C { storage: { x: int }; \
        \ @entrypoint f(): int { val fail_with: int = 1; return fail_with; } }"

  , testCase "require is a reserved word - cannot be used as identifier" $
      parseFails
        "contract C { storage: { x: int }; \
        \ @entrypoint f(): int { val require: int = 1; return require; } }"
  ]

-- ---------------------------------------------------------------------------
-- 2. Type-checker tests
-- ---------------------------------------------------------------------------

p5TypeCheckTests :: TestTree
p5TypeCheckTests =
  testGroup "Type Checker"
  [ testCase "fail_with has type never - accepted where int expected" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(b: bool): int \
        \   { if (b) { return fail_with(99); } else { return 0; } } }"

  , testCase "fail_with has type never - accepted where bool expected" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(b: bool): bool \
        \   { if (b) { return true; } else { return fail_with(0); } } }"

  , testCase "fail_with has type never - accepted where unit expected" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @originate init(): unit { return fail_with(0); } }"

  , testCase "all branches return or fail - no missing-return false positive" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(v: int): int \
        \   { if (v > 10) { return 100; } \
        \     else { if (v > 0) { return v; } else { return fail_with(42); } } } }"

  , testCase "fail_with payload is type-checked - rejects ill-typed payload" $
      tcFails
        "contract C { storage: { x: int }; \
        \ @entrypoint f(): int { return fail_with(true + 1); } }"

  , testCase "require with bool condition is well-typed" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(v: int): int { require(v > 0, 100); return v; } }"

  , testCase "require condition must be bool - rejects int condition" $
      tcFails
        "contract C { storage: { x: int }; \
        \ @entrypoint f(v: int): int { require(v, 1); return v; } }"

  , testCase "require payload may be any well-typed expression" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(v: int): int { require(v > 0, v * 2 + 1); return v; } }"

  , testCase "multiple requires in sequence are well-typed" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(a: int, b: int): int \
        \   { require(a > 0, 1); require(b > 0, 2); return a + b; } }"

  , testCase "require inside while body is well-typed" $
      tcOk
        "contract C { storage: { x: int }; \
        \ @entrypoint f(n: int): int \
        \   { var i: int = 0; \
        \     while (i < n) { require(i != 5, 99); i = i + 1; } \
        \     return i; } }"
  ]

-- ---------------------------------------------------------------------------
-- 3. Interpreter tests
-- ---------------------------------------------------------------------------

p5InterpreterTests :: TestTree
p5InterpreterTests =
  testGroup "Interpreter"
  [ testCase "fail_with taken branch - returns FailWith value" $ do
      let src = unlines
            [ "contract C { storage: { x: int };"
            , "  @originate init(): unit { storage.x = 0; return (); }"
            , "  @entrypoint f(b: bool): int"
            , "    { if (b) { return fail_with(99); } else { return storage.x; } }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["b" .= True]
      case callEntrypointWithJsonArgs repo c addr "f" src args of
        Right (Just (FailWith (CInt 99)), _) -> return ()
        Right (v, _) -> assertFailure $ "Expected FailWith(99), got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err

  , testCase "fail_with not taken - normal return value" $ do
      let src = unlines
            [ "contract C { storage: { x: int };"
            , "  @originate init(): unit { storage.x = 7; return (); }"
            , "  @entrypoint f(b: bool): int"
            , "    { if (b) { return fail_with(99); } else { return storage.x; } }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["b" .= False]
      case callEntrypointWithJsonArgs repo c addr "f" src args of
        Right (Just (CInt 7), _) -> return ()
        Right (v, _) -> assertFailure $ "Expected CInt 7, got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err

  , testCase "require passing - execution continues, result returned" $ do
      let src = unlines
            [ "contract C { storage: { counter: int };"
            , "  @originate init(): unit { storage.counter = 0; return (); }"
            , "  @entrypoint inc(v: int): int"
            , "    { require(v > 0, 200);"
            , "      storage.counter = storage.counter + v;"
            , "      return storage.counter; }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["v" .= (5 :: Int)]
      case callEntrypointWithJsonArgs repo c addr "inc" src args of
        Right (Just (CInt 5), _) -> return ()
        Right (v, _) -> assertFailure $ "Expected CInt 5, got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err

  , testCase "require failing - returns FailWith with correct payload" $ do
      let src = unlines
            [ "contract C { storage: { counter: int };"
            , "  @originate init(): unit { storage.counter = 0; return (); }"
            , "  @entrypoint inc(v: int): int"
            , "    { require(v > 0, 200);"
            , "      storage.counter = storage.counter + v;"
            , "      return storage.counter; }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["v" .= ((-3) :: Int)]
      case callEntrypointWithJsonArgs repo c addr "inc" src args of
        Right (Just (FailWith (CInt 200)), _) -> return ()
        Right (v, _) -> assertFailure $ "Expected FailWith(200), got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err

  , testCase "require failing - storage is NOT modified" $ do
      let src = unlines
            [ "contract C { storage: { counter: int };"
            , "  @originate init(): unit { storage.counter = 0; return (); }"
            , "  @entrypoint inc(v: int): int"
            , "    { require(v > 0, 200);"
            , "      storage.counter = 999;"
            , "      return storage.counter; }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["v" .= ((-1) :: Int)]
      case callEntrypointWithJsonArgs repo c addr "inc" src args of
        Right (Just (FailWith _), repo') ->
          case M.lookup addr repo' of
            Nothing -> assertFailure "Address not found in repo"
            Just ci -> case instanceStorage ci of
              Record [("counter", CInt 0)] -> return ()
              other -> assertFailure $ "Storage was mutated: " ++ show other
        Right (v, _) -> assertFailure $ "Expected abort, got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err

  , testCase "first require passes, second fails - correct payload" $ do
      let src = unlines
            [ "contract C { storage: { n: int };"
            , "  @originate init(): unit { storage.n = 0; return (); }"
            , "  @entrypoint f(x: int, y: int): int"
            , "    { require(x > 0, 300);"
            , "      require(y > 0, 400);"
            , "      return x + y; }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["x" .= (5 :: Int), "y" .= ((-1) :: Int)]
      case callEntrypointWithJsonArgs repo c addr "f" src args of
        Right (Just (FailWith (CInt 400)), _) -> return ()
        Right (Just (FailWith p), _) ->
          assertFailure $ "Expected payload CInt 400, got: " ++ show p
        Right (v, _) -> assertFailure $ "Expected abort, got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err

  , testCase "require inside if-else - only fires on the taken branch" $ do
      let src = unlines
            [ "contract C { storage: { counter: int };"
            , "  @originate init(): unit { storage.counter = 0; return (); }"
            , "  @entrypoint f(flag: bool, v: int): int"
            , "    { if (flag) {"
            , "        require(v > 0, 500);"
            , "        storage.counter = 999;"
            , "      } else {"
            , "        storage.counter = v;"
            , "      }"
            , "      return storage.counter; }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["flag" .= False, "v" .= (3 :: Int)]
      case callEntrypointWithJsonArgs repo c addr "f" src args of
        Right (Just (CInt 3), _) -> return ()
        Right (v, _) -> assertFailure $ "Expected CInt 3, got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err

  , testCase "fail_with payload expression is fully evaluated before abort" $ do
      let src = unlines
            [ "contract C { storage: { x: int };"
            , "  @originate init(): unit { storage.x = 10; return (); }"
            , "  @entrypoint f(v: int): int"
            , "    { return fail_with(v * 3 + 1); }"
            , "}"
            ]
      (addr, repo) <- originate src
      c <- parseIO src
      let args = object ["v" .= (4 :: Int)]
      case callEntrypointWithJsonArgs repo c addr "f" src args of
        Right (Just (FailWith (CInt 13)), _) -> return ()
        Right (v, _) -> assertFailure $ "Expected FailWith(CInt 13), got: " ++ show v
        Left  err    -> assertFailure $ "Unexpected error: " ++ err
  ]