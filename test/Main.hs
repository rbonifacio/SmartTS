{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import SmartTS.AST
import SmartTS.Parser
import Data.Aeson (object, (.=), toJSON)
import SmartTS.Interpreter (ContractInstance (..), contractInstanceFromStorageValue,
                             originateWithJsonArgs)
import SmartTS.TypeCheck (typeCheckContract)
import qualified Data.Map.Strict as M

main :: IO ()
main = defaultMain tests

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
        , pairEnumParserTests
        ]
    , typeCheckTests
    , pairTypeCheckTests
    , enumTypeCheckTests
    , pairInterpreterTests
    , enumInterpreterTests
    ]

-- Helper function to parse and assert success
parseSuccess :: String -> (Contract -> Assertion) -> Assertion
parseSuccess input assertion = case parseContractFromString input of
  Left err -> assertFailure $ "Parse failed: " ++ show err
  Right contract -> assertion contract

-- Helper function to parse and assert failure
parseFailure :: String -> Assertion
parseFailure input = case parseContractFromString input of
  Left _ -> return ()  -- Expected failure
  Right _ -> assertFailure "Expected parse failure but got success"

typeCheckSuccess :: String -> Assertion
typeCheckSuccess input = case parseContractFromString input of
  Left err -> assertFailure $ "Parse failed: " ++ show err
  Right c ->
    case typeCheckContract c of
      Left err -> assertFailure $ "Type check failed: " ++ err
      Right () -> return ()

typeCheckFailure :: String -> Assertion
typeCheckFailure input = case parseContractFromString input of
  Left err -> assertFailure $ "Parse failed (need valid parse for type test): " ++ show err
  Right c ->
    case typeCheckContract c of
      Left _ -> return ()
      Right () -> assertFailure "Expected type error but checking succeeded"

contractTests :: TestTree
contractTests = testGroup "Contract Parsing"
  [ testCase "Simple contract with storage and method" $
      parseSuccess "contract MyContract { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract "MyContract" [( "x", TInt)] [] [MethodDecl Originate "init" [] TInt (SequenceStmt [ReturnStmt (CInt 0)])] ->
            return ()
          _ -> assertFailure $ "Unexpected contract structure: " ++ show contract
  
  , testCase "Contract with multiple storage fields" $
      parseSuccess "contract Test { storage: { x: int, y: int }; @entrypoint test(): int { return 1; } }" $ \contract ->
        case contract of
          Contract "Test" storage _ _ ->
            assertEqual "Storage should have 2 fields" 2 (length storage)
          _ -> assertFailure "Unexpected contract name"
  
  , testCase "Contract with multiple methods" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } @entrypoint inc(): int { return 1; } }" $ \contract ->
        case contract of
          Contract _ _ _ methods ->
            assertEqual "Should have 2 methods" 2 (length methods)
  ]

storageTests :: TestTree
storageTests = testGroup "Storage Parsing"
  [ testCase "Single storage field" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ [(name, typ)] _ _ -> do
            assertEqual "Storage field name" "x" name
            assertEqual "Storage field type" TInt typ
          _ -> assertFailure "Unexpected storage structure"
  
  , testCase "Multiple storage fields" $
      parseSuccess "contract Test { storage: { x: int, y: int, z: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ storage _ _ ->
            assertEqual "Should have 3 storage fields" 3 (length storage)
  
  , testCase "Storage with single field (no comma)" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ storage _ _ ->
            assertEqual "Should have 1 storage field" 1 (length storage)
  ]

methodTests :: TestTree
methodTests = testGroup "Method Parsing"
  [ testCase "Method with @originate decorator" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl Originate "init" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected @originate method"
  
  , testCase "Method with @entrypoint decorator" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl EntryPoint "test" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected @entrypoint method"
  
  , testCase "Method with @private decorator" $
      parseSuccess "contract Test { storage: { x: int }; @private helper(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl Private "helper" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected @private method"
  
  , testCase "Method with parameters" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint add(a: int, b: int): int { return a + b; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl EntryPoint "add" params TInt _] -> do
            assertEqual "Should have 2 parameters" 2 (length params)
            case params of
              [FormalParameter "a" TInt, FormalParameter "b" TInt] ->
                return ()
              _ -> assertFailure "Unexpected parameter structure"
          _ -> assertFailure "Unexpected method structure"
  
  , testCase "Method with single parameter" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint inc(x: int): int { return x + 1; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ params _ _] ->
            assertEqual "Should have 1 parameter" 1 (length params)
          _ -> assertFailure "Unexpected method structure"
  
  , testCase "Method with empty parameter list" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ params _ _] ->
            assertEqual "Should have 0 parameters" 0 (length params)
          _ -> assertFailure "Unexpected method structure"
  ]

expressionTests :: TestTree
expressionTests = testGroup "Expression Parsing"
  [ testCase "Integer literal" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 42; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (CInt 42)])] ->
            return ()
          _ -> assertFailure $ "Expected integer literal 42, got: " ++ show contract
  
  , testCase "Variable reference" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return x; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure $ "Expected variable reference, got: " ++ show contract
  
  , testCase "Addition expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 1 + 2; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Add (CInt 1) (CInt 2))])] ->
            return ()
          _ -> assertFailure $ "Expected addition expression, got: " ++ show contract
  
  , testCase "Subtraction expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 5 - 3; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Sub (CInt 5) (CInt 3))])] ->
            return ()
          _ -> assertFailure $ "Expected subtraction expression, got: " ++ show contract
  
  , testCase "Chained addition" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 1 + 2 + 3; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt expr])] -> do
            -- Should parse as (1 + 2) + 3 due to left associativity
            case expr of
              Add (Add (CInt 1) (CInt 2)) (CInt 3) ->
                return ()
              _ -> assertFailure $ "Expected left-associative addition, got: " ++ show expr
          _ -> assertFailure $ "Unexpected expression structure: " ++ show contract
  
  , testCase "Mixed addition and subtraction" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 10 - 2 + 3; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt expr])] -> do
            -- Should parse as (10 - 2) + 3 due to left associativity
            case expr of
              Add (Sub (CInt 10) (CInt 2)) (CInt 3) ->
                return ()
              _ -> assertFailure $ "Expected left-associative mixed operations, got: " ++ show expr
          _ -> assertFailure $ "Unexpected expression structure: " ++ show contract
  
  , testCase "Parenthesized expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return (1 + 2); } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Add (CInt 1) (CInt 2))])] ->
            return ()
          _ -> assertFailure $ "Expected parenthesized addition, got: " ++ show contract
  
  , testCase "Unit expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return (); } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt Unit])] ->
            return ()
          _ -> assertFailure $ "Expected unit expression, got: " ++ show contract

  , testCase "Boolean expression (&&) and boolean type" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint check(): bool { return true && false; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ "check" [] TBool (SequenceStmt [ReturnStmt (And (CBool True) (CBool False))])] ->
            return ()
          _ -> assertFailure $ "Expected boolean && expression, got: " ++ show contract

  , testCase "Not expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint notit(): bool { return !false; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ "notit" [] TBool (SequenceStmt [ReturnStmt (Not (CBool False))])] ->
            return ()
          _ -> assertFailure $ "Expected !false, got: " ++ show contract

  , testCase "Relational expression (==)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint eq(): bool { return 1 == 2; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ "eq" [] TBool (SequenceStmt [ReturnStmt (Eq (CInt 1) (CInt 2))])] ->
            return ()
          _ -> assertFailure $ "Expected 1 == 2, got: " ++ show contract

  , testCase "Mul/Div/Mod expressions" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint arith(): int { return 6 * 7; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ "arith" [] TInt (SequenceStmt [ReturnStmt (Mul (CInt 6) (CInt 7))])] ->
            return ()
          _ -> assertFailure $ "Expected 6 * 7, got: " ++ show contract

  , testCase "Record type and record literal" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint get(): { a: int, b: bool } { return { a: 1, b: true }; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ "get" [] (TRecord [("a", TInt), ("b", TBool)]) (SequenceStmt [ReturnStmt (Record [("a", CInt 1), ("b", CBool True)])])] ->
            return ()
          _ -> assertFailure $ "Expected record type/literal, got: " ++ show contract

  , testCase "Record field access (x.f)" $
      parseSuccess "contract Test { storage: { x: { a: int, b: bool } }; @entrypoint proj(): int { return x.a; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "proj" [] TInt
                (SequenceStmt [ReturnStmt (FieldAccess (Var "x") "a")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected projection x.a, got: " ++ show contract

  , testCase "Chained field access (x.a.b)" $
      parseSuccess "contract Test { storage: { x: { a: { b: int } } }; @entrypoint proj2(): int { return x.a.b; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "proj2" [] TInt
                (SequenceStmt
                  [ReturnStmt (FieldAccess (FieldAccess (Var "x") "a") "b")])]
            -> return ()
          _ -> assertFailure $ "Expected chained projection x.a.b, got: " ++ show contract

  , testCase "Chained field access on record literal (..a.b)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint litproj2(): int { return { a: { b: 1 } }.a.b; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "litproj2" [] TInt
                (SequenceStmt
                  [ReturnStmt
                    (FieldAccess
                      (FieldAccess
                        (Record [("a", Record [("b", CInt 1)])])
                        "a")
                      "b")])]
            -> return ()
          _ -> assertFailure $ "Expected chained projection on record literal, got: " ++ show contract

  , testCase "Field access on record literal" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint litproj(): int { return { a: 1, b: true }.a; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "litproj" [] TInt
                (SequenceStmt [ReturnStmt (FieldAccess (Record [("a", CInt 1), ("b", CBool True)]) "a")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected projection on record literal, got: " ++ show contract
  ]

statementTests :: TestTree
statementTests = testGroup "Statement Parsing"
  [ testCase "Return statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 42; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (CInt 42)])] ->
            return ()
          _ -> assertFailure $ "Expected return statement, got: " ++ show contract
  
  , testCase "Assignment statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { x = 10; return x; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [AssignmentStmt (LVar "x") (CInt 10), ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure "Expected assignment and return statements"
  
  , testCase "Multiple statements in block" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { x = 1; x = 2; return x; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt stmts)] ->
            assertEqual "Should have 3 statements" 3 (length stmts)
          _ -> assertFailure "Unexpected statement structure"
  
  , testCase "Assignment with expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { x = 1 + 2; return x; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [AssignmentStmt (LVar "x") (Add (CInt 1) (CInt 2)), ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure "Expected assignment with expression"

  , testCase "If statement with else" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint f(): int { if (true) { return 1; } else { return 2; } } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ "f" [] TInt (SequenceStmt [IfStmt (CBool True) (SequenceStmt [ReturnStmt (CInt 1)]) (Just (SequenceStmt [ReturnStmt (CInt 2)]))])] ->
            return ()
          _ -> assertFailure $ "Expected if/else, got: " ++ show contract

  , testCase "While statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint loop(): int { while (false) { x = 1; } return x; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl _ "loop" [] TInt (SequenceStmt [WhileStmt (CBool False) (SequenceStmt [AssignmentStmt (LVar "x") (CInt 1)]) , ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure $ "Expected while statement, got: " ++ show contract

  , testCase "Field assignment statement (x.a = ...)" $
      parseSuccess "contract Test { storage: { x: { a: int } }; @entrypoint fa(): int { x.a = 3; return x.a; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [MethodDecl _ "fa" [] TInt
              (SequenceStmt
                [ AssignmentStmt (LField (LVar "x") "a") (CInt 3)
                , ReturnStmt (FieldAccess (Var "x") "a")
                ])] ->
              return ()
          _ -> assertFailure $ "Expected x.a assignment, got: " ++ show contract

  , testCase "Field assignment statement (x.a.b = ...)" $
      parseSuccess "contract Test { storage: { x: { a: { b: int } } }; @entrypoint fab(): int { x.a.b = 3; return x.a.b; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [MethodDecl _ "fab" [] TInt
              (SequenceStmt
                [ AssignmentStmt
                    (LField (LField (LVar "x") "a") "b")
                    (CInt 3)
                , ReturnStmt
                    (FieldAccess (FieldAccess (Var "x") "a") "b")
                ])] ->
              return ()
          _ -> assertFailure $ "Expected x.a.b assignment, got: " ++ show contract

  , testCase "Storage expression read (storage.x)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint sr(): int { return storage.x; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "sr" [] TInt
                (SequenceStmt
                  [ReturnStmt (FieldAccess StorageExpr "x")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected storage read, got: " ++ show contract

  , testCase "Storage expression write (storage.x = ...)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint sw(): int { storage.x = 10; return storage.x; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "sw" [] TInt
                (SequenceStmt
                  [ AssignmentStmt (LField LStorage "x") (CInt 10)
                  , ReturnStmt (FieldAccess StorageExpr "x")
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected storage write, got: " ++ show contract

  , testCase "Var declaration + assignment to local var" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint v(): int { var y: int = 10; y = 11; return y; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "v" [] TInt
                (SequenceStmt
                  [ VarDeclStmt "y" TInt (CInt 10)
                  , AssignmentStmt (LVar "y") (CInt 11)
                  , ReturnStmt (Var "y")
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected var decl + assignment, got: " ++ show contract

  , testCase "Val declaration + returning local val" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint c(): int { val y: int = 10; return y; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "c" [] TInt
                (SequenceStmt
                  [ ValDeclStmt "y" TInt (CInt 10)
                  , ReturnStmt (Var "y")
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected val decl, got: " ++ show contract

  , testCase "Field assignment to local record (x.a = ... but local var)" $
      parseSuccess "contract Test { storage: { x: { a: int } }; @entrypoint fa2(): int { var t: { a: int } = x; t.a = 7; return t.a; } }" $ \contract ->
        case contract of
          Contract _ _ _
            [ MethodDecl _ "fa2" [] TInt
                (SequenceStmt
                  [ VarDeclStmt "t" (TRecord [("a", TInt)]) (Var "x")
                  , AssignmentStmt (LField (LVar "t") "a") (CInt 7)
                  , ReturnStmt (FieldAccess (Var "t") "a")
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected local record field assignment, got: " ++ show contract
  ]

typeCheckTests :: TestTree
typeCheckTests =
  testGroup
    "Type checker"
    [ testCase "Minimal well-typed contract" $
        typeCheckSuccess
          "contract C { storage: { x: int }; @originate init(): int { return 0; } }"
    , testCase "Return type mismatch" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { return true; } }"
    , testCase "Arithmetic requires int" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { return 1 + true; } }"
    , testCase "Cannot assign to val" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { val v: int = 1; v = 2; return 0; } }"
    , testCase "Equality requires same types" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): bool { return 1 == true; } }"
    , testCase "If condition must be bool" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { if (1) { return 0; } else { return 1; } } }"
    , testCase "Storage field assignment matches storage type" $
        typeCheckSuccess
          "contract C { storage: { n: int }; @originate init(): unit { storage.n = 3; return (); } }"
    , testCase "Unknown storage field" $
        typeCheckFailure
          "contract C { storage: { n: int }; @originate init(): unit { storage.missing = 1; return (); } }"
    , testCase "Persisted storage decodes against contract storage type" $
        parseSuccess
          "contract C { storage: { n: int, b: bool }; @originate init(): unit { return (); } }"
          $ \c ->
            case contractInstanceFromStorageValue c (object ["n" .= (1 :: Int), "b" .= True]) of
              Left err -> assertFailure err
              Right (ContractInstance _ st) -> case st of
                Record [("n", CInt 1), ("b", CBool True)] -> return ()
                _ -> assertFailure $ "unexpected storage expr: " ++ show st
    ]

pairTypeCheckTests :: TestTree
pairTypeCheckTests = testGroup "Pair Type Checker Tests"
  [ testCase "pair<int,bool> return type is well-typed" $
      typeCheckSuccess
        "contract C { storage: { x: int }; \
        \@originate init(): pair<int, bool> { return pair(1, true); } }"

  , testCase "pair return type mismatch fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \@originate init(): pair<int, bool> { return pair(true, 1); } }"

  , testCase "fst returns first element type" $
      typeCheckSuccess
        "contract C { storage: { x: int }; \
        \@originate init(): int { return fst(pair(1, true)); } }"

  , testCase "snd returns second element type" $
      typeCheckSuccess
        "contract C { storage: { x: int }; \
        \@originate init(): bool { return snd(pair(1, true)); } }"

  , testCase "fst on non-pair fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \@originate init(): int { return fst(1); } }"

  , testCase "snd on non-pair fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \@originate init(): bool { return snd(true); } }"

  , testCase "var destructuring binds both names with correct types" $
      typeCheckSuccess
        "contract C { storage: { x: int }; \
        \@originate init(): int { var (a, b): pair<int, bool> = pair(10, true); return a; } }"

  , testCase "val destructuring binds both names with correct types" $
      typeCheckSuccess
        "contract C { storage: { x: int }; \
        \@originate init(): bool { val (a, b): pair<int, bool> = pair(10, true); return b; } }"

  , testCase "destructuring type annotation mismatch fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \@originate init(): unit { var (a, b): pair<bool, int> = pair(1, true); return (); } }"

  , testCase "destructuring non-pair type annotation fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \@originate init(): unit { var (a, b): int = pair(1, true); return (); } }"

  , testCase "pair in storage field is well-typed" $
      typeCheckSuccess
        "contract C { storage: { p: pair<int, bool> }; \
        \@originate init(): unit { storage.p = pair(0, false); return (); } }"
  ]

enumTypeCheckTests :: TestTree
enumTypeCheckTests = testGroup "Enum Type Checker Tests"
  [ testCase "enum literal is well-typed" $
      typeCheckSuccess
        "contract C { storage: { x: int }; \
        \enum Color { Red, Green } \
        \@originate init(): Color { return Red; } }"

  , testCase "unknown enum variant fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \enum Color { Red, Green } \
        \@originate init(): Color { return Blue; } }"

  , testCase "enum stored in storage field is well-typed" $
      typeCheckSuccess
        "contract C { storage: { c: Color }; \
        \enum Color { Red, Green } \
        \@originate init(): unit { storage.c = Red; return (); } }"

  , testCase "exhaustive match is well-typed" $
      typeCheckSuccess
        "contract C { storage: { x: int }; \
        \enum Color { Red, Green, Blue } \
        \@originate init(): unit { \
        \  match (Red) { \
        \    Red => { storage.x = 0; } \
        \    Green => { storage.x = 1; } \
        \    Blue => { storage.x = 2; } \
        \  } \
        \  return (); } }"

  , testCase "non-exhaustive match fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \enum Color { Red, Green, Blue } \
        \@originate init(): unit { \
        \  match (Red) { \
        \    Red => { storage.x = 0; } \
        \    Green => { storage.x = 1; } \
        \  } \
        \  return (); } }"

  , testCase "duplicate match arm fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \enum Color { Red, Green } \
        \@originate init(): unit { \
        \  match (Red) { \
        \    Red => { storage.x = 0; } \
        \    Red => { storage.x = 1; } \
        \    Green => { storage.x = 2; } \
        \  } \
        \  return (); } }"

  , testCase "match on non-enum type fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \@originate init(): unit { \
        \  match (1) { } \
        \  return (); } }"

  , testCase "duplicate enum name fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \enum Color { Red } \
        \enum Color { Blue } \
        \@originate init(): unit { return (); } }"

  , testCase "duplicate variant across enums fails" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \enum A { Same } \
        \enum B { Same } \
        \@originate init(): unit { return (); } }"

  , testCase "enum type equality is nominal - different enums with different variants" $
      typeCheckFailure
        "contract C { storage: { x: int }; \
        \enum A { ValA } \
        \enum B { ValB } \
        \@originate init(): A { return ValB; } }"
  ]

pairEnumParserTests :: TestTree
pairEnumParserTests = testGroup "Pair and Enum Parser Tests"
  [ testCase "pair<int, bool> as parameter type" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(p: pair<int, bool>): unit { return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ [FormalParameter "p" (TPair TInt TBool)] _ _] ->
              return ()
            _ -> assertFailure $ "Expected pair param type, got: " ++ show contract

  , testCase "pair<pair<int,int>, bool> nested pair type" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(p: pair<pair<int, int>, bool>): unit { return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ [FormalParameter "p" (TPair (TPair TInt TInt) TBool)] _ _] ->
              return ()
            _ -> assertFailure $ "Expected nested pair type, got: " ++ show contract

  , testCase "pair as storage field type" $
      parseSuccess
        "contract T { storage: { p: pair<int, bool> }; \
        \@originate init(): unit { return (); } }"
        $ \contract ->
          case contract of
            Contract _ [("p", TPair TInt TBool)] _ _ -> return ()
            _ -> assertFailure $ "Expected pair storage field, got: " ++ show contract

  , testCase "pair(e1, e2) constructor expression" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): unit { var p: pair<int, bool> = pair(1, true); return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt (VarDeclStmt "p" (TPair TInt TBool) (PairExpr (CInt 1) (CBool True)) : _))] ->
              return ()
            _ -> assertFailure $ "Expected PairExpr, got: " ++ show contract

  , testCase "fst(pair(1, true)) expression" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): unit { var n: int = fst(pair(1, true)); return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt (VarDeclStmt _ _ (Fst (PairExpr (CInt 1) (CBool True))) : _))] ->
              return ()
            _ -> assertFailure $ "Expected Fst, got: " ++ show contract

  , testCase "snd(pair(1, true)) expression" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): unit { var b: bool = snd(pair(1, true)); return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt (VarDeclStmt _ _ (Snd (PairExpr (CInt 1) (CBool True))) : _))] ->
              return ()
            _ -> assertFailure $ "Expected Snd, got: " ++ show contract

  , testCase "EnumLiteral - uppercase-initial identifier in return" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): unit { return Red; } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (EnumLiteral "Red")])] ->
              return ()
            _ -> assertFailure $ "Expected EnumLiteral Red, got: " ++ show contract

  , testCase "TEnum as return type annotation" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): Color { return Red; } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ (TEnum "Color") (SequenceStmt [ReturnStmt (EnumLiteral "Red")])] ->
              return ()
            _ -> assertFailure $ "Expected TEnum return type with EnumLiteral, got: " ++ show contract

  , testCase "pair is reserved - cannot be variable name" $
      parseFailure
        "contract T { storage: { x: int }; \
        \@originate init(): unit { var pair: int = 0; return (); } }"

  , testCase "fst is reserved - cannot be variable name" $
      parseFailure
        "contract T { storage: { x: int }; \
        \@originate init(): unit { var fst: int = 0; return (); } }"

  , testCase "snd is reserved - cannot be variable name" $
      parseFailure
        "contract T { storage: { x: int }; \
        \@originate init(): unit { var snd: int = 0; return (); } }"

  , testCase "var (x, y): pair<int, bool> = pair(1, true);" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): unit { var (a, b): pair<int, bool> = pair(1, true); return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt (VarDestructStmt "a" "b" (TPair TInt TBool) (PairExpr (CInt 1) (CBool True)) : _))] ->
              return ()
            _ -> assertFailure $ "Expected VarDestructStmt, got: " ++ show contract

  , testCase "val (x, y): pair<int, bool> = pair(1, true);" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): unit { val (a, b): pair<int, bool> = pair(1, true); return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt (ValDestructStmt "a" "b" (TPair TInt TBool) (PairExpr (CInt 1) (CBool True)) : _))] ->
              return ()
            _ -> assertFailure $ "Expected ValDestructStmt, got: " ++ show contract

  , testCase "match statement with two arms" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \@originate init(): unit { match (Red) { Red => { storage.x = 0; } Blue => { storage.x = 1; } } return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ _ [MethodDecl _ _ _ _ (SequenceStmt (MatchStmt (EnumLiteral "Red") arms : _))] ->
              assertEqual "Should have 2 arms" 2 (length arms)
            _ -> assertFailure $ "Expected MatchStmt, got: " ++ show contract

  , testCase "enum declaration inside contract" $
      parseSuccess
        "contract T { storage: { x: int }; enum Color { Red, Green, Blue } @originate init(): unit { return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ [EnumDecl "Color" variants] _ ->
              assertEqual "Should have 3 variants" ["Red", "Green", "Blue"] variants
            _ -> assertFailure $ "Expected EnumDecl, got: " ++ show contract

  , testCase "multiple enum declarations" $
      parseSuccess
        "contract T { storage: { x: int }; \
        \enum Color { Red, Blue } \
        \enum Dir { North, South } \
        \@originate init(): unit { return (); } }"
        $ \contract ->
          case contract of
            Contract _ _ enums _ ->
              assertEqual "Should have 2 enums" 2 (length enums)
  ]

pairInterpreterTests :: TestTree
pairInterpreterTests = testGroup "Pair Interpreter Tests"
  [ testCase "pair storage field persists via JSON roundtrip" $
      parseSuccess
        "contract C { storage: { p: pair<int, bool> }; \
        \@originate init(): unit { storage.p = pair(0, false); return (); } }"
        $ \c ->
          case contractInstanceFromStorageValue c
                 (object ["p" .= toJSON [toJSON (0 :: Int), toJSON False]]) of
            Left err -> assertFailure err
            Right (ContractInstance _ st) ->
              case st of
                Record [("p", PairExpr (CInt 0) (CBool False))] -> return ()
                _ -> assertFailure $ "unexpected storage: " ++ show st

  , testCase "originate contract with pair storage" $
      parseSuccess
        "contract C { storage: { p: pair<int, bool> }; \
        \@originate init(n: int, b: bool): unit { storage.p = pair(n, b); return (); } }"
        $ \c -> do
          let args = object ["n" .= (42 :: Int), "b" .= True]
          case originateWithJsonArgs M.empty c "src" args of
            Left err -> assertFailure err
            Right (_, repo) ->
              case M.elems repo of
                [ContractInstance _ (Record [("p", PairExpr (CInt 42) (CBool True))])] -> return ()
                xs -> assertFailure $ "unexpected repo: " ++ show xs

  , testCase "val destructuring binds first element at runtime" $
      parseSuccess
        "contract C { storage: { n: int }; \
        \@originate init(): unit { \
        \  val (a, b): pair<int, bool> = pair(7, true); \
        \  storage.n = a; \
        \  return (); } }"
        $ \c -> do
          case originateWithJsonArgs M.empty c "src" (object []) of
            Left err -> assertFailure err
            Right (_, repo) ->
              case M.elems repo of
                [ContractInstance _ (Record [("n", CInt 7)])] -> return ()
                xs -> assertFailure $ "unexpected repo: " ++ show xs
  ]

enumInterpreterTests :: TestTree
enumInterpreterTests = testGroup "Enum Interpreter Tests"
  [ testCase "enum storage field persists via JSON roundtrip" $
      parseSuccess
        "contract C { storage: { s: Status }; \
        \enum Status { Active, Inactive } \
        \@originate init(): unit { storage.s = Active; return (); } }"
        $ \c ->
          case contractInstanceFromStorageValue c
                 (object ["s" .= ("Active" :: String)]) of
            Left err -> assertFailure err
            Right (ContractInstance _ st) ->
              case st of
                Record [("s", EnumLiteral "Active")] -> return ()
                _ -> assertFailure $ "unexpected storage: " ++ show st

  , testCase "originate sets enum storage" $
      parseSuccess
        "contract C { storage: { s: Status }; \
        \enum Status { Active, Inactive } \
        \@originate init(): unit { storage.s = Active; return (); } }"
        $ \c ->
          case originateWithJsonArgs M.empty c "src" (object []) of
            Left err -> assertFailure err
            Right (_, repo) ->
              case M.elems repo of
                [ContractInstance _ (Record [("s", EnumLiteral "Active")])] -> return ()
                xs -> assertFailure $ "unexpected repo: " ++ show xs

  , testCase "match executes correct arm" $
      parseSuccess
        "contract C { storage: { n: int }; \
        \enum Color { Red, Blue } \
        \@originate init(): unit { \
        \  match (Blue) { \
        \    Red  => { storage.n = 0; } \
        \    Blue => { storage.n = 99; } \
        \  } \
        \  return (); } }"
        $ \c ->
          case originateWithJsonArgs M.empty c "src" (object []) of
            Left err -> assertFailure err
            Right (_, repo) ->
              case M.elems repo of
                [ContractInstance _ (Record [("n", CInt 99)])] -> return ()
                xs -> assertFailure $ "unexpected repo: " ++ show xs
  ]

errorTests :: TestTree
errorTests = testGroup "Error Cases"
  [ testCase "Missing contract keyword" $
      parseFailure "MyContract { storage: { x: int }; @originate init(): int { return 0; } }"
  
  , testCase "Missing storage declaration" $
      parseFailure "contract Test { @originate init(): int { return 0; } }"
  
  , testCase "Invalid storage syntax" $
      parseFailure "contract Test { storage x: int; @originate init(): int { return 0; } }"
  
  , testCase "Missing method decorator (defaults to Private)" $
      parseSuccess "contract Test { storage: { x: int }; init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ _ [MethodDecl Private "init" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected method without decorator to default to Private"
  
  , testCase "Missing return type" $
      parseFailure "contract Test { storage: { x: int }; @entrypoint test() { return 0; } }"
  
  , testCase "Missing semicolon after statement" $
      parseFailure "contract Test { storage: { x: int }; @entrypoint test(): int { return 0 } }"
  
  , testCase "Invalid expression syntax" $
      parseFailure "contract Test { storage: { x: int }; @entrypoint test(): int { return +; } }"
  ]
