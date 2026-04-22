{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import SmartTS.AST
import SmartTS.Parser
import Data.Aeson (object, (.=))
import SmartTS.Interpreter (ContractInstance (..), contractInstanceFromStorageValue)
import SmartTS.TypeCheck (typeCheckContract)

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
        , stringTests
        ]
    , typeCheckTests
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
          Contract "MyContract" [( "x", TInt)] [MethodDecl Originate "init" [] TInt (SequenceStmt [ReturnStmt (CInt 0)])] ->
            return ()
          _ -> assertFailure $ "Unexpected contract structure: " ++ show contract
  
  , testCase "Contract with multiple storage fields" $
      parseSuccess "contract Test { storage: { x: int, y: int }; @entrypoint test(): int { return 1; } }" $ \contract ->
        case contract of
          Contract "Test" storage _ ->
            assertEqual "Storage should have 2 fields" 2 (length storage)
          _ -> assertFailure "Unexpected contract name"
  
  , testCase "Contract with multiple methods" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } @entrypoint inc(): int { return 1; } }" $ \contract ->
        case contract of
          Contract _ _ methods ->
            assertEqual "Should have 2 methods" 2 (length methods)
  ]

storageTests :: TestTree
storageTests = testGroup "Storage Parsing"
  [ testCase "Single storage field" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ [(name, typ)] _ -> do
            assertEqual "Storage field name" "x" name
            assertEqual "Storage field type" TInt typ
          _ -> assertFailure "Unexpected storage structure"
  
  , testCase "Multiple storage fields" $
      parseSuccess "contract Test { storage: { x: int, y: int, z: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ storage _ ->
            assertEqual "Should have 3 storage fields" 3 (length storage)
  
  , testCase "Storage with single field (no comma)" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ storage _ ->
            assertEqual "Should have 1 storage field" 1 (length storage)
  ]

methodTests :: TestTree
methodTests = testGroup "Method Parsing"
  [ testCase "Method with @originate decorator" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl Originate "init" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected @originate method"
  
  , testCase "Method with @entrypoint decorator" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl EntryPoint "test" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected @entrypoint method"
  
  , testCase "Method with @private decorator" $
      parseSuccess "contract Test { storage: { x: int }; @private helper(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl Private "helper" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected @private method"
  
  , testCase "Method with parameters" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint add(a: int, b: int): int { return a + b; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl EntryPoint "add" params TInt _] -> do
            assertEqual "Should have 2 parameters" 2 (length params)
            case params of
              [FormalParameter "a" TInt, FormalParameter "b" TInt] ->
                return ()
              _ -> assertFailure "Unexpected parameter structure"
          _ -> assertFailure "Unexpected method structure"
  
  , testCase "Method with single parameter" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint inc(x: int): int { return x + 1; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ params _ _] ->
            assertEqual "Should have 1 parameter" 1 (length params)
          _ -> assertFailure "Unexpected method structure"
  
  , testCase "Method with empty parameter list" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ params _ _] ->
            assertEqual "Should have 0 parameters" 0 (length params)
          _ -> assertFailure "Unexpected method structure"
  ]

expressionTests :: TestTree
expressionTests = testGroup "Expression Parsing"
  [ testCase "Integer literal" $
      parseSuccess "contract Test { storage: { x: int }; @originate init(): int { return 42; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (CInt 42)])] ->
            return ()
          _ -> assertFailure $ "Expected integer literal 42, got: " ++ show contract
  
  , testCase "Variable reference" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure $ "Expected variable reference, got: " ++ show contract
  
  , testCase "Addition expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 1 + 2; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Add (CInt 1) (CInt 2))])] ->
            return ()
          _ -> assertFailure $ "Expected addition expression, got: " ++ show contract
  
  , testCase "Subtraction expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 5 - 3; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Sub (CInt 5) (CInt 3))])] ->
            return ()
          _ -> assertFailure $ "Expected subtraction expression, got: " ++ show contract
  
  , testCase "Chained addition" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 1 + 2 + 3; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt expr])] -> do
            -- Should parse as (1 + 2) + 3 due to left associativity
            case expr of
              Add (Add (CInt 1) (CInt 2)) (CInt 3) ->
                return ()
              _ -> assertFailure $ "Expected left-associative addition, got: " ++ show expr
          _ -> assertFailure $ "Unexpected expression structure: " ++ show contract
  
  , testCase "Mixed addition and subtraction" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 10 - 2 + 3; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt expr])] -> do
            -- Should parse as (10 - 2) + 3 due to left associativity
            case expr of
              Add (Sub (CInt 10) (CInt 2)) (CInt 3) ->
                return ()
              _ -> assertFailure $ "Expected left-associative mixed operations, got: " ++ show expr
          _ -> assertFailure $ "Unexpected expression structure: " ++ show contract
  
  , testCase "Parenthesized expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return (1 + 2); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Add (CInt 1) (CInt 2))])] ->
            return ()
          _ -> assertFailure $ "Expected parenthesized addition, got: " ++ show contract
  
  , testCase "Unit expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return (); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt Unit])] ->
            return ()
          _ -> assertFailure $ "Expected unit expression, got: " ++ show contract

  , testCase "Boolean expression (&&) and boolean type" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint check(): bool { return true && false; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "check" [] TBool (SequenceStmt [ReturnStmt (And (CBool True) (CBool False))])] ->
            return ()
          _ -> assertFailure $ "Expected boolean && expression, got: " ++ show contract

  , testCase "Not expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint notit(): bool { return !false; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "notit" [] TBool (SequenceStmt [ReturnStmt (Not (CBool False))])] ->
            return ()
          _ -> assertFailure $ "Expected !false, got: " ++ show contract

  , testCase "Relational expression (==)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint eq(): bool { return 1 == 2; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "eq" [] TBool (SequenceStmt [ReturnStmt (Eq (CInt 1) (CInt 2))])] ->
            return ()
          _ -> assertFailure $ "Expected 1 == 2, got: " ++ show contract

  , testCase "Mul/Div/Mod expressions" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint arith(): int { return 6 * 7; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "arith" [] TInt (SequenceStmt [ReturnStmt (Mul (CInt 6) (CInt 7))])] ->
            return ()
          _ -> assertFailure $ "Expected 6 * 7, got: " ++ show contract

  , testCase "Record type and record literal" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint get(): { a: int, b: bool } { return { a: 1, b: true }; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "get" [] (TRecord [("a", TInt), ("b", TBool)]) (SequenceStmt [ReturnStmt (Record [("a", CInt 1), ("b", CBool True)])])] ->
            return ()
          _ -> assertFailure $ "Expected record type/literal, got: " ++ show contract

  , testCase "Record field access (x.f)" $
      parseSuccess "contract Test { storage: { x: { a: int, b: bool } }; @entrypoint proj(): int { return x.a; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "proj" [] TInt
                (SequenceStmt [ReturnStmt (FieldAccess (Var "x") "a")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected projection x.a, got: " ++ show contract

  , testCase "Chained field access (x.a.b)" $
      parseSuccess "contract Test { storage: { x: { a: { b: int } } }; @entrypoint proj2(): int { return x.a.b; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "proj2" [] TInt
                (SequenceStmt
                  [ReturnStmt (FieldAccess (FieldAccess (Var "x") "a") "b")])]
            -> return ()
          _ -> assertFailure $ "Expected chained projection x.a.b, got: " ++ show contract

  , testCase "Chained field access on record literal (..a.b)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint litproj2(): int { return { a: { b: 1 } }.a.b; } }" $ \contract ->
        case contract of
          Contract _ _
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
          Contract _ _
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
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (CInt 42)])] ->
            return ()
          _ -> assertFailure $ "Expected return statement, got: " ++ show contract
  
  , testCase "Assignment statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { x = 10; return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [AssignmentStmt (LVar "x") (CInt 10), ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure "Expected assignment and return statements"
  
  , testCase "Multiple statements in block" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { x = 1; x = 2; return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt stmts)] ->
            assertEqual "Should have 3 statements" 3 (length stmts)
          _ -> assertFailure "Unexpected statement structure"
  
  , testCase "Assignment with expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { x = 1 + 2; return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [AssignmentStmt (LVar "x") (Add (CInt 1) (CInt 2)), ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure "Expected assignment with expression"

  , testCase "If statement with else" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint f(): int { if (true) { return 1; } else { return 2; } } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "f" [] TInt (SequenceStmt [IfStmt (CBool True) (SequenceStmt [ReturnStmt (CInt 1)]) (Just (SequenceStmt [ReturnStmt (CInt 2)]))])] ->
            return ()
          _ -> assertFailure $ "Expected if/else, got: " ++ show contract

  , testCase "While statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint loop(): int { while (false) { x = 1; } return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "loop" [] TInt (SequenceStmt [WhileStmt (CBool False) (SequenceStmt [AssignmentStmt (LVar "x") (CInt 1)]) , ReturnStmt (Var "x")])] ->
            return ()
          _ -> assertFailure $ "Expected while statement, got: " ++ show contract

  , testCase "Field assignment statement (x.a = ...)" $
      parseSuccess "contract Test { storage: { x: { a: int } }; @entrypoint fa(): int { x.a = 3; return x.a; } }" $ \contract ->
        case contract of
          Contract _ _
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
          Contract _ _
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
          Contract _ _
            [ MethodDecl _ "sr" [] TInt
                (SequenceStmt
                  [ReturnStmt (FieldAccess StorageExpr "x")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected storage read, got: " ++ show contract

  , testCase "Storage expression write (storage.x = ...)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint sw(): int { storage.x = 10; return storage.x; } }" $ \contract ->
        case contract of
          Contract _ _
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
          Contract _ _
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
          Contract _ _
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
          Contract _ _
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
          Contract _ _ [MethodDecl Private "init" [] TInt _] ->
            return ()
          _ -> assertFailure "Expected method without decorator to default to Private"
  
  , testCase "Missing return type" $
      parseFailure "contract Test { storage: { x: int }; @entrypoint test() { return 0; } }"
  
  , testCase "Missing semicolon after statement" $
      parseFailure "contract Test { storage: { x: int }; @entrypoint test(): int { return 0 } }"
  
  , testCase "Invalid expression syntax" $
      parseFailure "contract Test { storage: { x: int }; @entrypoint test(): int { return +; } }"
  ]

stringTests :: TestTree
stringTests = testGroup "String Type and Operations"
  [ testCase "String literal parsing" $
      parseSuccess "contract Test { storage: { x: string }; @originate init(): string { return \"hello\"; } }" $ \contract ->
        case contract of
          Contract _ [("x", TString)] [MethodDecl Originate "init" [] TString (SequenceStmt [ReturnStmt (CString "hello")])] ->
            return ()
          _ -> assertFailure $ "Expected string literal, got: " ++ show contract

  , testCase "String storage field type" $
      parseSuccess "contract Test { storage: { name: string }; @originate init(): unit { return (); } }" $ \contract ->
        case contract of
          Contract _ storage _ ->
            case lookup "name" storage of
              Just TString -> return ()
              _ -> assertFailure "Expected string type in storage"
          _ -> assertFailure "Unexpected contract structure"

  , testCase "String concatenation function call" $
      parseSuccess "contract Test { storage: { x: string }; @entrypoint concat(): string { return string_concat(\"hello\", \"world\"); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl EntryPoint "concat" [] TString (SequenceStmt [ReturnStmt (Call "string_concat" [CString "hello", CString "world"])])] ->
            return ()
          _ -> assertFailure $ "Expected string_concat function call, got: " ++ show contract

  , testCase "String length function call" $
      parseSuccess "contract Test { storage: { x: string }; @entrypoint len(): int { return string_length(\"test\"); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl EntryPoint "len" [] TInt (SequenceStmt [ReturnStmt (Call "string_length" [CString "test"])])] ->
            return ()
          _ -> assertFailure $ "Expected string_length function call, got: " ++ show contract

  , testCase "String uppercase function call" $
      parseSuccess "contract Test { storage: { x: string }; @entrypoint upper(): string { return string_uppercase(\"hello\"); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl EntryPoint "upper" [] TString (SequenceStmt [ReturnStmt (Call "string_uppercase" [CString "hello"])])] ->
            return ()
          _ -> assertFailure $ "Expected string_uppercase function call, got: " ++ show contract

  , testCase "String lowercase function call" $
      parseSuccess "contract Test { storage: { x: string }; @entrypoint lower(): string { return string_lowercase(\"HELLO\"); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl EntryPoint "lower" [] TString (SequenceStmt [ReturnStmt (Call "string_lowercase" [CString "HELLO"])])] ->
            return ()
          _ -> assertFailure $ "Expected string_lowercase function call, got: " ++ show contract

  , testCase "String equality check" $
      typeCheckSuccess "contract Test { storage: { x: string }; @originate init(): bool { return \"hello\" == \"hello\"; } }"

  , testCase "String inequality check" $
      typeCheckSuccess "contract Test { storage: { x: string }; @originate init(): bool { return \"hello\" != \"world\"; } }"

  , testCase "String concatenation with + operator" $
      typeCheckSuccess "contract Test { storage: { x: string }; @originate init(): string { return \"hello\" + \"world\"; } }"

  , testCase "String concatenation type check should fail for int + string" $
      typeCheckFailure "contract Test { storage: { x: string }; @originate init(): string { return 1 + \"world\"; } }"

  , testCase "String type in var declaration" $
      parseSuccess "contract Test { storage: { x: string }; @entrypoint test(): string { var s: string = \"hello\"; return s; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl EntryPoint "test" [] TString (SequenceStmt [VarDeclStmt "s" TString (CString "hello"), ReturnStmt (Var "s")])] ->
            return ()
          _ -> assertFailure $ "Expected var declaration with string type, got: " ++ show contract

  , testCase "String type in val declaration" $
      typeCheckSuccess "contract Test { storage: { x: string }; @entrypoint test(): string { val s: string = \"hello\"; return s; } }"

  , testCase "string_concat type check - requires both arguments to be strings" $
      typeCheckFailure "contract Test { storage: { x: string }; @originate init(): string { return string_concat(\"hello\", 123); } }"

  , testCase "string_length type check - requires argument to be string" $
      typeCheckFailure "contract Test { storage: { x: string }; @originate init(): int { return string_length(123); } }"

  , testCase "string_uppercase type check - requires argument to be string" $
      typeCheckFailure "contract Test { storage: { x: string }; @originate init(): string { return string_uppercase(123); } }"

  , testCase "string_lowercase type check - requires argument to be string" $
      typeCheckFailure "contract Test { storage: { x: string }; @originate init(): string { return string_lowercase(false); } }"

  , testCase "String with escape sequences" $
      parseSuccess "contract Test { storage: { x: string }; @originate init(): string { return \"hello\\nworld\"; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl Originate "init" [] TString (SequenceStmt [ReturnStmt (CString s)])] ->
            -- The parser should process escape sequences
            case s of
              "hello\nworld" -> return ()
              _ -> assertFailure $ "Expected escaped newline, got: " ++ show s
          _ -> assertFailure $ "Unexpected contract structure, got: " ++ show contract
  ]
