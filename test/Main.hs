{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import SmartTS.IR.AST
import SmartTS.Parser
import Data.Aeson (object, (.=))
import SmartTS.Interpreter
  ( ContractInstance (..)
  , contractInstanceFromStorageValue
  , execMethodWithInitialStorage
  , findEntryPointByName
  )
import SmartTS.CodeGen.CompileLLTZ (translateExpression, translateStatement)
import qualified SmartTS.IR.LLTZ.Core as L
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
        ]
    , typeCheckTests
    , forLoopTests
    , stringTests
    ]

-- Helper function to parse and assert success
parseSuccess :: String -> (ParsedContract -> Assertion) -> Assertion
parseSuccess input assertion = case parseContractFromString input of
  Left err      -> assertFailure $ "Parse failed: " ++ show err
  Right contract -> assertion contract

-- Helper function to parse and assert failure
parseFailure :: String -> Assertion
parseFailure input = case parseContractFromString input of
  Left _  -> return ()  -- Expected failure
  Right _ -> assertFailure "Expected parse failure but got success"

typeCheckSuccess :: String -> Assertion
typeCheckSuccess input = case parseContractFromString input of
  Left err -> assertFailure $ "Parse failed: " ++ show err
  Right c ->
    case typeCheckContract c of
      Left err -> assertFailure $ "Type check failed: " ++ err
      Right _  -> return ()

typeCheckFailure :: String -> Assertion
typeCheckFailure input = case parseContractFromString input of
  Left err -> assertFailure $ "Parse failed (need valid parse for type test): " ++ show err
  Right c ->
    case typeCheckContract c of
      Left _  -> return ()
      Right _ -> assertFailure "Expected type error but checking succeeded"

contractTests :: TestTree
contractTests = testGroup "Contract Parsing"
  [ testCase "Simple contract with storage and method" $
      parseSuccess "contract MyContract { storage: { x: int }; @originate init(): int { return 0; } }" $ \contract ->
        case contract of
          Contract "MyContract" [( "x", TInt)] [MethodDecl Originate "init" [] TInt (SequenceStmt [ReturnStmt (CInt _ 0)])] ->
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

  , testCase "String literal" $
    parseSuccess
      "contract Test { storage: { x: int }; @entrypoint get(): string { return \"hello\"; } }"
      $ \contract ->
        case contract of
          Contract _ _
            [MethodDecl _ "get" [] TString
              (SequenceStmt [ReturnStmt (CString () "hello")])]
              -> return ()
          _ -> assertFailure $ "Expected string literal, got: " ++ show contract
  
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
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (CInt _ 42)])] ->
            return ()
          _ -> assertFailure $ "Expected integer literal 42, got: " ++ show contract

  , testCase "Variable reference" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Var _ "x")])] ->
            return ()
          _ -> assertFailure $ "Expected variable reference, got: " ++ show contract

  , testCase "Addition expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 1 + 2; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Add _ (CInt _ 1) (CInt _ 2))])] ->
            return ()
          _ -> assertFailure $ "Expected addition expression, got: " ++ show contract

  , testCase "Subtraction expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 5 - 3; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Sub _ (CInt _ 5) (CInt _ 3))])] ->
            return ()
          _ -> assertFailure $ "Expected subtraction expression, got: " ++ show contract

  , testCase "Chained addition" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 1 + 2 + 3; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt expr])] -> do
            -- Should parse as (1 + 2) + 3 due to left associativity
            case expr of
              Add _ (Add _ (CInt _ 1) (CInt _ 2)) (CInt _ 3) ->
                return ()
              _ -> assertFailure $ "Expected left-associative addition, got: " ++ show expr
          _ -> assertFailure $ "Unexpected expression structure: " ++ show contract

  , testCase "Mixed addition and subtraction" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 10 - 2 + 3; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt expr])] -> do
            -- Should parse as (10 - 2) + 3 due to left associativity
            case expr of
              Add _ (Sub _ (CInt _ 10) (CInt _ 2)) (CInt _ 3) ->
                return ()
              _ -> assertFailure $ "Expected left-associative mixed operations, got: " ++ show expr
          _ -> assertFailure $ "Unexpected expression structure: " ++ show contract

  , testCase "Parenthesized expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return (1 + 2); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Add _ (CInt _ 1) (CInt _ 2))])] ->
            return ()
          _ -> assertFailure $ "Expected parenthesized addition, got: " ++ show contract

  , testCase "Unit expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return (); } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (Unit _)])] ->
            return ()
          _ -> assertFailure $ "Expected unit expression, got: " ++ show contract

  , testCase "Boolean expression (&&) and boolean type" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint check(): bool { return true && false; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "check" [] TBool (SequenceStmt [ReturnStmt (And _ (CBool _ True) (CBool _ False))])] ->
            return ()
          _ -> assertFailure $ "Expected boolean && expression, got: " ++ show contract

  , testCase "Not expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint notit(): bool { return !false; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "notit" [] TBool (SequenceStmt [ReturnStmt (Not _ (CBool _ False))])] ->
            return ()
          _ -> assertFailure $ "Expected !false, got: " ++ show contract

  , testCase "Relational expression (==)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint eq(): bool { return 1 == 2; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "eq" [] TBool (SequenceStmt [ReturnStmt (Eq _ (CInt _ 1) (CInt _ 2))])] ->
            return ()
          _ -> assertFailure $ "Expected 1 == 2, got: " ++ show contract

  , testCase "Mul/Div/Mod expressions" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint arith(): int { return 6 * 7; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "arith" [] TInt (SequenceStmt [ReturnStmt (Mul _ (CInt _ 6) (CInt _ 7))])] ->
            return ()
          _ -> assertFailure $ "Expected 6 * 7, got: " ++ show contract

  , testCase "Record type and record literal" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint get(): { a: int, b: bool } { return { a: 1, b: true }; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "get" [] (TRecord [("a", TInt), ("b", TBool)]) (SequenceStmt [ReturnStmt (Record _ [("a", CInt _ 1), ("b", CBool _ True)])])] ->
            return ()
          _ -> assertFailure $ "Expected record type/literal, got: " ++ show contract

  , testCase "Record field access (x.f)" $
      parseSuccess "contract Test { storage: { x: { a: int, b: bool } }; @entrypoint proj(): int { return x.a; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "proj" [] TInt
                (SequenceStmt [ReturnStmt (FieldAccess _ (Var _ "x") "a")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected projection x.a, got: " ++ show contract

  , testCase "Chained field access (x.a.b)" $
      parseSuccess "contract Test { storage: { x: { a: { b: int } } }; @entrypoint proj2(): int { return x.a.b; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "proj2" [] TInt
                (SequenceStmt
                  [ReturnStmt (FieldAccess _ (FieldAccess _ (Var _ "x") "a") "b")])]
            -> return ()
          _ -> assertFailure $ "Expected chained projection x.a.b, got: " ++ show contract

  , testCase "Chained field access on record literal (..a.b)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint litproj2(): int { return { a: { b: 1 } }.a.b; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "litproj2" [] TInt
                (SequenceStmt
                  [ReturnStmt
                    (FieldAccess _
                      (FieldAccess _
                        (Record _ [("a", Record _ [("b", CInt _ 1)])])
                        "a")
                      "b")])]
            -> return ()
          _ -> assertFailure $ "Expected chained projection on record literal, got: " ++ show contract

  , testCase "Field access on record literal" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint litproj(): int { return { a: 1, b: true }.a; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "litproj" [] TInt
                (SequenceStmt [ReturnStmt (FieldAccess _ (Record _ [("a", CInt _ 1), ("b", CBool _ True)]) "a")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected projection on record literal, got: " ++ show contract
  ]

statementTests :: TestTree
statementTests = testGroup "Statement Parsing"
  [ testCase "Return statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { return 42; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [ReturnStmt (CInt _ 42)])] ->
            return ()
          _ -> assertFailure $ "Expected return statement, got: " ++ show contract

  , testCase "Assignment statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint test(): int { x = 10; return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [AssignmentStmt (LVar "x") (CInt _ 10), ReturnStmt (Var _ "x")])] ->
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
          Contract _ _ [MethodDecl _ _ _ _ (SequenceStmt [AssignmentStmt (LVar "x") (Add _ (CInt _ 1) (CInt _ 2)), ReturnStmt (Var _ "x")])] ->
            return ()
          _ -> assertFailure "Expected assignment with expression"

  , testCase "If statement with else" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint f(): int { if (true) { return 1; } else { return 2; } } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "f" [] TInt (SequenceStmt [IfStmt (CBool _ True) (SequenceStmt [ReturnStmt (CInt _ 1)]) (Just (SequenceStmt [ReturnStmt (CInt _ 2)]))])] ->
            return ()
          _ -> assertFailure $ "Expected if/else, got: " ++ show contract

  , testCase "While statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint loop(): int { while (false) { x = 1; } return x; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "loop" [] TInt (SequenceStmt [WhileStmt (CBool _ False) (SequenceStmt [AssignmentStmt (LVar "x") (CInt _ 1)]) , ReturnStmt (Var _ "x")])] ->
            return ()
          _ -> assertFailure $ "Expected while statement, got: " ++ show contract

  , testCase "Field assignment statement (x.a = ...)" $
      parseSuccess "contract Test { storage: { x: { a: int } }; @entrypoint fa(): int { x.a = 3; return x.a; } }" $ \contract ->
        case contract of
          Contract _ _
            [MethodDecl _ "fa" [] TInt
              (SequenceStmt
                [ AssignmentStmt (LField (LVar "x") "a") (CInt _ 3)
                , ReturnStmt (FieldAccess _ (Var _ "x") "a")
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
                    (CInt _ 3)
                , ReturnStmt
                    (FieldAccess _ (FieldAccess _ (Var _ "x") "a") "b")
                ])] ->
              return ()
          _ -> assertFailure $ "Expected x.a.b assignment, got: " ++ show contract

  , testCase "Storage expression read (storage.x)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint sr(): int { return storage.x; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "sr" [] TInt
                (SequenceStmt
                  [ReturnStmt (FieldAccess _ (StorageExpr _) "x")])
            ] ->
              return ()
          _ -> assertFailure $ "Expected storage read, got: " ++ show contract

  , testCase "Storage expression write (storage.x = ...)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint sw(): int { storage.x = 10; return storage.x; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "sw" [] TInt
                (SequenceStmt
                  [ AssignmentStmt (LField LStorage "x") (CInt _ 10)
                  , ReturnStmt (FieldAccess _ (StorageExpr _) "x")
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
                  [ VarDeclStmt "y" TInt (CInt _ 10)
                  , AssignmentStmt (LVar "y") (CInt _ 11)
                  , ReturnStmt (Var _ "y")
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
                  [ ValDeclStmt "y" TInt (CInt _ 10)
                  , ReturnStmt (Var _ "y")
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
                  [ VarDeclStmt "t" (TRecord [("a", TInt)]) (Var _ "x")
                  , AssignmentStmt (LField (LVar "t") "a") (CInt _ 7)
                  , ReturnStmt (FieldAccess _ (Var _ "t") "a")
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected local record field assignment, got: " ++ show contract
  , testCase "For statement" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint f(): int { for (var i: int = 0; i < 1; i = i + 1) { x = x + 1; } return x; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "f" [] TInt
                (SequenceStmt
                  [ ForStmt (VarDeclStmt "i" TInt (CInt _ 0)) (Lt _ (Var _ "i") (CInt _ 1)) (AssignmentStmt (LVar "i") (Add _ (Var _ "i") (CInt _ 1))) (SequenceStmt [AssignmentStmt (LVar "x") (Add _ (Var _ "x") (CInt _ 1))])
                  , ReturnStmt (Var _ "x")
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected for statement, got: " ++ show contract
  ]

typeCheckTests :: TestTree
typeCheckTests =
  testGroup
    "Type checker"
    [ testCase "Minimal well-typed contract" $
        typeCheckSuccess
          "contract C { storage: { x: int }; @originate init(): int { return 0; } }"
    , testCase "For loop type checks" $
        typeCheckSuccess
          "contract C { storage: { x: int }; @originate init(): int { storage.x = 0; return 0; } @entrypoint inc(): int { for (var i: int = 0; i < 3; i = i + 1) { storage.x = storage.x + 1; } return storage.x; } }"
    , testCase "Return type mismatch" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { return true; } }"
    , testCase "Arithmetic requires int" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { return 1 + true; } }"
    , testCase "String variable declaration" $
      typeCheckSuccess
        "contract C { storage: { name: string }; @originate init(): unit { var s: string = \"hello\"; return (); } }"
        
    , testCase "String return type" $
        typeCheckSuccess
          "contract C { storage: { name: string }; @originate init(): string { return \"hello\"; } }"

    , testCase "String type mismatch" $
        typeCheckFailure
          "contract C { storage: { name: string }; @originate init(): string { return 42; } }"
          
    , testCase "Cannot assign to val" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { val v: int = 1; v = 2; return 0; } }"
    , testCase "Equality requires same types" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): bool { return 1 == true; } }"
    , testCase "If condition must be bool" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { if (1) { return 0; } else { return 1; } } }"
    , testCase "For condition must be bool" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { storage.x = 0; return 0; } @entrypoint bad(): int { for (var i: int = 0; i + 1; i = i + 1) { storage.x = storage.x + 1; } return storage.x; } }"
    , testCase "Loop variable not visible after loop" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { storage.x = 0; return 0; } @entrypoint outscope(): int { for (var i: int = 0; i < 1; i = i + 1) { storage.x = storage.x + 1; } return i; } }"
    , testCase "Shadowing loop init fails" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): int { storage.x = 0; return 0; } @entrypoint shadow(): int { var y: int = 10; for (var y: int = 0; y < 1; y = y + 1) { storage.x = storage.x + 1; } return y; } }"
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
                Record _ [("n", CInt _ 1), ("b", CBool _ True)] -> return ()
                _ -> assertFailure $ "unexpected storage expr: " ++ show st
    , testCase "String concatenation" $
        typeCheckSuccess
          "contract C { storage: {}; @originate init(): string { return \"ab\" + \"cd\"; } }"

    , testCase "Concatenation rejects mixed operands" $
        typeCheckFailure
          "contract C { storage: {}; @originate init(): string { return \"a\" + 1; } }"

    , testCase "Length accepts string" $
        typeCheckSuccess
          "contract C { storage: { s: string }; @originate init(): int { return length(\"hello\"); } }"

    , testCase "Length rejects int" $
        typeCheckFailure
          "contract C { storage: { s: string }; @originate init(): int { return length(123); } }"

    , testCase "Length requires one argument" $
        typeCheckFailure
          "contract C { storage: { s: string }; @originate init(): int { return length(\"a\", \"b\"); } }"
    ]

-- | Run an entrypoint of a source contract on the given storage and return its result.
runEntrypoint :: String -> Name -> TypedExpr -> Either String TypedExpr
runEntrypoint input name storage = do
  parsed <- either (Left . show) Right (parseContractFromString input)
  contract <- typeCheckContract parsed
  method <- findEntryPointByName contract name
  (ret, _) <- execMethodWithInitialStorage contract storage method mempty
  maybe (Left "entrypoint returned no value") Right ret

forLoopTests :: TestTree
forLoopTests =
  testGroup
    "For loop"
    [ testCase "Interpreter sums 1..4 in a for loop" $
        case runEntrypoint
               "contract C { storage: {}; @entrypoint f(): int { var s: int = 0; for (var i: int = 1; i <= 4; i = i + 1) { s = s + i; } return s; } }"
               "f"
               (Record (TRecord []) [])
          of
          Right (CInt _ 10) -> return ()
          other -> assertFailure $ "Expected 10, got: " ++ show other
    , testCase "Return inside the body leaves the loop" $
        case runEntrypoint
               "contract C { storage: {}; @entrypoint f(): int { for (var i: int = 0; i < 10; i = i + 1) { if (i == 3) { return i; } } return 99; } }"
               "f"
               (Record (TRecord []) [])
          of
          Right (CInt _ 3) -> return ()
          other -> assertFailure $ "Expected 3, got: " ++ show other
    , testCase "Update clause must be an assignment" $
        parseFailure
          "contract C { storage: {}; @entrypoint f(): int { for (var i: int = 0; i < 1; var j: int = 0) { } return 0; } }"
    , testCase "Identifiers starting with `for` are not keywords" $
        typeCheckSuccess
          "contract C { storage: {}; @entrypoint f(): int { var format: int = 1; format = format + 1; return format; } }"
    , testCase "For translates to an LLTZ For node" $
        let loop =
              ForStmt
                (VarDeclStmt "i" TInt (CInt TInt 0))
                (Lt TBool (Var TInt "i") (CInt TInt 3))
                (AssignmentStmt (LVar "i") (Add TInt (Var TInt "i") (CInt TInt 1)))
                (SequenceStmt [])
         in case translateStatement loop of
              L.Expr (L.For (L.MutVar "i") (L.Expr (L.Const (L.CInt 0)) L.TInt) _ _ _) L.TUnit ->
                return ()
              other -> assertFailure $ "Expected an LLTZ For node, got: " ++ show other
    ]

stringTests :: TestTree
stringTests =
  testGroup
    "Strings"
    [ testCase "Interpreter concatenates strings and computes length" $
        case runEntrypoint
               "contract C { storage: {}; @entrypoint f(): int { val s: string = \"Hello, \" + \"Ana!\"; return length(s); } }"
               "f"
               (Record (TRecord []) [])
          of
          Right (CInt _ 11) -> return ()
          other -> assertFailure $ "Expected 11, got: " ++ show other
    , testCase "Interpreter returns the concatenated string" $
        case runEntrypoint
               "contract C { storage: {}; @entrypoint f(): string { return \"ab\" + \"cd\"; } }"
               "f"
               (Record (TRecord []) [])
          of
          Right (CString _ "abcd") -> return ()
          other -> assertFailure $ "Expected \"abcd\", got: " ++ show other
    , testCase "`length` is reserved and cannot name a method" $
        parseFailure
          "contract C { storage: {}; @private length(x: int): int { return x; } }"
    , testCase "`length` is reserved and cannot name a variable" $
        parseFailure
          "contract C { storage: {}; @entrypoint f(): int { val length: int = 1; return length; } }"
    , testCase "Concatenation translates to PrimConcat2" $
        case translateExpression (Add TString (CString TString "a") (CString TString "b")) of
          L.Expr (L.Prim L.PrimConcat2 [_, _]) L.TString -> return ()
          other -> assertFailure $ "Expected PrimConcat2, got: " ++ show other
    , testCase "length translates to PrimSize" $
        case translateExpression (Call TInt "length" [CString TString "ab"]) of
          L.Expr (L.Prim L.PrimSize [_]) L.TInt -> return ()
          other -> assertFailure $ "Expected PrimSize, got: " ++ show other
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
