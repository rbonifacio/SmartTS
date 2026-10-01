{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import SmartTS.IR.AST
import SmartTS.Parser
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import SmartTS.Interpreter
  ( ContractInstance (..)
  , callEntrypointWithJsonArgs
  , contractInstanceFromStorageValue
  , execMethodWithInitialStorage
  , exprToJson
  , findEntryPointByName
  , jsonToExprByType
  , originateWithJsonArgs
  )
import SmartTS.CodeGen.CompileLLTZ (translateExpression, translateStatement, translateType)
import qualified SmartTS.IR.LLTZ.Core as L
import SmartTS.TypeCheck (typeCheckContract)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "SmartTS"
    [ codegenTests
      ,testGroup
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
    , codecTests
    , e2eTests
    , e2eMapCounterTests
    ]

codegenTests :: TestTree
codegenTests = testGroup "CodeGen: CompileLLTZ"
  [ testCase "MapMemCheck translates to PrimMem with order [key, map]" $
      let typed = MapMemCheck TBool (Var (TMap TInt TBool) "m") (Var TInt "k")
          result = translateExpression typed
      in case L.exprDesc result of
           L.Prim L.PrimMem [L.Expr (L.Variable (L.Var "k")) _, L.Expr (L.Variable (L.Var "m")) _] ->
             return ()
           other -> assertFailure $ "Expected Prim PrimMem [key, map], got: " ++ show other

  , testCase "MapAccess translates to IfNone over PrimGet, failing with a non-empty FAILWITH" $
    let typed = MapAccess TInt (Var (TMap TInt TInt) "m") (Var TInt "k")
        result = translateExpression typed
    in case L.exprDesc result of
         L.IfNone
           (L.Expr (L.Prim L.PrimGet [keyArg, mapArg]) (L.TOption L.TInt))
           (L.Expr (L.Prim L.PrimFailwith failArgs) _)
           (L.LambdaBinder (L.Var "__value", L.TInt) (L.Expr (L.Variable (L.Var "__value")) L.TInt)) -> do
             -- GET takes [key, map] in this order
             case (keyArg, mapArg) of
               (L.Expr (L.Variable (L.Var "k")) _, L.Expr (L.Variable (L.Var "m")) _) -> return ()
               _ -> assertFailure "PrimGet should take [key, map] in this order"
             -- FAILWITH takes exactly one argument (the value pushed before the instruction)
             case failArgs of
               [L.Expr (L.Const (L.CString _)) L.TString] -> return ()
               _ -> assertFailure $ "FAILWITH should take exactly one CString argument, got: " ++ show failArgs
         other -> assertFailure $ "Unexpected structure for MapAccess: " ++ show other
         
  , testCase "MapAccess keeps the value type as its result type" $
      let typed = MapAccess TBool (Var (TMap TInt TBool) "m") (Var TInt "k")
          result = translateExpression typed
      in L.exprType result @?= L.TBool

  , testCase "MapEmpty translates to PrimEmptyMap with the right types" $
      let typed = MapEmpty (TMap TInt TBool)
          result = translateExpression typed
      in case L.exprDesc result of
           L.Prim (L.PrimEmptyMap L.TInt L.TBool) [] -> return ()
           other -> assertFailure $ "Expected PrimEmptyMap TInt TBool, got: " ++ show other

  , testCase "MapEmpty keeps type TMap k v" $
      let typed = MapEmpty (TMap TBool TInt)
          result = translateExpression typed
      in L.exprType result @?= L.TMap L.TBool L.TInt

  , testCase "MapRem translates to PrimUpdate [key, PrimNone v, map]" $
      let typed = MapRem (TMap TInt TBool) (Var (TMap TInt TBool) "m") (Var TInt "k")
          result = translateExpression typed
      in case L.exprDesc result of
           L.Prim L.PrimUpdate
             [ L.Expr (L.Variable (L.Var "k")) _
             , L.Expr (L.Prim (L.PrimNone L.TBool) []) _
             , L.Expr (L.Variable (L.Var "m")) _
             ] -> return ()
           other -> assertFailure $ "Expected PrimUpdate [key, PrimNone v, map] for MapRem, got: "
                      ++ show other

  , testCase "MapRem keeps type TMap k v" $
      let typed = MapRem (TMap TInt TBool) (Var (TMap TInt TBool) "m") (Var TInt "k")
          result = translateExpression typed
      in L.exprType result @?= L.TMap L.TInt L.TBool

  , testCase "MapVal with one entry translates to PrimUpdate over PrimEmptyMap" $
      let typed = MapVal (TMap TInt TBool) (M.fromList [(CInt TInt 1, CBool TBool True)])
          result = translateExpression typed
      in case L.exprDesc result of
           L.Prim L.PrimUpdate [_, _, L.Expr (L.Prim (L.PrimEmptyMap L.TInt L.TBool) []) _] ->
             return ()
           other -> assertFailure $ "Expected PrimUpdate over PrimEmptyMap for MapVal, got: "
                      ++ show other

  , testCase "MapVal keeps type TMap k v" $
      let typed = MapVal (TMap TInt TBool) (M.fromList [(CInt TInt 1, CBool TBool True)])
          result = translateExpression typed
      in L.exprType result @?= L.TMap L.TInt L.TBool

  , testCase "translateType converts TMap TInt TBool" $
      translateType (TMap TInt TBool) @?= L.TMap L.TInt L.TBool

  , testCase "translateType converts a nested TMap (map-valued)" $
      translateType (TMap TBool (TMap TInt TBool)) @?= L.TMap L.TBool (L.TMap L.TInt L.TBool)

  , testCase "Map index assignment becomes Assign of PrimUpdate [key, Some val, map] with map types" $
      let stmt = AssignmentStmt (LMapAccess (LVar "m") (CInt TInt 1)) (CBool TBool True)
          mapTy = L.TMap L.TInt L.TBool
      in case translateStatement stmt of
           L.Expr (L.Assign (L.MutVar "m")
                    (L.Expr (L.Prim L.PrimUpdate
                              [ _
                              , L.Expr (L.Prim L.PrimSome [_]) (L.TOption L.TBool)
                              , L.Expr (L.Variable (L.Var "m")) readTy
                              ]) updTy)) L.TUnit ->
             (readTy, updTy) @?= (mapTy, mapTy)
           other -> assertFailure $ "Expected Assign (MutVar \"m\") (PrimUpdate ...), got: " ++ show other

  , testCase "Nested map assignment reads the inner map with its own type" $
      let stmt = AssignmentStmt
                   (LMapAccess (LMapAccess (LVar "m") (CInt TInt 1)) (CInt TInt 2))
                   (CInt TInt 5)
          inner = L.TMap L.TInt L.TInt
      in case translateStatement stmt of
           L.Expr (L.Assign (L.MutVar "m")
                    (L.Expr (L.Prim L.PrimUpdate [_, L.Expr (L.Prim L.PrimSome [innerUpd]) _, outer]) outerTy)) _ -> do
             L.exprType innerUpd @?= inner
             L.exprType outer @?= L.TMap L.TInt inner
             outerTy @?= L.TMap L.TInt inner
           other -> assertFailure $ "Expected nested map update, got: " ++ show other
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

  , testCase "Map type in return type (map<int, bool>)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint get(): map<int, bool> { return empty_map; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "get" [] (TMap TInt TBool) (SequenceStmt [ReturnStmt (MapEmpty _)])] ->
            return ()
          _ -> assertFailure $ "Expected map<int, bool> return type with empty_map, got: " ++ show contract

  , testCase "empty_map expression" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint get(): map<int, int> { return empty_map; } }" $ \contract ->
        case contract of
          Contract _ _ [MethodDecl _ "get" [] (TMap TInt TInt) (SequenceStmt [ReturnStmt (MapEmpty _)])] ->
            return ()
          _ -> assertFailure $ "Expected empty_map, got: " ++ show contract

  , testCase "Map index read (m[k])" $
      parseSuccess "contract Test { storage: { m: map<int, bool> }; @entrypoint get(k: int): bool { return m[k]; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "get" [FormalParameter "k" TInt] TBool
                (SequenceStmt [ReturnStmt (MapAccess _ (Var _ "m") (Var _ "k"))])
            ] ->
              return ()
          _ -> assertFailure $ "Expected m[k] map access, got: " ++ show contract

  , testCase "Map index read with literal key (m[0])" $
      parseSuccess "contract Test { storage: { m: map<int, bool> }; @entrypoint get(): bool { return m[0]; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "get" [] TBool
                (SequenceStmt [ReturnStmt (MapAccess _ (Var _ "m") (CInt _ 0))])
            ] ->
              return ()
          _ -> assertFailure $ "Expected m[0] map access, got: " ++ show contract

  , testCase "Nested storage map index read (storage.m[k])" $
      parseSuccess "contract Test { storage: { m: map<int, bool> }; @entrypoint get(k: int): bool { return storage.m[k]; } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "get" [FormalParameter "k" TInt] TBool
                (SequenceStmt
                  [ReturnStmt (MapAccess _ (FieldAccess _ (StorageExpr _) "m") (Var _ "k"))])
            ] ->
              return ()
          _ -> assertFailure $ "Expected storage.m[k] map access, got: " ++ show contract

  , testCase "mem(map, key) expression" $
      parseSuccess "contract Test { storage: { m: map<int, bool> }; @entrypoint has(k: int): bool { return mem(storage.m, k); } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "has" [FormalParameter "k" TInt] TBool
                (SequenceStmt
                  [ReturnStmt (MapMemCheck _ (FieldAccess _ (StorageExpr _) "m") (Var _ "k"))])
            ] ->
              return ()
          _ -> assertFailure $ "Expected mem(storage.m, k), got: " ++ show contract

  , testCase "remove(map, key) expression" $
      parseSuccess "contract Test { storage: { m: map<int, bool> }; @entrypoint drop(k: int): map<int, bool> { return remove(storage.m, k); } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "drop" [FormalParameter "k" TInt] (TMap TInt TBool)
                (SequenceStmt
                  [ReturnStmt (MapRem _ (FieldAccess _ (StorageExpr _) "m") (Var _ "k"))])
            ] ->
              return ()
          _ -> assertFailure $ "Expected remove(storage.m, k), got: " ++ show contract
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

  , testCase "var declaration with empty_map initializer" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint f(): unit { var m: map<int, int> = empty_map; return (); } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "f" [] TUnit
                (SequenceStmt
                  [ VarDeclStmt "m" (TMap TInt TInt) (MapEmpty _)
                  , ReturnStmt (Unit _)
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected var m: map<int,int> = empty_map, got: " ++ show contract

  , testCase "Map index assignment (m[k] = v)" $
      parseSuccess "contract Test { storage: { x: int }; @entrypoint f(): unit { var m: map<int, bool> = empty_map; m[1] = true; return (); } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "f" [] TUnit
                (SequenceStmt
                  [ VarDeclStmt "m" (TMap TInt TBool) (MapEmpty _)
                  , AssignmentStmt (LMapAccess (LVar "m") (CInt _ 1)) (CBool _ True)
                  , ReturnStmt (Unit _)
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected m[1] = true assignment, got: " ++ show contract

  , testCase "Storage map index assignment (storage.m[k] = v)" $
      parseSuccess "contract Test { storage: { m: map<int, bool> }; @entrypoint f(k: int): unit { storage.m[k] = true; return (); } }" $ \contract ->
        case contract of
          Contract _ _
            [ MethodDecl _ "f" [FormalParameter "k" TInt] TUnit
                (SequenceStmt
                  [ AssignmentStmt (LMapAccess (LField LStorage "m") (Var _ "k")) (CBool _ True)
                  , ReturnStmt (Unit _)
                  ])
            ] ->
              return ()
          _ -> assertFailure $ "Expected storage.m[k] = true assignment, got: " ++ show contract
  ]

typeCheckTests :: TestTree
typeCheckTests =
  testGroup
    "Type checker"
    [ 
      testCase "Minimal well-typed contract" $
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

    -- map<K, V>: valid cases

    , testCase "map<int, bool> is a valid storage type" $
        typeCheckSuccess
          "contract C { storage: { m: map<int, bool> }; @originate init(): unit { return (); } }"

    , testCase "map<bool, int> is a valid storage type" $
        typeCheckSuccess
          "contract C { storage: { m: map<bool, int> }; @originate init(): unit { return (); } }"

    , testCase "empty_map with a map<int, int> context is accepted" $
        typeCheckSuccess
          "contract C { storage: { m: map<int, int> }; @originate init(): unit { storage.m = empty_map; return (); } }"

    , testCase "Reading storage.m[k] has the value type" $
        typeCheckSuccess
          "contract C { storage: { m: map<int, int> }; @entrypoint get(k: int): int { return storage.m[k]; } }"

    , testCase "Writing storage.m[k] with matching types" $
        typeCheckSuccess
          "contract C { storage: { m: map<int, bool> }; @originate init(): unit { storage.m[0] = true; return (); } }"

    , testCase "Writing a local map variable with matching types" $
        typeCheckSuccess
          "contract C { storage: { x: int }; @entrypoint f(): unit { var m: map<int, int> = empty_map; m[1] = 42; return (); } }"

    , testCase "mem(map, key) returns bool" $
        typeCheckSuccess
          "contract C { storage: { m: map<int, bool> }; @entrypoint f(k: int): bool { return mem(storage.m, k); } }"

    , testCase "remove(map, key) returns map<K, V>" $
        typeCheckSuccess
          "contract C { storage: { m: map<int, bool> }; @entrypoint f(k: int): map<int, bool> { return remove(storage.m, k); } }"

    , testCase "empty_map inside a record literal takes the field type" $
        typeCheckSuccess
          "contract C { storage: { n: int, m: map<int, int> }; @originate init(): unit { storage = { n: 0, m: empty_map }; return (); } }"

    , testCase "string is a comparable key type" $
        typeCheckSuccess
          "contract C { storage: { m: map<string, int> }; @entrypoint f(k: string): unit { storage.m[k] = length(k); return (); } }"

    , testCase "map<int, map<int, bool>>: values may be maps" $
        typeCheckSuccess
          "contract C { storage: { m: map<int, map<int, bool>> }; @originate init(): unit { return (); } }"
    
    -- map<K, V>: invalid (non-comparable) keys and type errors
    
    , testCase "map<unit, int>: unit is not a comparable key" $
        typeCheckFailure
          "contract C { storage: { m: map<unit, int> }; @originate init(): unit { storage.m[()] = 1; return (); } }"

    , testCase "A map used as a key is rejected" $
        typeCheckFailure
          "contract C { storage: { m: map<int, int> }; @originate init(): unit { var k: map<int, int> = empty_map; storage.m[k] = 1; return (); } }"

    , testCase "empty_map with a record key is rejected" $
        typeCheckFailure
          "contract C { storage: { m: map<{ x: int }, bool> }; @originate init(): unit { storage.m = empty_map; return (); } }"

    , testCase "Reading storage.m[k] with the wrong key type is rejected" $
        typeCheckFailure
          "contract C { storage: { m: map<int, bool> }; @entrypoint f(k: bool): bool { return storage.m[k]; } }"

    , testCase "Writing storage.m[k] with the wrong value type is rejected" $
        typeCheckFailure
          "contract C { storage: { m: map<int, bool> }; @originate init(): unit { storage.m[0] = 42; return (); } }"

    , testCase "mem(map, key) with the wrong key type is rejected" $
        typeCheckFailure
          "contract C { storage: { m: map<int, bool> }; @entrypoint f(): bool { return mem(storage.m, true); } }"

    , testCase "remove(map, key) with the wrong key type is rejected" $
        typeCheckFailure
          "contract C { storage: { m: map<int, bool> }; @entrypoint f(): map<int, bool> { return remove(storage.m, true); } }"

    , testCase "empty_map without a map type context is rejected" $
        typeCheckFailure
          "contract C { storage: { x: int }; @originate init(): unit { var m: int = empty_map; return (); } }"

    , testCase "Indexing a non-map with [] is rejected" $
        typeCheckFailure
          "contract C { storage: { x: int }; @entrypoint f(): int { return storage.x[0]; } }"
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

codecTests :: TestTree
codecTests =
  testGroup
    "Codec: map<K, V> JSON"
    [ testCase "exprToJson encodes empty map as empty array" $
        exprToJson (MapVal (TMap TInt TBool) M.empty) @?= Array V.empty

    , testCase "exprToJson encodes map<int, bool> as array of {key, value}" $
        let m = MapVal (TMap TInt TBool) (M.fromList [(CInt TInt 1, CBool TBool True)])
         in exprToJson m @?= Array (V.singleton (object ["key" .= (1 :: Int), "value" .= True]))

    , testCase "exprToJson encodes map<bool, int> as array of {key, value}" $
        let m = MapVal (TMap TBool TInt) (M.fromList [(CBool TBool False, CInt TInt 9)])
         in exprToJson m @?= Array (V.singleton (object ["key" .= False, "value" .= (9 :: Int)]))

    , testCase "jsonToExprByType decodes [] into an empty map<int, bool>" $
        case jsonToExprByType (TMap TInt TBool) (Array V.empty) of
          Left err -> assertFailure err
          Right (MapVal _ m) -> assertEqual "empty map" M.empty m
          Right other -> assertFailure $ "Expected MapVal, got: " ++ show other

    , testCase "jsonToExprByType decodes [{key,value}, ...] into map<int, bool>" $
        let json = Array $ V.fromList
              [ object ["key" .= (1 :: Int), "value" .= True]
              , object ["key" .= (2 :: Int), "value" .= False]
              ]
         in case jsonToExprByType (TMap TInt TBool) json of
              Left err -> assertFailure err
              Right (MapVal _ m) ->
                assertEqual
                  "decoded entries"
                  (M.fromList [(CInt TInt 1, CBool TBool True), (CInt TInt 2, CBool TBool False)])
                  m
              Right other -> assertFailure $ "Expected MapVal, got: " ++ show other

    , testCase "jsonToExprByType rejects a map entry missing \"value\"" $
        let json = Array (V.singleton (object ["key" .= (1 :: Int)]))
         in case jsonToExprByType (TMap TInt TBool) json of
              Left _  -> return ()
              Right _ -> assertFailure "Expected decoding failure for missing \"value\" field"

    , testCase "jsonToExprByType rejects a non-array for a map type" $
        case jsonToExprByType (TMap TInt TBool) (object ["1" .= True]) of
          Left _  -> return ()
          Right _ -> assertFailure "Expected decoding failure: a map must be a JSON array"

    , testCase "Round-trip: map<int, bool> survives exprToJson . jsonToExprByType" $
        let original = M.fromList [(CInt TInt 1, CBool TBool True), (CInt TInt 2, CBool TBool False)]
            json = exprToJson (MapVal (TMap TInt TBool) original)
         in case jsonToExprByType (TMap TInt TBool) json of
              Left err -> assertFailure err
              Right (MapVal _ m) -> assertEqual "round-tripped map" original m
              Right other -> assertFailure $ "Expected MapVal, got: " ++ show other

    , testCase "Storage with map field decodes via contractInstanceFromStorageValue" $
        parseSuccess
          "contract C { storage: { admin_id: int, members: map<int, bool> }; @originate init(): unit { return (); } }"
          $ \c ->
            let storageJson =
                  object
                    [ "admin_id" .= (7 :: Int)
                    , "members" .=
                        Array (V.singleton (object ["key" .= (1 :: Int), "value" .= True]))
                    ]
             in case contractInstanceFromStorageValue c storageJson of
                  Left err -> assertFailure err
                  Right (ContractInstance _ st) -> case st of
                    Record _ [("admin_id", CInt _ 7), ("members", MapVal _ m)] ->
                      assertEqual
                        "members map"
                        (M.fromList [(CInt TInt 1, CBool TBool True)])
                        m
                    _ -> assertFailure $ "unexpected storage expr: " ++ show st
    ]

membershipSource :: String
membershipSource =
  "contract Membership {\n\
  \  storage: {\n\
  \    admin_id: int,\n\
  \    members: map<int, bool>\n\
  \  };\n\
  \\n\
  \  @originate\n\
  \  init(admin: int): unit {\n\
  \    storage.admin_id = admin;\n\
  \    storage.members = empty_map;\n\
  \    return ();\n\
  \  }\n\
  \\n\
  \  @entrypoint\n\
  \  addMember(new_user: int): unit {\n\
  \    storage.members[new_user] = true;\n\
  \    return ();\n\
  \  }\n\
  \\n\
  \  @entrypoint\n\
  \  removeMember(user: int): bool {\n\
  \    if (mem(storage.members, user)) {\n\
  \      storage.members = remove(storage.members, user);\n\
  \      return true;\n\
  \    }\n\
  \    return false;\n\
  \  }\n\
  \}"

membershipTypedContract :: TypedContract
membershipTypedContract =
  case parseContractFromString membershipSource of
    Left err -> error ("Membership.smartts failed to parse: " ++ show err)
    Right c -> case typeCheckContract c of
      Left err -> error ("Membership.smartts failed to type-check: " ++ err)
      Right tc -> tc

mapCounterSource :: String
mapCounterSource =
  "contract MapCounter {\n\
  \  storage: {\n\
  \    counts: map<int, int>\n\
  \  };\n\
  \\n\
  \  @originate\n\
  \  init(): unit {\n\
  \    storage.counts = empty_map;\n\
  \    return ();\n\
  \  }\n\
  \\n\
  \  @entrypoint\n\
  \  setEntry(key: int): unit {\n\
  \    storage.counts[key] = 1;\n\
  \    return ();\n\
  \  }\n\
  \\n\
  \  @entrypoint\n\
  \  getEntry(key: int): int {\n\
  \    return storage.counts[key];\n\
  \  }\n\
  \\n\
  \  @entrypoint\n\
  \  hasEntry(key: int): bool {\n\
  \    return mem(storage.counts, key);\n\
  \  }\n\
  \\n\
  \  @entrypoint\n\
  \  remEntry(key: int): unit {\n\
  \    storage.counts = remove(storage.counts, key);\n\
  \    return ();\n\
  \  }\n\
  \}"

mapCounterTypedContract :: TypedContract
mapCounterTypedContract =
  case parseContractFromString mapCounterSource of
    Left err -> error ("MapCounter failed to parse: " ++ show err)
    Right c -> case typeCheckContract c of
      Left err -> error ("MapCounter failed to type-check: " ++ err)
      Right tc -> tc

e2eTests :: TestTree
e2eTests =
  testGroup
    "End-to-end: Membership.smartts"
    [ testCase "originate initializes storage with admin_id and an empty members map" $
        case originateWithJsonArgs M.empty membershipTypedContract membershipSource (object ["admin" .= (1 :: Int)]) of
          Left err -> assertFailure err
          Right (_, repo) -> case M.toList repo of
            [(_, ContractInstance "Membership" (Record _ [("admin_id", CInt _ 1), ("members", MapVal _ m)]))] ->
              assertEqual "members starts empty" M.empty m
            other -> assertFailure $ "Unexpected repository contents: " ++ show other

    , testCase "addMember inserts the new user into storage.members" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty membershipTypedContract membershipSource (object ["admin" .= (1 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        case callEntrypointWithJsonArgs repo0 membershipTypedContract addr "addMember" membershipSource (object ["new_user" .= (42 :: Int)]) of
          Left err -> assertFailure err
          Right (_, repo1) -> case M.lookup addr repo1 of
            Just (ContractInstance _ (Record _ [("admin_id", _), ("members", MapVal _ m)])) ->
              assertEqual "members has the new user" (M.fromList [(CInt TInt 42, CBool TBool True)]) m
            other -> assertFailure $ "Unexpected instance: " ++ show other

    , testCase "removeMember(user) returns true and removes a present member" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty membershipTypedContract membershipSource (object ["admin" .= (1 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        repo1 <-
          case callEntrypointWithJsonArgs repo0 membershipTypedContract addr "addMember" membershipSource (object ["new_user" .= (42 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right (_, r) -> return r
        case callEntrypointWithJsonArgs repo1 membershipTypedContract addr "removeMember" membershipSource (object ["user" .= (42 :: Int)]) of
          Left err -> assertFailure err
          Right (ret, repo2) -> do
            assertEqual "removeMember returns true" (Just (CBool TBool True)) ret
            case M.lookup addr repo2 of
              Just (ContractInstance _ (Record _ [("admin_id", _), ("members", MapVal _ m)])) ->
                assertEqual "members is empty after removal" M.empty m
              other -> assertFailure $ "Unexpected instance: " ++ show other

    , testCase "removeMember(user) returns false for an absent member" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty membershipTypedContract membershipSource (object ["admin" .= (1 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        case callEntrypointWithJsonArgs repo0 membershipTypedContract addr "removeMember" membershipSource (object ["user" .= (99 :: Int)]) of
          Left err -> assertFailure err
          Right (ret, _) -> assertEqual "removeMember returns false" (Just (CBool TBool False)) ret

    , testCase "Full round-trip: originate, persist storage to JSON, reload, call entrypoint" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty membershipTypedContract membershipSource (object ["admin" .= (1 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        repo1 <-
          case callEntrypointWithJsonArgs repo0 membershipTypedContract addr "addMember" membershipSource (object ["new_user" .= (7 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right (_, r) -> return r
        ci0 <- case M.lookup addr repo1 of
          Nothing -> assertFailure "Missing contract instance after addMember" >> error "unreachable"
          Just c  -> return c
        let storageJson = exprToJson (instanceStorage ci0)
        reloadedStorage <-
          case contractInstanceFromStorageValue membershipTypedContract storageJson of
            Left err -> assertFailure err >> error "unreachable"
            Right ci -> return ci
        let repo2 = M.insert addr reloadedStorage repo1
        case callEntrypointWithJsonArgs repo2 membershipTypedContract addr "removeMember" membershipSource (object ["user" .= (7 :: Int)]) of
          Left err -> assertFailure err
          Right (ret, _) -> assertEqual "removeMember finds the reloaded member" (Just (CBool TBool True)) ret
    ]

e2eMapCounterTests :: TestTree
e2eMapCounterTests =
  testGroup
    "End-to-end: MapCounter contract exercising every map operation"
    [ testCase "originate initializes storage with an empty counts map" $
        case originateWithJsonArgs M.empty mapCounterTypedContract mapCounterSource (object []) of
          Left err -> assertFailure err
          Right (_, repo) -> case M.toList repo of
            [(_, ContractInstance "MapCounter" (Record _ [("counts", MapVal _ m)]))] ->
              assertEqual "counts starts empty" M.empty m
            other -> assertFailure $ "Unexpected repository contents: " ++ show other

    , testCase "setEntry inserts an entry into the map" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty mapCounterTypedContract mapCounterSource (object []) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        case callEntrypointWithJsonArgs repo0 mapCounterTypedContract addr "setEntry" mapCounterSource (object ["key" .= (7 :: Int)]) of
          Left err -> assertFailure err
          Right (_, repo1) -> case M.lookup addr repo1 of
            Just (ContractInstance _ (Record _ [("counts", MapVal _ m)])) ->
              assertEqual "counts tem a entrada inserida" (M.fromList [(CInt TInt 7, CInt TInt 1)]) m
            other -> assertFailure $ "Unexpected instance: " ++ show other

    , testCase "getEntry returns the value of an existing key" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty mapCounterTypedContract mapCounterSource (object []) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        repo1 <-
          case callEntrypointWithJsonArgs repo0 mapCounterTypedContract addr "setEntry" mapCounterSource (object ["key" .= (7 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right (_, r) -> return r
        case callEntrypointWithJsonArgs repo1 mapCounterTypedContract addr "getEntry" mapCounterSource (object ["key" .= (7 :: Int)]) of
          Left err -> assertFailure err
          Right (ret, _) -> assertEqual "getEntry devolve 1" (Just (CInt TInt 1)) ret

    , testCase "hasEntry returns true for an existing key" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty mapCounterTypedContract mapCounterSource (object []) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        repo1 <-
          case callEntrypointWithJsonArgs repo0 mapCounterTypedContract addr "setEntry" mapCounterSource (object ["key" .= (5 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right (_, r) -> return r
        case callEntrypointWithJsonArgs repo1 mapCounterTypedContract addr "hasEntry" mapCounterSource (object ["key" .= (5 :: Int)]) of
          Left err -> assertFailure err
          Right (ret, _) -> assertEqual "hasEntry devolve true para chave presente" (Just (CBool TBool True)) ret

    , testCase "hasEntry returns false for a missing key" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty mapCounterTypedContract mapCounterSource (object []) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        case callEntrypointWithJsonArgs repo0 mapCounterTypedContract addr "hasEntry" mapCounterSource (object ["key" .= (99 :: Int)]) of
          Left err -> assertFailure err
          Right (ret, _) -> assertEqual "hasEntry returns false for a missing key" (Just (CBool TBool False)) ret

    , testCase "remEntry removes a key from the map" $ do
        (addr, repo0) <-
          case originateWithJsonArgs M.empty mapCounterTypedContract mapCounterSource (object []) of
            Left err -> assertFailure err >> error "unreachable"
            Right ok -> return ok
        repo1 <-
          case callEntrypointWithJsonArgs repo0 mapCounterTypedContract addr "setEntry" mapCounterSource (object ["key" .= (3 :: Int)]) of
            Left err -> assertFailure err >> error "unreachable"
            Right (_, r) -> return r
        case callEntrypointWithJsonArgs repo1 mapCounterTypedContract addr "remEntry" mapCounterSource (object ["key" .= (3 :: Int)]) of
          Left err -> assertFailure err
          Right (_, repo2) -> case M.lookup addr repo2 of
            Just (ContractInstance _ (Record _ [("counts", MapVal _ m)])) ->
              assertEqual "counts is empty after remEntry" M.empty m
            other -> assertFailure $ "Unexpected instance after remEntry: " ++ show other
    ]
