-- | Bidirectional type checking for SmartTS (annotated locals, parameters, returns).
-- Designed to grow: judgments live here; surface 'Type' stays in "AST" until we add variables/schemes.
module SmartTS.TypeCheck
  ( typeCheckContract
  ) where

import Control.Monad (foldM, void)
import Data.List (nub)
import qualified Data.Map.Strict as M
import SmartTS.AST

data BindingKind = Param | LocalMutable | LocalImmutable
  deriving (Eq, Show)

data TcBinding = TcBinding
  { bindingKind :: BindingKind
  , bindingType :: Type
  }
  deriving (Eq, Show)

-- | Environment for checking one method body.
data TcEnv = TcEnv
  { envStorageType :: Type
  , envBindings :: M.Map Name TcBinding
  , envReturnType :: Type
  , envIsView :: Bool
  }
  deriving (Eq, Show)

typeCheckContract :: Contract -> Either String ()
typeCheckContract c = do
  checkDuplicateStorage (contractStorage c)
  mapM_ (checkDuplicateParams . methodArgs) (contractMethods c)
  mapM_ (checkMethod c) (contractMethods c)

checkDuplicateStorage :: Storage -> Either String ()
checkDuplicateStorage fields =
  let names = map fst fields
   in if length names == length (nub names)
        then Right ()
        else Left "Duplicate field name in contract storage."

checkDuplicateParams :: [FormalParameter] -> Either String ()
checkDuplicateParams params =
  let names = map (\(FormalParameter n _) -> n) params
   in if length names == length (nub names)
        then Right ()
        else Left "Duplicate parameter name in method."

checkMethod :: Contract -> MethodDecl -> Either String ()
checkMethod c m =
  let storageT = TRecord (contractStorage c)
      paramMap =
        M.fromList
          [ (n, TcBinding Param t)
          | FormalParameter n t <- methodArgs m
          ]
      env0 =
        TcEnv
          { envStorageType = storageT
          , envBindings = paramMap
          , envReturnType = methodReturnType m
          , envIsView = isViewMethod m
          }
   in void (checkStmt env0 (methodBody m))

-- | Check a statement; returns updated environment (bindings from @var@/@val@).
checkStmt :: TcEnv -> Stmt -> Either String TcEnv
checkStmt env (SequenceStmt ss) = foldM checkStmt env ss
checkStmt env (ReturnStmt e) = do
  t <- inferExpr env e
  expectType "return value" t (envReturnType env)
  return env
checkStmt env (VarDeclStmt n typ e) = do
  noDuplicateLocal n env
  t <- inferExpr env e
  expectType ("initializer of var `" ++ n ++ "`") t typ
  return $ insertLocal n LocalMutable typ env
checkStmt env (ValDeclStmt n typ e) = do
  noDuplicateLocal n env
  t <- inferExpr env e
  expectType ("initializer of val `" ++ n ++ "`") t typ
  return $ insertLocal n LocalImmutable typ env
checkStmt env (AssignmentStmt lv e) = do
  checkAssignable env lv
  tl <- typeOfLValue env lv
  te <- inferExpr env e
  expectType "assignment" te tl
  return env
checkStmt env (IfStmt cond thn mel) = do
  tc <- inferExpr env cond
  expectType "if condition" tc TBool
  void (checkStmt env thn)
  case mel of
    Nothing -> return ()
    Just els -> void (checkStmt env els)
  return env
checkStmt env (WhileStmt cond body) = do
  tc <- inferExpr env cond
  expectType "while condition" tc TBool
  void (checkStmt env body)
  return env

noDuplicateLocal :: Name -> TcEnv -> Either String ()
noDuplicateLocal n env =
  case M.lookup n (envBindings env) of
    Just (TcBinding LocalMutable _) ->
      Left $ "Duplicate local `" ++ n ++ "` in the same block."
    Just (TcBinding LocalImmutable _) ->
      Left $ "Duplicate local `" ++ n ++ "` in the same block."
    _ -> Right ()

insertLocal :: Name -> BindingKind -> Type -> TcEnv -> TcEnv
insertLocal n k t env =
  env {envBindings = M.insert n (TcBinding k t) (envBindings env)}

-- | In @view methods, storage is read-only.
-- Locals must still respect mutability: parameters and @val@ are not assignable.
checkAssignable :: TcEnv -> LValue -> Either String ()
checkAssignable env lv =
  case rootOf lv of
    LStorage ->
      if envIsView env
        then Left "@view method cannot modify storage."
        else Right ()
    LVar n ->
      case M.lookup n (envBindings env) of
        Nothing -> Left $ "Unknown assignment target: `" ++ n ++ "`."
        Just (TcBinding Param _) ->
          Left $ "Cannot assign to method parameter `" ++ n ++ "` (or through it for field updates)."
        Just (TcBinding LocalImmutable _) ->
          Left $ "Cannot assign to immutable val `" ++ n ++ "` (or through it for field updates)."
        Just (TcBinding LocalMutable _) -> Right ()
    LField {} -> Right ()

rootOf :: LValue -> LValue
rootOf LStorage = LStorage
rootOf (LVar n) = LVar n
rootOf (LField p _) = rootOf p

typeOfLValue :: TcEnv -> LValue -> Either String Type
typeOfLValue env LStorage = pure (envStorageType env)
typeOfLValue env (LVar n) =
  case M.lookup n (envBindings env) of
    Nothing -> Left $ "Unknown variable `" ++ n ++ "`."
    Just b -> Right (bindingType b)
typeOfLValue env (LField root fld) = do
  tRoot <- typeOfLValue env root
  case tRoot of
    TRecord fields ->
      case lookup fld fields of
        Nothing -> Left $ "Record has no field `" ++ fld ++ "`."
        Just t -> Right t
    _ -> Left "Field access requires a record value (or typed storage)."

inferExpr :: TcEnv -> Expr -> Either String Type
inferExpr _ (CInt _) = Right TInt
inferExpr _ (CBool _) = Right TBool
inferExpr _ Unit = Right TUnit
inferExpr env StorageExpr = pure (envStorageType env)
inferExpr env (Var n) =
  case M.lookup n (envBindings env) of
    Nothing -> Left $ "Unknown variable `" ++ n ++ "`."
    Just b -> Right (bindingType b)
inferExpr env (FieldAccess e fld) = do
  t <- inferExpr env e
  case t of
    TRecord fields ->
      case lookup fld fields of
        Nothing -> Left $ "Record has no field `" ++ fld ++ "`."
        Just ft -> Right ft
    _ -> Left "Field access requires a record-typed expression."
inferExpr env (Not e) = do
  t <- inferExpr env e
  expectType "operand of !" t TBool
  return TBool
inferExpr env (And a b) = inferBoolBin env a b
inferExpr env (Or a b) = inferBoolBin env a b
inferExpr env (Add a b) = inferIntBin env a b
inferExpr env (Sub a b) = inferIntBin env a b
inferExpr env (Mul a b) = inferIntBin env a b
inferExpr env (Div a b) = inferIntBin env a b
inferExpr env (Mod a b) = inferIntBin env a b
inferExpr env (Eq a b) = inferEq env a b
inferExpr env (Neq a b) = inferEq env a b
inferExpr env (Lt a b) = inferIntCmp env a b
inferExpr env (Lte a b) = inferIntCmp env a b
inferExpr env (Gt a b) = inferIntCmp env a b
inferExpr env (Gte a b) = inferIntCmp env a b
inferExpr env (Record pairs) = do
  ts <- mapM (\(k, e) -> (,) k <$> inferExpr env e) pairs
  Right (TRecord [(k, t) | (k, t) <- ts])

inferBoolBin :: TcEnv -> Expr -> Expr -> Either String Type
inferBoolBin env a b = do
  ta <- inferExpr env a
  tb <- inferExpr env b
  expectType "left operand of boolean operator" ta TBool
  expectType "right operand of boolean operator" tb TBool
  return TBool

inferIntBin :: TcEnv -> Expr -> Expr -> Either String Type
inferIntBin env a b = do
  ta <- inferExpr env a
  tb <- inferExpr env b
  expectType "left operand of arithmetic operator" ta TInt
  expectType "right operand of arithmetic operator" tb TInt
  return TInt

inferIntCmp :: TcEnv -> Expr -> Expr -> Either String Type
inferIntCmp env a b = do
  ta <- inferExpr env a
  tb <- inferExpr env b
  expectType "left operand of comparison" ta TInt
  expectType "right operand of comparison" tb TInt
  return TBool

inferEq :: TcEnv -> Expr -> Expr -> Either String Type
inferEq env a b = do
  ta <- inferExpr env a
  tb <- inferExpr env b
  if typesEqual ta tb
    then Right TBool
    else
      Left $
        "Equality requires operands of the same type (got "
          ++ prettyType ta
          ++ " and "
          ++ prettyType tb
          ++ ")."

expectType :: String -> Type -> Type -> Either String ()
expectType ctx got expected =
  if typesEqual got expected
    then Right ()
    else
      Left $
        ctx ++ " has wrong type: expected " ++ prettyType expected ++ ", inferred " ++ prettyType got ++ "."

typesEqual :: Type -> Type -> Bool
typesEqual TInt TInt = True
typesEqual TBool TBool = True
typesEqual TUnit TUnit = True
typesEqual (TRecord as) (TRecord bs) = length as == length bs && and (zipWith fieldEq as bs)
  where
    fieldEq (n1, t1) (n2, t2) = n1 == n2 && typesEqual t1 t2
typesEqual _ _ = False

prettyType :: Type -> String
prettyType TInt = "int"
prettyType TBool = "bool"
prettyType TUnit = "unit"
prettyType (TRecord fs) =
  "{"
    ++ concat
      [ n ++ ": " ++ prettyType t ++ if i < lastI then ", " else ""
      | (i, (n, t)) <- zip [0 :: Int ..] fs
      , let lastI = length fs - 1
      ]
    ++ "}"