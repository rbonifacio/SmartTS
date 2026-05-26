-- | Bidirectional type checking for SmartTS (annotated locals, parameters, returns).
-- Designed to grow: judgments live here; surface 'Type' stays in "AST" until we add variables/schemes.
module SmartTS.TypeCheck
  ( typeCheckContract
  ) where

import Control.Monad (foldM, unless, void)
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
  { envStorageType  :: Type
  , envBindings     :: M.Map Name TcBinding
  , envReturnType   :: Type
  , envEnumRegistry :: M.Map Name [Name]
  , envVariantMap   :: M.Map Name Name
  }
  deriving (Eq, Show)

typeCheckContract :: Contract -> Either String ()
typeCheckContract c = do
  checkDuplicateStorage (contractStorage c)
  checkDuplicateEnumNames (contractEnums c)
  checkDuplicateVariants (contractEnums c)
  mapM_ (checkDuplicateParams . methodArgs) (contractMethods c)
  mapM_ (checkMethod c) (contractMethods c)

checkDuplicateEnumNames :: [EnumDecl] -> Either String ()
checkDuplicateEnumNames decls =
  let names = [n | EnumDecl n _ <- decls]
  in if length names == length (nub names)
       then Right ()
       else Left "Duplicate enum name in contract."

checkDuplicateVariants :: [EnumDecl] -> Either String ()
checkDuplicateVariants decls =
  let allVs = concatMap (\(EnumDecl _ vs) -> vs) decls
  in if length allVs == length (nub allVs)
       then Right ()
       else Left "Duplicate enum variant name (variants must be globally unique within a contract)."

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
      paramMap = M.fromList
        [ (n, TcBinding Param t)
        | FormalParameter n t <- methodArgs m
        ]
      env0 = TcEnv
        { envStorageType  = storageT
        , envBindings     = paramMap
        , envReturnType   = methodReturnType m
        , envEnumRegistry = buildEnumRegistry (contractEnums c)
        , envVariantMap   = buildVariantMap   (contractEnums c)
        }
  in void (checkStmt env0 (methodBody m))

buildEnumRegistry :: [EnumDecl] -> M.Map Name [Name]
buildEnumRegistry decls = M.fromList [(n, vs) | EnumDecl n vs <- decls]

buildVariantMap :: [EnumDecl] -> M.Map Name Name
buildVariantMap decls = M.fromList [(v, n) | EnumDecl n vs <- decls, v <- vs]

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
checkStmt env (VarDestructStmt n1 n2 t e) = do
  te <- inferExpr env e
  expectType ("initializer of var (" ++ n1 ++ ", " ++ n2 ++ ")") te t
  case t of
    TPair t1 t2 -> do
      noDuplicateLocal n1 env
      noDuplicateLocal n2 env
      return $ insertLocal n2 LocalMutable t2
             $ insertLocal n1 LocalMutable t1 env
    _ -> Left "Destructuring declaration requires a pair<T,U> type annotation."
checkStmt env (ValDestructStmt n1 n2 t e) = do
  te <- inferExpr env e
  expectType ("initializer of val (" ++ n1 ++ ", " ++ n2 ++ ")") te t
  case t of
    TPair t1 t2 -> do
      noDuplicateLocal n1 env
      noDuplicateLocal n2 env
      return $ insertLocal n2 LocalImmutable t2
             $ insertLocal n1 LocalImmutable t1 env
    _ -> Left "Destructuring declaration requires a pair<T,U> type annotation."
checkStmt env (MatchStmt e arms) = do
  te <- inferExpr env e
  case te of
    TEnum enumName ->
      case M.lookup enumName (envEnumRegistry env) of
        Nothing -> Left $ "Unknown enum type `" ++ enumName ++ "` in match."
        Just variants -> do
          let armNames = map fst arms
          let missing  = filter (`notElem` armNames) variants
          let extra    = filter (`notElem` variants) armNames
          unless (null missing) $
            Left $ "Non-exhaustive match on enum `" ++ enumName
                ++ "`: missing " ++ show missing ++ "."
          unless (null extra) $
            Left $ "Unknown variants in match on enum `" ++ enumName
                ++ "`: " ++ show extra ++ "."
          unless (length armNames == length (nub armNames)) $
            Left "Duplicate arm in match statement."
          mapM_ (\(_, body) -> void (checkStmt env body)) arms
          return env
    _ -> Left "match expression must have an enum type."

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

-- | @storage@ is always assignable; locals must be mutable. Parameters and @val@ are not.
checkAssignable :: TcEnv -> LValue -> Either String ()
checkAssignable env lv =
  case rootOf lv of
    LStorage -> Right ()
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
inferExpr env (PairExpr e1 e2) = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  return (TPair t1 t2)
inferExpr env (Fst e) = do
  t <- inferExpr env e
  case t of
    TPair t1 _ -> Right t1
    _ -> Left "fst requires a pair-typed expression."
inferExpr env (Snd e) = do
  t <- inferExpr env e
  case t of
    TPair _ t2 -> Right t2
    _ -> Left "snd requires a pair-typed expression."
inferExpr env (EnumLiteral n) =
  case M.lookup n (envVariantMap env) of
    Nothing       -> Left $ "Unknown enum variant `" ++ n ++ "`."
    Just enumName -> Right (TEnum enumName)

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
typesEqual (TPair a1 b1) (TPair a2 b2) = typesEqual a1 a2 && typesEqual b1 b2
typesEqual (TEnum n1) (TEnum n2) = n1 == n2
typesEqual _ _ = False

prettyType :: Type -> String
prettyType TInt = "int"
prettyType TBool = "bool"
prettyType TUnit = "unit"
prettyType (TPair t u) = "pair<" ++ prettyType t ++ ", " ++ prettyType u ++ ">"
prettyType (TEnum n) = n
prettyType (TRecord fs) =
  "{"
    ++ concat
      [ n ++ ": " ++ prettyType t ++ if i < lastI then ", " else ""
      | (i, (n, t)) <- zip [0 :: Int ..] fs
      , let lastI = length fs - 1
      ]
    ++ "}"
