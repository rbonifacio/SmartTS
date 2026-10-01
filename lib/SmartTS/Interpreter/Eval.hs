-- | Expression evaluation and statement execution.
-- Both live in the same module so that the 'Call' expression can invoke 'execStmt'
-- without creating a circular import with "SmartTS.Interpreter.Contract".
module SmartTS.Interpreter.Eval where

import Control.Monad.State
import qualified Data.Map.Strict as M
import SmartTS.IR.AST
import SmartTS.Interpreter.Runtime

-- ---------------------------------------------------------------------------
-- Expression evaluation
-- ---------------------------------------------------------------------------

evalExpr :: TypedExpr -> EvalM TypedExpr
evalExpr e@(CInt _ _)  = return e
evalExpr e@(CBool _ _) = return e
evalExpr e@(Unit _)    = return e
evalExpr (StorageExpr _) = do
  rt <- get
  case rtStorage rt of
    Nothing -> return (Unit TUnit)
    Just s  -> return s
evalExpr (Var _ n) = do
  rt <- get
  case M.lookup n (rtLocals rt) of
    Just b  -> return (bindingValue b)
    Nothing ->
      case M.lookup n (rtParams rt) of
        Just v  -> return v
        Nothing -> interpretBug ("unknown variable `" ++ n ++ "` after type check")
evalExpr (Record ty fields) = do
  fs <- mapM (\(k, e) -> (,) k <$> evalExpr e) fields
  return (Record ty fs)
evalExpr (FieldAccess _ base fld) = do
  b <- evalExpr base
  case b of
    Record _ fs ->
      case lookup fld fs of
        Just v  -> return v
        Nothing -> interpretBug ("missing record field `" ++ fld ++ "` after type check")
    _ -> interpretBug "field access on non-record after type check"
evalExpr (Not _ e) = do
  v <- evalExpr e
  case v of
    CBool _ b -> return (CBool TBool (not b))
    _         -> interpretBug "operand of ! was not bool after type check"
evalExpr (And _ a b) = boolBin a b (&&)
evalExpr (Or  _ a b) = boolBin a b (||)
evalExpr (Add _ a b) = intBin a b (+)
evalExpr (Sub _ a b) = intBin a b (-)
evalExpr (Mul _ a b) = intBin a b (*)
evalExpr (Div _ a b) = do
  x <- evalInt a
  y <- evalInt b
  if y == 0 then lift (Left "Division by zero.") else return (CInt TInt (x `div` y))
evalExpr (Mod _ a b) = do
  x <- evalInt a
  y <- evalInt b
  if y == 0 then lift (Left "Modulo by zero.") else return (CInt TInt (x `mod` y))
evalExpr (Eq  _ a b) = CBool TBool <$> ((==) <$> evalExpr a <*> evalExpr b)
evalExpr (Neq _ a b) = CBool TBool <$> ((/=) <$> evalExpr a <*> evalExpr b)
evalExpr (Lt  _ a b) = intCmp a b (<)
evalExpr (Lte _ a b) = intCmp a b (<=)
evalExpr (Gt  _ a b) = intCmp a b (>)
evalExpr (Gte _ a b) = intCmp a b (>=)
evalExpr (Call _ name args) = do
  rt <- get
  m <- case M.lookup name (rtMethods rt) of
    Nothing  -> interpretBug ("unknown method `" ++ name ++ "` after type check")
    Just m'  -> return m'
  argVals <- mapM evalExpr args
  outerRt <- get
  let paramNames = [n | FormalParameter n _ <- methodArgs m]
      params     = M.fromList (zip paramNames argVals)
      innerRt    = outerRt {rtParams = params, rtLocals = M.empty}
  (mRet, innerRt') <- lift $ runStateT (execStmt (methodBody m)) innerRt
  -- Propagate storage mutations from the called method back to the caller.
  modify $ \r -> r {rtStorage = rtStorage innerRt'}
  case mRet of
    Nothing -> interpretBug ("method `" ++ name ++ "` did not return a value after type check")
    Just v  -> return v

-- ---------------------------------------------------------------------------
-- Statement execution
-- ---------------------------------------------------------------------------

execStmt :: TypedStmt -> EvalM (Maybe TypedExpr)
execStmt (SequenceStmt ss) = execSequence ss
execStmt (ReturnStmt e) = Just <$> evalExpr e
execStmt (VarDeclStmt n _ e) = do
  v <- evalExpr e
  modify $ \rt -> rt {rtLocals = M.insert n (Binding True v) (rtLocals rt)}
  return Nothing
execStmt (ValDeclStmt n _ e) = do
  v <- evalExpr e
  modify $ \rt -> rt {rtLocals = M.insert n (Binding False v) (rtLocals rt)}
  return Nothing
execStmt (AssignmentStmt lv e) = do
  v <- evalExpr e
  assignLValue lv v
  return Nothing
execStmt (IfStmt cond thenS elseS) = do
  c <- evalExpr cond
  case c of
    CBool _ True  -> execStmt thenS
    CBool _ False ->
      case elseS of
        Nothing -> return Nothing
        Just es -> execStmt es
    _ -> interpretBug "if condition was not bool after type check"
execStmt (ForStmt initS cond update body) = do
  _ <- execStmt initS
  loop
  where
    loop = do
      c <- evalExpr cond
      case c of
        CBool _ False -> return Nothing
        CBool _ True -> do
          ret <- execStmt body
          case ret of
            Just v  -> return (Just v)
            Nothing -> execStmt update >> loop
        _ -> interpretBug "for condition was not bool after type check"
execStmt (WhileStmt cond body) = loop
  where
    loop = do
      c <- evalExpr cond
      case c of
        CBool _ False -> return Nothing
        CBool _ True  -> do
          ret <- execStmt body
          case ret of
            Just v  -> return (Just v)
            Nothing -> loop
        _ -> interpretBug "while condition was not bool after type check"

execSequence :: [TypedStmt] -> EvalM (Maybe TypedExpr)
execSequence [] = return Nothing
execSequence (s : ss) = do
  ret <- execStmt s
  case ret of
    Just v  -> return (Just v)
    Nothing -> execSequence ss

-- ---------------------------------------------------------------------------
-- LValue assignment helpers
-- ---------------------------------------------------------------------------

assignLValue :: LValue -> TypedExpr -> EvalM ()
assignLValue LStorage v =
  modify $ \rt -> rt {rtStorage = Just v}
assignLValue (LVar n) v = do
  rt <- get
  case M.lookup n (rtLocals rt) of
    Just b ->
      if bindingMutable b
        then modify $ \r -> r {rtLocals = M.insert n (Binding True v) (rtLocals r)}
        else interpretBug ("assignment to immutable val `" ++ n ++ "` after type check")
    Nothing ->
      if M.member n (rtParams rt)
        then interpretBug ("assignment to parameter `" ++ n ++ "` after type check")
        else interpretBug ("unknown assignment target `" ++ n ++ "` after type check")
assignLValue (LField lv fld) v = do
  rt <- get
  let (root, path) = flattenLValue lv [fld]
  rootExpr <- lift $ resolveRootExpr rt root
  updated  <- lift $ setFieldPath rootExpr path v
  assignLValue root updated

flattenLValue :: LValue -> [Name] -> (LValue, [Name])
flattenLValue LStorage      acc = (LStorage, acc)
flattenLValue (LVar n)      acc = (LVar n, acc)
flattenLValue (LField p fld) acc = flattenLValue p (fld : acc)

resolveRootExpr :: Runtime -> LValue -> Either String TypedExpr
resolveRootExpr rt LStorage =
  case rtStorage rt of
    Nothing -> Right (Record (TRecord []) [])
    Just s  -> Right s
resolveRootExpr rt (LVar n) =
  case M.lookup n (rtLocals rt) of
    Just b  -> Right (bindingValue b)
    Nothing ->
      case M.lookup n (rtParams rt) of
        Just _  -> interpretBug ("field update through parameter `" ++ n ++ "` after type check")
        Nothing -> interpretBug ("unknown root for field update `" ++ n ++ "` after type check")
resolveRootExpr _ _ = interpretBug "invalid root for field update"

setFieldPath :: TypedExpr -> [Name] -> TypedExpr -> Either String TypedExpr
setFieldPath _ [] _          = interpretBug "empty field path in assignment"
setFieldPath base [f] v      = setField base f v
setFieldPath base (f : fs) v = do
  child  <- getOrCreateField base f
  child' <- setFieldPath child fs v
  setField base f child'

getOrCreateField :: TypedExpr -> Name -> Either String TypedExpr
getOrCreateField (Record _ fields) f =
  case lookup f fields of
    Just v  -> Right v
    Nothing -> Right (Record (TRecord []) [])
getOrCreateField (Unit _) _ = Right (Record (TRecord []) [])
getOrCreateField _ _ = interpretBug "field path through non-record value after type check"

setField :: TypedExpr -> Name -> TypedExpr -> Either String TypedExpr
setField (Record ty fields) f v = Right (Record ty (insertOrReplace f v fields))
setField (Unit _) f v           = Right (Record (TRecord [(f, exprAnn v)]) [(f, v)])
setField _ _ _                  = interpretBug "setField on non-record after type check"

insertOrReplace :: Name -> TypedExpr -> [(Name, TypedExpr)] -> [(Name, TypedExpr)]
insertOrReplace k v [] = [(k, v)]
insertOrReplace k v ((k0, v0) : rest)
  | k == k0   = (k, v) : rest
  | otherwise = (k0, v0) : insertOrReplace k v rest

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

evalInt :: TypedExpr -> EvalM Int
evalInt e = do
  v <- evalExpr e
  case v of
    CInt _ n -> return n
    _        -> interpretBug "expected int subexpression after type check"

intBin :: TypedExpr -> TypedExpr -> (Int -> Int -> Int) -> EvalM TypedExpr
intBin a b op = CInt TInt <$> (op <$> evalInt a <*> evalInt b)

boolBin :: TypedExpr -> TypedExpr -> (Bool -> Bool -> Bool) -> EvalM TypedExpr
boolBin a b op = do
  x <- evalExpr a
  y <- evalExpr b
  case (x, y) of
    (CBool _ bx, CBool _ by) -> return (CBool TBool (op bx by))
    _                        -> interpretBug "boolean operator on non-bool after type check"

intCmp :: TypedExpr -> TypedExpr -> (Int -> Int -> Bool) -> EvalM TypedExpr
intCmp a b op = CBool TBool <$> (op <$> evalInt a <*> evalInt b)
