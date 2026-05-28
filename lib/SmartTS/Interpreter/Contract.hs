-- | Statement execution and contract origination/invocation.
--
-- The CLI runs "SmartTS.TypeCheck.typeCheckContract" on source before interpretation.
-- Persisted storage is decoded with "SmartTS.Interpreter.Codec.jsonToExprByType".
-- After those steps, expression shapes that contradict the static types are treated as
-- internal bugs ("interpretBug") rather than user-facing "Left" errors.
module SmartTS.Interpreter.Contract where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as M
import SmartTS.AST
import SmartTS.Interpreter.Codec
import SmartTS.Interpreter.Eval
import SmartTS.Interpreter.Runtime

execStmt :: Runtime -> Stmt -> Either String (Maybe Expr, Runtime)
execStmt rt (SequenceStmt ss) = execSequence rt ss
execStmt rt (ReturnStmt e) = do
  v <- evalExpr rt e
  Right (Just v, rt)
execStmt rt (VarDeclStmt n _ e) = do
  v <- evalExpr rt e
  let locals' = M.insert n (Binding True v) (rtLocals rt)
  Right (Nothing, rt {rtLocals = locals'})
execStmt rt (ValDeclStmt n _ e) = do
  v <- evalExpr rt e
  let locals' = M.insert n (Binding False v) (rtLocals rt)
  Right (Nothing, rt {rtLocals = locals'})
execStmt rt (AssignmentStmt lv e) = do
  v <- evalExpr rt e
  rt' <- assignLValue rt lv v
  Right (Nothing, rt')
execStmt rt (IfStmt cond thenS elseS) = do
  c <- evalExpr rt cond
  case c of
    CBool True -> execStmt rt thenS
    CBool False ->
      case elseS of
        Nothing -> Right (Nothing, rt)
        Just es -> execStmt rt es
    _ -> interpretBug "if condition was not bool after type check"
execStmt rt (WhileStmt cond body) = loop rt
  where
    loop cur = do
      c <- evalExpr cur cond
      case c of
        CBool False -> Right (Nothing, cur)
        CBool True -> do
          (ret, next) <- execStmt cur body
          case ret of
            Just v -> Right (Just v, next)
            Nothing -> loop next
        _ -> interpretBug "while condition was not bool after type check"

execSequence :: Runtime -> [Stmt] -> Either String (Maybe Expr, Runtime)
execSequence rt [] = Right (Nothing, rt)
execSequence rt (s : ss) = do
  (ret, rt') <- execStmt rt s
  case ret of
    Just v -> Right (Just v, rt')
    Nothing -> execSequence rt' ss

assignLValue :: Runtime -> LValue -> Expr -> Either String Runtime
assignLValue rt LStorage v = Right rt {rtStorage = Just v}
assignLValue rt (LVar n) v =
  case M.lookup n (rtLocals rt) of
    Just b ->
      if bindingMutable b
        then
          Right
            rt
              { rtLocals =
                  M.insert n (Binding True v) (rtLocals rt)
              }
        else interpretBug ("assignment to immutable val `" ++ n ++ "` after type check")
    Nothing ->
      if M.member n (rtParams rt)
        then interpretBug ("assignment to parameter `" ++ n ++ "` after type check")
        else interpretBug ("unknown assignment target `" ++ n ++ "` after type check")
assignLValue rt (LField lv fld) v = do
  (root, path) <- flattenLValue lv [fld]
  rootExpr <- resolveRootExpr rt root
  updated <- setFieldPath rootExpr path v
  assignLValue rt root updated

flattenLValue :: LValue -> [Name] -> Either String (LValue, [Name])
flattenLValue LStorage acc = Right (LStorage, acc)
flattenLValue (LVar n) acc = Right (LVar n, acc)
flattenLValue (LField parent fld) acc = flattenLValue parent (fld : acc)

resolveRootExpr :: Runtime -> LValue -> Either String Expr
resolveRootExpr rt LStorage =
  case rtStorage rt of
    Nothing -> Right (Record [])
    Just s -> Right s
resolveRootExpr rt (LVar n) =
  case M.lookup n (rtLocals rt) of
    Just b -> Right (bindingValue b)
    Nothing ->
      case M.lookup n (rtParams rt) of
        Just _ -> interpretBug ("field update through parameter `" ++ n ++ "` after type check")
        Nothing -> interpretBug ("unknown root for field update `" ++ n ++ "` after type check")
resolveRootExpr _ _ = interpretBug "invalid root for field update"

setFieldPath :: Expr -> [Name] -> Expr -> Either String Expr
setFieldPath _ [] _ = interpretBug "empty field path in assignment"
setFieldPath base [f] v = setField base f v
setFieldPath base (f : fs) v = do
  child <- getOrCreateField base f
  child' <- setFieldPath child fs v
  setField base f child'

getOrCreateField :: Expr -> Name -> Either String Expr
getOrCreateField (Record fields) f =
  case lookup f fields of
    Just v -> Right v
    Nothing -> Right (Record [])
getOrCreateField Unit _ = Right (Record [])
getOrCreateField _ _ = interpretBug "field path through non-record value after type check"

setField :: Expr -> Name -> Expr -> Either String Expr
setField (Record fields) f v = Right (Record (insertOrReplace f v fields))
setField Unit f v = Right (Record [(f, v)])
setField _ _ _ = interpretBug "setField on non-record after type check"

insertOrReplace :: Name -> Expr -> [(Name, Expr)] -> [(Name, Expr)]
insertOrReplace k v [] = [(k, v)]
insertOrReplace k v ((k0, v0) : rest)
  | k == k0 = (k, v) : rest
  | otherwise = (k0, v0) : insertOrReplace k v rest

execMethod :: MethodDecl -> M.Map Name Expr -> Either String Runtime
execMethod m params = do
  let initialRt =
        Runtime
          { rtStorage = Nothing
          , rtParams = params
          , rtLocals = M.empty
          }
  (_, rt') <- execStmt initialRt (methodBody m)
  Right rt'

-- | Run a method with persisted storage (for @entrypoint calls).
execMethodWithInitialStorage ::
  Expr ->
  MethodDecl ->
  M.Map Name Expr ->
  Either String (Maybe Expr, Runtime)
execMethodWithInitialStorage initialStorage m params = do
  let initialRt =
        Runtime
          { rtStorage = Just initialStorage
          , rtParams = params
          , rtLocals = M.empty
          }
  execStmt initialRt (methodBody m)

findEntryPointByName :: Contract -> Name -> Either String MethodDecl
findEntryPointByName c name =
  case filter (\m -> isEntryPointMethod m && methodName m == name) (contractMethods c) of
    [] -> Left $ "No @entrypoint named \"" ++ name ++ "\"."
    [m] -> Right m
    _ -> Left $ "Multiple @entrypoint methods named \"" ++ name ++ "\"."

originateWithJsonArgs ::
  RepositoryState ->
  Contract ->
  String ->
  Value ->
  Either String (Address, RepositoryState)
originateWithJsonArgs repo c sourceText argsJson = do
  m <- case findMethods Originate c of
    [] -> Left "Contract must have exactly one @originate method."
    [mm] -> Right mm
    _ -> Left "Contract must have exactly one @originate method."
  params <- bindArgsByName (methodArgs m) argsJson
  rt <- execMethod m params
  storageExpr <-
    case rtStorage rt of
      Nothing -> Left "Originate method did not initialize `storage`."
      Just s -> Right s
  let address = generateAddress sourceText (M.size repo)
      repo' =
        M.insert
          address
          (ContractInstance (contractName c) storageExpr)
          repo
  Right (address, repo')

-- | Execute an @entrypoint with JSON args; persist updated storage into the repository map.
-- @sourceText@ must be the exact .smartts file contents so it can be checked against the hash embedded in @addr@ (when present).
callEntrypointWithJsonArgs ::
  RepositoryState ->
  Contract ->
  Address ->
  Name ->
  String ->
  Value ->
  Either String (Maybe Expr, RepositoryState)
callEntrypointWithJsonArgs repo c addr entryName sourceText argsJson = do
  ci <-
    case M.lookup addr repo of
      Nothing -> Left $ "Unknown address: " ++ addr
      Just x -> Right x
  assertAddressMatchesSource addr sourceText
  if instanceContractName ci /= contractName c
    then
      Left $
        "Contract name mismatch: instance is "
          ++ instanceContractName ci
          ++ " but loaded source is "
          ++ contractName c
          ++ "."
    else do
      m <- findEntryPointByName c entryName
      params <- bindArgsByName (methodArgs m) argsJson
      (ret, rt') <- execMethodWithInitialStorage (instanceStorage ci) m params
      newStorage <-
        case rtStorage rt' of
          Nothing -> Left "Entrypoint cleared `storage`; not allowed."
          Just s -> Right s
      let ci' = ci {instanceStorage = newStorage}
          repo' = M.insert addr ci' repo
      Right (ret, repo')
