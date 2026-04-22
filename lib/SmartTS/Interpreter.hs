-- | Evaluation of SmartTS contracts.
--
-- The CLI runs "SmartTS.TypeCheck.typeCheckContract" on source before interpretation.
-- Persisted storage is decoded with "jsonToExprByType" against the contract storage record.
-- After those steps, expression shapes that contradict the static types are treated as
-- internal bugs ("interpretBug") rather than user-facing "Left" errors.
module SmartTS.Interpreter where

import Data.Aeson (Value(..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy (fromStrict)
import Data.Char (isDigit, isHexDigit, toLower)
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.Foldable (toList)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as M
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import SmartTS.AST

type Address = Name

data ContractInstance = ContractInstance
  { instanceContractName :: Name
  , instanceStorage :: Expr
  }
  deriving (Eq, Show)

type RepositoryState = M.Map Address ContractInstance

data Binding = Binding
  { bindingMutable :: Bool
  , bindingValue :: Expr
  }
  deriving (Eq, Show)

data Runtime = Runtime
  { rtStorage :: Maybe Expr
  , rtParams :: M.Map Name Expr
  , rtLocals :: M.Map Name Binding
  }
  deriving (Eq, Show)

-- | Impossible case after type checking and typed storage decode (see module header).
interpretBug :: String -> a
interpretBug msg =
  error $ "SmartTS internal error (please report): " ++ msg

-- | First 16 hex characters of SHA-256 (UTF-8 bytes of @sourceText@). Used in addresses and call-time checks.
-- Implemented with the pure Haskell @SHA@ package (FIPS 180-2), no FFI.
sourceHashPrefix16 :: String -> String
sourceHashPrefix16 sourceText =
  let bs = encodeUtf8 (T.pack sourceText)
      digest = sha256 (fromStrict bs)
   in take 16 (showDigest digest)

-- | If @addr@ is @KT1@ + 16 hex digits + @_@ + decimal instance id, return those 16 hex chars (lower-cased).
-- Otherwise 'Nothing' (legacy name-based addresses or malformed ids).
parseEmbeddedSourceHashPrefix :: Address -> Maybe String
parseEmbeddedSourceHashPrefix addr = do
  rest <- stripPrefix "KT1" addr
  case break (== '_') rest of
    (hexPart, '_' : numStr)
      | length hexPart == 16,
        all isHexDigit hexPart,
        not (null numStr),
        all isDigit numStr ->
          Just (map toLower hexPart)
    _ -> Nothing

-- | When the address embeds a source hash, require the loaded file to match. Legacy addresses skip this.
assertAddressMatchesSource :: Address -> String -> Either String ()
assertAddressMatchesSource addr sourceText =
  case parseEmbeddedSourceHashPrefix addr of
    Nothing -> Right ()
    Just embedded ->
      let actual = map toLower (sourceHashPrefix16 sourceText)
       in if embedded == actual
            then Right ()
            else
              Left $
                "Contract source on disk does not match the code hash in the address "
                  ++ "(embedded "
                  ++ embedded
                  ++ "...; file hashes to "
                  ++ actual
                  ++ "...). Restore the original source or originate a new instance."

-- | Synthetic address: @KT1@ + first 16 hex chars of SHA-256(UTF-8 source) + @_@ + instance index.
-- Same source always yields the same prefix; the suffix distinguishes multiple deployments in one repo.
generateAddress :: String -> Int -> Address
generateAddress sourceText instanceId =
  "KT1" ++ sourceHashPrefix16 sourceText ++ "_" ++ show instanceId

exprToJson :: Expr -> Value
exprToJson (CInt n) = Number (fromIntegral n)
exprToJson (CBool b) = Bool b
exprToJson (Record fields) =
  Object $
    KM.fromList
      [ (fromStringKey k, exprToJson v)
      | (k, v) <- fields
      ]
exprToJson (List _ elems) = toJSON (map exprToJson elems)
exprToJson Unit = Null
exprToJson _ = Null

jsonToExprByType :: Type -> Value -> Either String Expr
jsonToExprByType TInt (Number n) =
  case floatingOrInteger n :: Either Double Int of
    Right i -> Right (CInt i)
    Left _ -> Left "Expected integer number for int type."
jsonToExprByType TBool (Bool b) = Right (CBool b)
jsonToExprByType (TRecord fieldsT) (Object obj) = do
  fields <- mapM (decodeField obj) fieldsT
  Right (Record fields)
  where
    decodeField o (fname, ftype) =
      case KM.lookup (fromStringKey fname) o of
        Nothing -> Left $ "Missing record field in JSON args: " ++ fname
        Just v -> do
          ev <- jsonToExprByType ftype v
          Right (fname, ev)
jsonToExprByType (TList elemType) (Array arr) = do
  elems <- mapM (jsonToExprByType elemType) (toList arr)
  Right (List elemType elems)
jsonToExprByType _ _ = Left "JSON value does not match the expected SmartTS type."

jsonToExprUntyped :: Value -> Either String Expr
jsonToExprUntyped (Number n) =
  case floatingOrInteger n :: Either Double Int of
    Right i -> Right (CInt i)
    Left _ -> Left "Only integer numbers are currently supported."
jsonToExprUntyped (Bool b) = Right (CBool b)
jsonToExprUntyped Null = Right Unit
jsonToExprUntyped (Object obj) = do
  fields <- mapM decodeKV (KM.toList obj)
  Right (Record fields)
  where
    decodeKV (k, v) = do
      ev <- jsonToExprUntyped v
      Right (toStringKey k, ev)
jsonToExprUntyped _ = Left "Unsupported JSON value for SmartTS expression."

-- | Decode persisted @storage@ JSON using the contract\'s declared storage record type.
contractInstanceFromStorageValue :: Contract -> Value -> Either String ContractInstance
contractInstanceFromStorageValue c v = do
  st <- jsonToExprByType (TRecord (contractStorage c)) v
  Right (ContractInstance (contractName c) st)

bindArgsByName :: [FormalParameter] -> Value -> Either String (M.Map Name Expr)
bindArgsByName params (Object obj) = do
  pairs <- mapM decodeParam params
  Right (M.fromList pairs)
  where
    decodeParam (FormalParameter pname ptype) =
      case KM.lookup (fromStringKey pname) obj of
        Nothing -> Left $ "Missing argument in JSON object: " ++ pname
        Just v -> do
          e <- jsonToExprByType ptype v
          Right (pname, e)
bindArgsByName _ _ = Left "--args must be a JSON object."

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
execStmt rt (ForEachStmt varName listExpr body) = do
  listVal <- evalExpr rt listExpr
  case listVal of
    List _ elems -> loop rt elems
    _ -> interpretBug "forEach iterable was not list after type check"
  where
    loop cur [] = Right (Nothing, cur)
    loop cur (el:rest) = do
      let bodyRt =
            cur
              { rtLocals =
                  M.insert varName (Binding False el) (rtLocals cur)
              }
      (ret, next) <- execStmt bodyRt body
      let nextNoVar = next {rtLocals = M.delete varName (rtLocals next)}
      case ret of
        Just v -> Right (Just v, nextNoVar)
        Nothing -> loop nextNoVar rest

execSequence :: Runtime -> [Stmt] -> Either String (Maybe Expr, Runtime)
execSequence rt [] = Right (Nothing, rt)
execSequence rt (s:ss) = do
  (ret, rt') <- execStmt rt s
  case ret of
    Just v -> Right (Just v, rt')
    Nothing -> execSequence rt' ss

evalExpr :: Runtime -> Expr -> Either String Expr
evalExpr _ c@(CInt _) = Right c
evalExpr _ c@(CBool _) = Right c
evalExpr _ Unit = Right Unit
evalExpr rt StorageExpr =
  case rtStorage rt of
    Nothing -> Right Unit
    Just s -> Right s
evalExpr rt (Var n) =
  case M.lookup n (rtLocals rt) of
    Just b -> Right (bindingValue b)
    Nothing ->
      case M.lookup n (rtParams rt) of
        Just v -> Right v
        Nothing -> interpretBug ("unknown variable `" ++ n ++ "` after type check")
evalExpr rt (Record fields) = do
  fs <- mapM (\(k, e) -> (,) k <$> evalExpr rt e) fields
  Right (Record fs)
evalExpr rt (List t elems) = do
  vs <- mapM (evalExpr rt) elems
  Right (List t vs)
evalExpr rt (FieldAccess base fld) = do
  b <- evalExpr rt base
  case b of
    Record fs ->
      case lookup fld fs of
        Just v -> Right v
        Nothing -> interpretBug ("missing record field `" ++ fld ++ "` after type check")
    _ -> interpretBug "field access on non-record after type check"
evalExpr rt (Not e) = do
  v <- evalExpr rt e
  case v of
    CBool b -> Right (CBool (not b))
    _ -> interpretBug "operand of ! was not bool after type check"
evalExpr rt (And a b) = boolBin rt a b (&&)
evalExpr rt (Or a b) = boolBin rt a b (||)
evalExpr rt (Add a b) = intBin rt a b (+)
evalExpr rt (Sub a b) = intBin rt a b (-)
evalExpr rt (Mul a b) = intBin rt a b (*)
evalExpr rt (Div a b) = do
  x <- evalInt rt a
  y <- evalInt rt b
  if y == 0 then Left "Division by zero." else Right (CInt (x `div` y))
evalExpr rt (Mod a b) = do
  x <- evalInt rt a
  y <- evalInt rt b
  if y == 0 then Left "Modulo by zero." else Right (CInt (x `mod` y))
evalExpr rt (Eq a b) = Right . CBool =<< ((==) <$> evalExpr rt a <*> evalExpr rt b)
evalExpr rt (Neq a b) = Right . CBool =<< ((/=) <$> evalExpr rt a <*> evalExpr rt b)
evalExpr rt (Lt a b) = intCmp rt a b (<)
evalExpr rt (Lte a b) = intCmp rt a b (<=)
evalExpr rt (Gt a b) = intCmp rt a b (>)
evalExpr rt (Gte a b) = intCmp rt a b (>=)
evalExpr rt (ListHead e) = do
  v <- evalExpr rt e
  case v of
    List _ [] -> Left "Head of empty list."
    List _ (x:_) -> Right x
    _ -> interpretBug "head on non-list after type check"
evalExpr rt (ListTail e) = do
  v <- evalExpr rt e
  case v of
    List _ [] -> Left "Tail of empty list."
    List t (_:xs) -> Right (List t xs)
    _ -> interpretBug "tail on non-list after type check"
evalExpr rt (ListSize e) = do
  v <- evalExpr rt e
  case v of
    List _ elems -> Right (CInt (length elems))
    _ -> interpretBug "size on non-list after type check"
evalExpr rt (ListCons el lst) = do
  vEl <- evalExpr rt el
  vLst <- evalExpr rt lst
  case vLst of
    List t elems -> Right (List t (vEl : elems))
    _ -> interpretBug "cons on non-list after type check"

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
setFieldPath base (f:fs) v = do
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
insertOrReplace k v ((k0, v0):rest)
  | k == k0 = (k, v) : rest
  | otherwise = (k0, v0) : insertOrReplace k v rest

evalInt :: Runtime -> Expr -> Either String Int
evalInt rt e = do
  v <- evalExpr rt e
  case v of
    CInt n -> Right n
    _ -> interpretBug "expected int subexpression after type check"

intBin :: Runtime -> Expr -> Expr -> (Int -> Int -> Int) -> Either String Expr
intBin rt a b op = do
  x <- evalInt rt a
  y <- evalInt rt b
  Right (CInt (op x y))

boolBin :: Runtime -> Expr -> Expr -> (Bool -> Bool -> Bool) -> Either String Expr
boolBin rt a b op = do
  x <- evalExpr rt a
  y <- evalExpr rt b
  case (x, y) of
    (CBool bx, CBool by) -> Right (CBool (op bx by))
    _ -> interpretBug "boolean operator on non-bool after type check"

intCmp :: Runtime -> Expr -> Expr -> (Int -> Int -> Bool) -> Either String Expr
intCmp rt a b op = do
  x <- evalInt rt a
  y <- evalInt rt b
  Right (CBool (op x y))

fromStringKey :: String -> K.Key
fromStringKey = K.fromString

toStringKey :: K.Key -> String
toStringKey = K.toString
