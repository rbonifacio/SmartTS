module SmartTS.Interpreter.Codec where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Data.Scientific (floatingOrInteger)
import SmartTS.IR.AST
import SmartTS.Interpreter.Runtime
import qualified Data.Text as T

exprToJson :: Expr a -> Value
exprToJson (CInt _ n) = Number (fromIntegral n)
exprToJson (CBool _ b) = Bool b
exprToJson (CString _ s) = String (T.pack s)
exprToJson (Record _ fields) =
  Object $
    KM.fromList
      [ (fromStringKey k, exprToJson v)
      | (k, v) <- fields
      ]
exprToJson (Unit _) = Null
exprToJson _ = Null

jsonToExprByType :: Type -> Value -> Either String TypedExpr
jsonToExprByType TInt (Number n) =
  case floatingOrInteger n :: Either Double Int of
    Right i -> Right (CInt TInt i)
    Left _  -> Left "Expected integer number for int type."
jsonToExprByType TBool (Bool b) = Right (CBool TBool b)
jsonToExprByType TString (String s) =
  Right (CString TString (T.unpack s))
jsonToExprByType (TRecord fieldsT) (Object obj) = do
  pairs <- mapM (decodeField obj) fieldsT
  Right (Record (TRecord fieldsT) pairs)
  where
    decodeField o (fname, ftype) =
      case KM.lookup (fromStringKey fname) o of
        Nothing -> Left $ "Missing record field in JSON args: " ++ fname
        Just v  -> do
          ev <- jsonToExprByType ftype v
          Right (fname, ev)
jsonToExprByType _ _ = Left "JSON value does not match the expected SmartTS type."

jsonToExprUntyped :: Value -> Either String ParsedExpr
jsonToExprUntyped (Number n) =
  case floatingOrInteger n :: Either Double Int of
    Right i -> Right (CInt () i)
    Left _  -> Left "Only integer numbers are currently supported."
jsonToExprUntyped (Bool b) = Right (CBool () b)
jsonToExprUntyped (String s) =
  Right (CString () (T.unpack s))
jsonToExprUntyped Null = Right (Unit ())
jsonToExprUntyped (Object obj) = do
  fields <- mapM decodeKV (KM.toList obj)
  Right (Record () fields)
  where
    decodeKV (k, v) = do
      ev <- jsonToExprUntyped v
      Right (toStringKey k, ev)
jsonToExprUntyped _ = Left "Unsupported JSON value for SmartTS expression."

-- | Decode persisted @storage@ JSON using the contract\'s declared storage record type.
contractInstanceFromStorageValue :: Contract a -> Value -> Either String ContractInstance
contractInstanceFromStorageValue c v = do
  st <- jsonToExprByType (TRecord (contractStorage c)) v
  Right (ContractInstance (contractName c) st)

bindArgsByName :: [FormalParameter] -> Value -> Either String (M.Map Name TypedExpr)
bindArgsByName params (Object obj) = do
  pairs <- mapM decodeParam params
  Right (M.fromList pairs)
  where
    decodeParam (FormalParameter pname ptype) =
      case KM.lookup (fromStringKey pname) obj of
        Nothing -> Left $ "Missing argument in JSON object: " ++ pname
        Just v  -> do
          e <- jsonToExprByType ptype v
          Right (pname, e)
bindArgsByName _ _ = Left "--args must be a JSON object."

fromStringKey :: String -> K.Key
fromStringKey = K.fromString

toStringKey :: K.Key -> String
toStringKey = K.toString
