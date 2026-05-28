module SmartTS.Interpreter.Eval where

import qualified Data.Map.Strict as M
import SmartTS.AST
import SmartTS.Interpreter.Runtime

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
