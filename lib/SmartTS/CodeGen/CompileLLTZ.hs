module SmartTS.CodeGen.CompileLLTZ where

import qualified SmartTS.IR.AST          as A
import qualified SmartTS.IR.LLTZ.Core    as L
import qualified SmartTS.IR.LLTZ.Builder as B
import qualified Data.Map.Strict         as M

translateType :: A.Type -> L.Type
translateType A.TInt             = L.TInt
translateType A.TBool            = L.TBool
translateType A.TString          = L.TString
translateType A.TUnit            = L.TUnit
translateType (A.TRecord fields) = L.TTuple (L.RowNode (map toLeaf fields))
  where
    toLeaf (name, ty) = L.RowLeaf (Just (L.Label name)) (translateType ty)
translateType (A.TMap k v)       = L.TMap (translateType k) (translateType v)

-- Basic Expressions
translateExpression :: A.TypedExpr -> L.Expr
translateExpression (A.CInt  ty value) = B.constInt  value (translateType ty)
translateExpression (A.CBool ty value) = B.constBool value (translateType ty)
translateExpression (A.CString ty value) = B.constString value (translateType ty)
translateExpression (A.Var   ty name)  = B.variable  name  (translateType ty)
-- Boolean Expressions
translateExpression (A.And ty e1 e2) = translateBinaryExpression e1 e2 ty L.PrimAnd
translateExpression (A.Or  ty e1 e2) = translateBinaryExpression e1 e2 ty L.PrimOr
translateExpression (A.Not ty e)     = translateUnaryExpression e ty L.PrimNot
-- String Expressions
translateExpression (A.Add ty e1 e2)
  | ty == A.TString = translateBinaryExpression e1 e2 ty L.PrimConcat2
translateExpression (A.Call ty "length" [e]) = translateUnaryExpression e ty L.PrimSize
-- Map Expressions
translateExpression (A.MapEmpty ty) =
  case translateType ty of
    lt@(L.TMap k v) -> B.prim (L.PrimEmptyMap k v) [] lt
    _ -> error "[Impossible] empty_map with a non-map type after type check."
-- Map literals only exist as runtime values; they are built by successive updates.
translateExpression (A.MapVal ty entries) =
  case translateType ty of
    lt@(L.TMap k v) ->
      foldr
        (\(key, val) acc ->
          B.prim L.PrimUpdate [translateExpression key, B.some (translateExpression val), acc] lt)
        (B.prim (L.PrimEmptyMap k v) [] lt)
        (M.toList entries)
    _ -> error "[Impossible] map value with a non-map type after type check."
translateExpression (A.MapMemCheck ty mapExpr key) =
  B.prim L.PrimMem [translateExpression key, translateExpression mapExpr] (translateType ty)
translateExpression (A.MapRem ty mapExpr key) =
  case translateType ty of
    lt@(L.TMap _ v) ->
      B.prim L.PrimUpdate [translateExpression key, B.none v, translateExpression mapExpr] lt
    _ -> error "[Impossible] remove with a non-map type after type check."
translateExpression (A.MapAccess ty mapExpr key) =
  mapGet (translateExpression mapExpr) (translateExpression key) (translateType ty)
-- TODO: Write here the translation of the remaining expressions.

-- | Translate a SmartTS block (a list of statements) into a nested LLTZ let-expression.
--
-- LLTZ is an expression-based language derived from the Lambda calculus.
-- A statement sequence is encoded as a chain of LetIn / LetMutIn nodes where
-- each binding carries the rest of the block as its continuation.  The type of
-- the whole chain is propagated from the innermost expression, so a block that
-- ends in a ReturnStmt carries the return type all the way to the top.
--
-- I decided to enrich LLTZ with a Skip expression, which does not have any effect
-- in the program. This facilitates the translation, but there might exist a differnt
-- approach to deal with situations, for instance, where we have an if-then without
-- an else.
--
-- TODO: Reason about the effect of 'return statements'
translateBlock :: [A.TypedStmt] -> L.Expr
translateBlock [] = B.skip
translateBlock (s:ss) =
  case s of
    (A.VarDeclStmt name _ty expr) -> B.letMutIn name (lltzExpr expr) block
    (A.ValDeclStmt name _ty expr) -> B.letIn    name (lltzExpr expr) block
    -- For effectful statements whose value is not bound, we sequence them by
    -- discarding the result via a wildcard binding.
    _                             -> B.letIn "_" (translateStatement s) block
  where
    block = translateBlock ss
    lltzExpr = translateExpression

-- | Translate a single SmartTS statement into an LLTZ expression.
translateStatement :: A.TypedStmt -> L.Expr
-- Translate the assignment statement.
translateStatement (A.AssignmentStmt lv expr) =
  translateAssignment lv (translateExpression expr)
-- Translate the if-then-else statement.
translateStatement (A.IfStmt cond s1 (Just s2)) =
  let cond' = translateExpression cond
      s1'   = translateStatement s1
      s2'   = translateStatement s2
  in
    -- Branch type equality is guaranteed by the type checker; the assert
    -- is a defensive check that catches any inconsistency in this pass.
    assert
      (L.exprType s1' == L.exprType s2')
      "[Impossible] Inconsistent branch types."
      (B.ifBool cond' s1' s2')
-- Translate the if-then statement (no else branch).
-- Here, the need to a skip statement becomes more clear.
translateStatement (A.IfStmt cond s1 Nothing) =
  B.ifBool (translateExpression cond) (translateStatement s1) B.skip
-- Translate the for statement.
-- The parser only accepts a mutable declaration as the initializer, so the
-- loop variable and its initial value map directly onto LLTZ's For node.
-- Like while, the loop produces no value (TUnit).
translateStatement (A.ForStmt (A.VarDeclStmt name _ty initExpr) cond update block) =
  B.for name
    (translateExpression initExpr)
    (translateExpression cond)
    (translateStatement update)
    (translateStatement block)
translateStatement (A.ForStmt {}) =
  error "[Impossible] The for initializer must be a var declaration."
-- Translate the while statement.
-- The result type is TUnit because Michelson's LOOP instruction does not produce
-- a value: when the loop exits the stack is in the same state as before the
-- condition was first evaluated, so no value escapes the loop.
translateStatement (A.WhileStmt cond block) =
  B.while (translateExpression cond) (translateStatement block)
-- In LLTZ (an expression-based IR) there is no explicit return construct:
-- the value of the last expression in a block is the return value.
translateStatement (A.ReturnStmt expr) = translateExpression expr
-- Translate a nested block of statements.
translateStatement (A.SequenceStmt stmts) = translateBlock stmts

-- Auxiliary functions for translating assignments.

-- | Assign a value to an LValue. A map update @m[k] = v@ becomes the
-- assignment of @UPDATE k (Some v) m@ to @m@, recursively for nested maps.
-- TODO: Deal with the remaining LValues (storage and record field).
translateAssignment :: A.TypedLValue -> L.Expr -> L.Expr
translateAssignment (A.LVar name) value = B.assign name value
translateAssignment (A.LMapAccess lv key) value =
  let key'  = translateExpression key
      mapTy = L.TMap (L.exprType key') (L.exprType value)
      updated = B.prim L.PrimUpdate [key', B.some value, readLValue lv mapTy] mapTy
  in translateAssignment lv updated
translateAssignment _ _ =
  error "Code generation for assignments to storage and record fields is not supported yet."

-- | Read the current value of an LValue whose type is known from the context.
readLValue :: A.TypedLValue -> L.Type -> L.Expr
readLValue (A.LVar name) ty = B.variable name ty
readLValue (A.LMapAccess lv key) ty =
  let key' = translateExpression key
  in mapGet (readLValue lv (L.TMap (L.exprType key') ty)) key' ty
readLValue _ _ =
  error "Code generation for reads of storage and record fields is not supported yet."

-- | Look up a key in a map, failing with MAP_ACCESS_KEY_NOT_FOUND when it is absent.
mapGet :: L.Expr -> L.Expr -> L.Type -> L.Expr
mapGet mapExpr key valTy =
  let found   = B.prim L.PrimGet [key, mapExpr] (L.TOption valTy)
      failure = B.prim L.PrimFailwith [B.constString "MAP_ACCESS_KEY_NOT_FOUND" L.TString] valTy
      binder  = L.LambdaBinder (L.Var "__value", valTy) (B.variable "__value" valTy)
  in L.Expr (L.IfNone found failure binder) valTy

-- Auxiliary functions for translating expressions.

translateUnaryExpression :: A.TypedExpr -> A.Type -> L.Primitive -> L.Expr
translateUnaryExpression e ty prim = B.prim prim [translateExpression e] (translateType ty)

translateBinaryExpression :: A.TypedExpr -> A.TypedExpr -> A.Type -> L.Primitive -> L.Expr
translateBinaryExpression left right ty prim =
  B.prim prim [translateExpression left, translateExpression right] (translateType ty)

assert :: Bool -> String -> a -> a
assert False msg _ = error msg
assert True  _   v = v
