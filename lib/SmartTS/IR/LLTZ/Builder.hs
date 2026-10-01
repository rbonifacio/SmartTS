module SmartTS.IR.LLTZ.Builder where

import qualified SmartTS.IR.LLTZ.Core as L

-- ---------------------------------------------------------------------------
-- Effectful expressions (always typed TUnit)
-- ---------------------------------------------------------------------------

skip :: L.Expr
skip = L.Expr L.Skip L.TUnit

assign :: String -> L.Expr -> L.Expr
assign v e = L.Expr (L.Assign (L.MutVar v) e) L.TUnit

while :: L.Expr -> L.Expr -> L.Expr
while c b = L.Expr (L.While c b) L.TUnit

for :: String -> L.Expr -> L.Expr -> L.Expr -> L.Expr -> L.Expr
for v i c u b = L.Expr (L.For (L.MutVar v) i c u b) L.TUnit

-- ---------------------------------------------------------------------------
-- Type-propagating expressions (type inferred from sub-expressions)
-- ---------------------------------------------------------------------------

letIn :: String -> L.Expr -> L.Expr -> L.Expr
letIn v e body = L.Expr (L.LetIn (L.Var v) e body) (L.exprType body)

letMutIn :: String -> L.Expr -> L.Expr -> L.Expr
letMutIn v e body = L.Expr (L.LetMutIn (L.MutVar v) e body) (L.exprType body)

-- | Type is taken from the true branch; callers must ensure both branches agree.
ifBool :: L.Expr -> L.Expr -> L.Expr -> L.Expr
ifBool c t f = L.Expr (L.IfBool c t f) (L.exprType t)

-- ---------------------------------------------------------------------------
-- Typed leaf expressions
-- ---------------------------------------------------------------------------

constInt :: Int -> L.Type -> L.Expr
constInt v ty = L.Expr (L.Const (L.CInt v)) ty

constBool :: Bool -> L.Type -> L.Expr
constBool v ty = L.Expr (L.Const (L.CBool v)) ty

variable :: String -> L.Type -> L.Expr
variable name ty = L.Expr (L.Variable (L.Var name)) ty

prim :: L.Primitive -> [L.Expr] -> L.Type -> L.Expr
prim p args ty = L.Expr (L.Prim p args) ty
