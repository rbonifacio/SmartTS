{-# LANGUAGE DeriveFunctor #-}

-- | Haskell translation of the LLTZ intermediate representation.
--
module SmartTS.IR.LLTZ.Core where


-- ---------------------------------------------------------------------------
-- Row
-- ---------------------------------------------------------------------------

-- | Labels annotate leaves of a row (become Michelson field annotations like %fieldName).
newtype Label = Label String
  deriving (Eq, Show)

-- | A rose-tree structure used for n-ary product types (Tuple) and n-ary sum
-- types (Or).  Each leaf optionally carries a label.
data Row a
  = RowNode [Row a]
  | RowLeaf (Maybe Label) a
  deriving (Eq, Show, Functor)

-- | A row with one hole — used to specify the injection position in a sum type.
-- @RowCtxNode lefts middle rights@ means the hole is at the position of
-- @middle@ inside a node whose other children are @lefts@ (before) and
-- @rights@ (after).
data RowContext a
  = RowHole a
  | RowCtxNode [Row a] (Row a) [Row a]
  deriving (Eq, Show)

-- | A path into a row: a list of child indices, one per Node level.
newtype RowPath = RowPath [Int]
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data Type
  = TTuple (Row Type)
  | TOr (Row Type)
  | TOption Type
  | TList Type
  | TSet Type
  | TFunction Type Type
  | TMap Type Type
  | TBool
  | TNat
  | TInt
  | TString
  | TUnit
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Variables
-- ---------------------------------------------------------------------------

newtype Var    = Var    String  deriving (Eq, Show)
newtype MutVar = MutVar String  deriving (Eq, Show)

-- | A binder pairs a variable name with its declared type.
type Binder = (Var, Type)

-- ---------------------------------------------------------------------------
-- Lambda helpers
-- ---------------------------------------------------------------------------

-- | A lambda abstraction with a single argument.
data LambdaBinder = LambdaBinder
  { lamVar  :: Binder
  , lamBody :: Expr
  } deriving (Eq, Show)

-- | A lambda abstraction with two arguments (used by IfCons to bind head and tail).
data Lambda2Binder = Lambda2Binder
  { lam2Var1 :: Binder
  , lam2Var2 :: Binder
  , lam2Body :: Expr
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

data Constant
  = CUnit
  | CBool      Bool
  | CNat       Int   -- ^ non-negative; Integer for arbitrary precision
  | CInt       Int
  | CString    String
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

-- | Built-in operations.  Constructors are prefixed with 'Prim' to avoid
-- clashes with Haskell prelude names (Left, Right, Not, …).
data Primitive
  = PrimEmptyMap    Type Type
  | PrimEmptySet    Type
  | PrimNil         Type
  | PrimNone        Type
  | PrimUnit
  -- arity 1 / 2
  | PrimCar
  | PrimCdr
  | PrimLeft        (Maybe String) (Maybe String) Type  -- ^ left-annot, right-annot, right-type
  | PrimRight       (Maybe String) (Maybe String) Type  -- ^ left-annot, right-annot, left-type
  | PrimSome
  | PrimEq
  | PrimAbs
  | PrimNeg
  | PrimCastNat     -- ^ ISNAT / NAT cast (INT → option NAT)
  | PrimCastInt     -- ^ INT cast (NAT → INT)
  | PrimPackBytes   -- ^ BYTES cast
  | PrimIsNat
  | PrimNeq
  | PrimLe
  | PrimLt
  | PrimGe
  | PrimGt
  | PrimNot
  | PrimSize
  | PrimContract    (Maybe String) Type   -- ^ optional entrypoint annotation, parameter type
  | PrimGetN        Int                 -- ^ GET n (right-comb index)
  | PrimCast        Type
  | PrimRename      (Maybe String)
  | PrimFailwith
  | PrimNever
  | PrimPair        (Maybe String) (Maybe String)  -- ^ field annotations
  | PrimAdd
  | PrimMul
  | PrimSub
  | PrimLsr
  | PrimLsl
  | PrimXor
  | PrimEdiv
  | PrimAnd
  | PrimOr
  | PrimCons
  | PrimCompare
  | PrimConcat1     -- ^ CONCAT on a list
  | PrimConcat2     -- ^ CONCAT on two values
  | PrimGet
  | PrimMem
  | PrimExec
  | PrimApply
  | PrimUpdateN     Int                 -- ^ UPDATE n (right-comb index)
  | PrimSlice
  | PrimUpdate
  | PrimGetAndUpdate
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | A typed LLTZ expression.  Every node carries its 'Type'; source locations
-- and optimisation annotations are omitted.
data Expr = Expr
  { exprDesc :: ExprDesc
  , exprType :: Type
  } deriving (Eq, Show)

data ExprDesc
  = Const       Constant
  | Variable    Var
  | LetIn       Var Expr Expr
  | LetMutIn    MutVar Expr Expr
  | Lambda      LambdaBinder
  | LambdaRec   Binder LambdaBinder
  | App         Expr Expr
  | Prim        Primitive [Expr]
  | Deref       MutVar
  | Assign      MutVar Expr
  | IfBool      Expr Expr Expr
  | IfNone      Expr Expr LambdaBinder
  | IfCons      Expr Expr Lambda2Binder
  | IfLeft      Expr LambdaBinder LambdaBinder
  | While       Expr Expr
  | WhileLeft   Expr LambdaBinder
  | For         MutVar Expr Expr Expr Expr
  | ForEach     Expr LambdaBinder
  | MapColl     Expr LambdaBinder
  | FoldLeft    Expr Expr LambdaBinder
  | FoldRight   Expr Expr LambdaBinder
  | LetTupleIn  [Var] Expr Expr
  | TupleExpr   (Row Expr)
  | Proj        Expr RowPath
  | UpdateTuple Expr RowPath Expr
  | Inj         (RowContext Type) Expr
  | Match       Expr (Row LambdaBinder)
  | Skip        -- LLTZ extension. Do nothing!!!
  deriving (Eq, Show)
