module SmartTS.IR.AST where

data Contract a = Contract {
  contractName :: Name,
  contractStorage :: Storage,
  contractMethods :: [MethodDecl a]
} deriving (Eq, Show)

type Storage = [(Name, Type)]

data MethodDecl a = MethodDecl {
  methodKind :: MethodKind,
  methodName :: Name,
  methodArgs :: [FormalParameter],
  methodReturnType ::  ReturnType,
  methodBody ::  MethodBody a
} deriving (Eq, Show)

data MethodKind = Originate
                | EntryPoint
                | Private
  deriving (Eq, Show)

data FormalParameter = FormalParameter Name Type
  deriving (Eq, Show)

type ReturnType = Type

data Type = TInt
          | TBool
          | TUnit
          | TRecord [(Name, Type)]
  deriving (Eq, Show)

type Name = String

data Expr a
  = CInt    a Int
  | CBool   a Bool
  | StorageExpr a
  | Var     a Name
  | FieldAccess a (Expr a) Name
  | And     a (Expr a) (Expr a)
  | Or      a (Expr a) (Expr a)
  | Not     a (Expr a)
  | Add     a (Expr a) (Expr a)
  | Sub     a (Expr a) (Expr a)
  | Mul     a (Expr a) (Expr a)
  | Div     a (Expr a) (Expr a)
  | Mod     a (Expr a) (Expr a)
  | Eq      a (Expr a) (Expr a)
  | Neq     a (Expr a) (Expr a)
  | Lt      a (Expr a) (Expr a)
  | Lte     a (Expr a) (Expr a)
  | Gt      a (Expr a) (Expr a)
  | Gte     a (Expr a) (Expr a)
  | Record  a [(Name, Expr a)]
  | Unit    a
  | Call    a Name [Expr a]
  deriving (Eq, Show)

-- | Extract the annotation from any expression node.
exprAnn :: Expr a -> a
exprAnn (CInt a _)          = a
exprAnn (CBool a _)         = a
exprAnn (StorageExpr a)     = a
exprAnn (Var a _)           = a
exprAnn (FieldAccess a _ _) = a
exprAnn (And a _ _)         = a
exprAnn (Or a _ _)          = a
exprAnn (Not a _)           = a
exprAnn (Add a _ _)         = a
exprAnn (Sub a _ _)         = a
exprAnn (Mul a _ _)         = a
exprAnn (Div a _ _)         = a
exprAnn (Mod a _ _)         = a
exprAnn (Eq a _ _)          = a
exprAnn (Neq a _ _)         = a
exprAnn (Lt a _ _)          = a
exprAnn (Lte a _ _)         = a
exprAnn (Gt a _ _)          = a
exprAnn (Gte a _ _)         = a
exprAnn (Record a _)        = a
exprAnn (Unit a)            = a
exprAnn (Call a _ _)        = a

type MethodBody a = Stmt a

-- | What is allowed on the left-hand side of an assignment.
-- Supports TypeScript-like record field paths: `x.a.b`.
data LValue
  = LStorage
  | LVar Name
  | LField LValue Name
  deriving (Eq, Show)

data Stmt a
  = AssignmentStmt LValue (Expr a)
  | VarDeclStmt Name Type (Expr a)       -- (mutable)
  | ValDeclStmt Name Type (Expr a)       -- (immutable)
  | IfStmt (Expr a) (Stmt a) (Maybe (Stmt a))   -- (condition, then, else)
  | ForStmt (Stmt a) (Expr a) (Stmt a) (Stmt a) -- (init, condition, update, body)
  | WhileStmt (Expr a) (Stmt a)                 -- (condition, body)
  | ReturnStmt (Expr a)
  | SequenceStmt [Stmt a]
  deriving (Eq, Show)

-- | Type aliases for the two phases of the compilation pipeline.
type ParsedExpr     = Expr ()
type TypedExpr      = Expr Type
type ParsedStmt     = Stmt ()
type TypedStmt      = Stmt Type
type ParsedContract = Contract ()
type TypedContract  = Contract Type

findMethods :: MethodKind -> Contract a -> [MethodDecl a]
findMethods k c = [m | m <- contractMethods c, methodKind m == k]

findOriginatorMethod :: Contract a -> MethodDecl a
findOriginatorMethod c =
  case findMethods Originate c of
    []  -> error "A contract must declare one `originate` method."
    [m] -> m
    _   -> error "A contract must declare just one `originate` method."

isEntryPointMethod :: MethodDecl a -> Bool
isEntryPointMethod m = methodKind m == EntryPoint

isOriginateMethod :: MethodDecl a -> Bool
isOriginateMethod m = methodKind m == Originate
