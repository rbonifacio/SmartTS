module SmartTS.AST where

data Contract = Contract {
  contractName    :: Name,
  contractStorage :: Storage,
  contractEnums   :: [EnumDecl],
  contractMethods :: [MethodDecl]
} deriving (Eq, Show)

data EnumDecl = EnumDecl Name [Name]
  deriving (Eq, Show)

type Storage = [(Name, Type)]

data MethodDecl = MethodDecl {
  methodKind       :: MethodKind,
  methodName       :: Name,
  methodArgs       :: [FormalParameter],
  methodReturnType :: ReturnType,
  methodBody       :: MethodBody
} deriving (Eq, Show)

data MethodKind = Originate | EntryPoint | Private
  deriving (Eq, Show)

data FormalParameter = FormalParameter Name Type
  deriving (Eq, Show)

type ReturnType = Type

data Type
  = TInt
  | TBool
  | TUnit
  | TRecord [(Name, Type)]
  | TPair Type Type
  | TEnum Name
  deriving (Eq, Show)

type Name = String

data Expr
  = CInt  Int
  | CBool Bool
  | StorageExpr
  | Var Name
  | FieldAccess Expr Name
  | And  Expr Expr
  | Or   Expr Expr
  | Not  Expr
  | Add  Expr Expr
  | Sub  Expr Expr
  | Mul  Expr Expr
  | Div  Expr Expr
  | Mod  Expr Expr
  | Eq   Expr Expr
  | Neq  Expr Expr
  | Lt   Expr Expr
  | Lte  Expr Expr
  | Gt   Expr Expr
  | Gte  Expr Expr
  | Record [(Name, Expr)]
  | Unit
  | PairExpr Expr Expr
  | Fst Expr
  | Snd Expr
  | EnumLiteral Name
  deriving (Eq, Show)

type MethodBody = Stmt

data LValue
  = LStorage
  | LVar Name
  | LField LValue Name
  deriving (Eq, Show)

data Stmt
  = AssignmentStmt LValue Expr
  | VarDeclStmt Name Type Expr
  | ValDeclStmt Name Type Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | WhileStmt Expr Stmt
  | ReturnStmt Expr
  | SequenceStmt [Stmt]
  | VarDestructStmt Name Name Type Expr
  | ValDestructStmt Name Name Type Expr
  | MatchStmt Expr [(Name, Stmt)]
  deriving (Eq, Show)

findMethods :: MethodKind -> Contract -> [MethodDecl]
findMethods k c = [m | m <- contractMethods c, methodKind m == k]

findOriginatorMethod :: Contract -> MethodDecl
findOriginatorMethod c =
  case findMethods Originate c of
    []  -> error "A contract must declare one `originate` method."
    [m] -> m
    _   -> error "A contract must declare just one `originate` method."

isEntryPointMethod :: MethodDecl -> Bool
isEntryPointMethod m = methodKind m == EntryPoint

isOriginateMethod :: MethodDecl -> Bool
isOriginateMethod m = methodKind m == Originate
