module SmartTS.Parser where

import SmartTS.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void

type Parser = Parsec Void String

-- Lexer helpers
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Keywords and identifiers
reservedWords :: [String]
reservedWords =
  [ "contract"
  , "storage"
  , "int"
  , "bool"
  , "unit"
  , "return"
  , "if"
  , "else"
  , "while"
  , "var"
  , "val"
  , "true"
  , "false"
  , "view"
  ]

identifier :: Parser String
identifier = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  if name `elem` reservedWords
    then fail $ "reserved word: " ++ name
    else return name

reserved :: String -> Parser String
reserved = symbol

-- Types
parseType :: Parser Type
parseType = parseRecordType <|> parsePrimitiveType
  where
    parsePrimitiveType :: Parser Type
    parsePrimitiveType =
      (reserved "int" >> return TInt)
        <|> (reserved "bool" >> return TBool)
        <|> (reserved "unit" >> return TUnit)

    parseRecordType :: Parser Type
    parseRecordType = do
      fields <- braces $ sepBy parseTypeField (symbol ",")
      return $ TRecord fields

    parseTypeField :: Parser (Name, Type)
    parseTypeField = do
      name <- parseName
      _ <- symbol ":"
      typ <- parseType
      return (name, typ)

-- Names
parseName :: Parser Name
parseName = identifier

-- Expressions
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operators

operators :: [[Operator Parser Expr]]
operators =
  [ [ Prefix (Not <$ symbol "!") ]
  , [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")
    , InfixL (Mod <$ symbol "%")
    ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-")
    ]
  , [ InfixN (Eq <$ symbol "==")
    , InfixN (Neq <$ symbol "!=")
    , InfixN (Lte <$ symbol "<=")
    , InfixN (Gte <$ symbol ">=")
    , InfixN (Lt <$ symbol "<")
    , InfixN (Gt <$ symbol ">")
    ]
  , [ InfixL (And <$ symbol "&&") ]
  , [ InfixL (Or <$ symbol "||") ]
  ]

parseTerm :: Parser Expr
parseTerm = do
  base <- parseAtomOrStorage
  fields <- many (symbol "." *> parseName)
  return (foldl FieldAccess base fields)

parseAtomOrStorage :: Parser Expr
parseAtomOrStorage =
  parseStorageExpr
    <|> parseAtom

parseAtom :: Parser Expr
parseAtom =
  parseUnit
    <|> parseRecordExpr
    <|> parseBool
    <|> parseInt
    <|> parseVar
    <|> parens parseExpr

parseStorageExpr :: Parser Expr
parseStorageExpr = do
  _ <- reserved "storage"
  return StorageExpr

parseInt :: Parser Expr
parseInt = CInt <$> lexeme L.decimal

parseVar :: Parser Expr
parseVar = Var <$> parseName

parseBool :: Parser Expr
parseBool =
  (reserved "true" >> return (CBool True))
    <|> (reserved "false" >> return (CBool False))

parseRecordExpr :: Parser Expr
parseRecordExpr = do
  fields <- braces $ sepBy parseRecordField (symbol ",")
  return $ Record fields

parseRecordField :: Parser (Name, Expr)
parseRecordField = do
  name <- parseName
  _ <- symbol ":"
  expr <- parseExpr
  return (name, expr)

parseUnit :: Parser Expr
parseUnit = do
  _ <- symbol "()"
  return Unit

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- Statements
parseStmt :: Parser Stmt
parseStmt =
  parseIfStmt
    <|> parseWhileStmt
    <|> parseVarDeclStmt
    <|> parseValDeclStmt
    <|> parseReturn
    <|> parseAssignment
    <|> parseBlock

parseVarDeclStmt :: Parser Stmt
parseVarDeclStmt = do
  _ <- reserved "var"
  name <- parseName
  _ <- symbol ":"
  typ <- parseType
  _ <- symbol "="
  expr <- parseExpr
  _ <- symbol ";"
  return $ VarDeclStmt name typ expr

parseValDeclStmt :: Parser Stmt
parseValDeclStmt = do
  _ <- reserved "val"
  name <- parseName
  _ <- symbol ":"
  typ <- parseType
  _ <- symbol "="
  expr <- parseExpr
  _ <- symbol ";"
  return $ ValDeclStmt name typ expr

parseIfStmt :: Parser Stmt
parseIfStmt = do
  _ <- reserved "if"
  cond <- parens parseExpr
  thenBranch <- parseStmt
  elseBranch <- optional $ do
    _ <- reserved "else"
    parseStmt
  return $ IfStmt cond thenBranch elseBranch

parseWhileStmt :: Parser Stmt
parseWhileStmt = do
  _ <- reserved "while"
  cond <- parens parseExpr
  body <- parseStmt
  return $ WhileStmt cond body

parseAssignment :: Parser Stmt
parseAssignment = do
  target <- parseLValue
  _ <- symbol "="
  expr <- parseExpr
  _ <- symbol ";"
  return $ AssignmentStmt target expr

parseLValue :: Parser LValue
parseLValue = do
  base <- parseAssignableBase
  fields <- many (symbol "." *> parseName)
  return (foldl LField base fields)

parseAssignableBase :: Parser LValue
parseAssignableBase =
  (reserved "storage" >> return LStorage) <|> (LVar <$> parseName)

parseReturn :: Parser Stmt
parseReturn = do
  _ <- reserved "return"
  expr <- parseExpr
  _ <- symbol ";"
  return $ ReturnStmt expr

parseBlock :: Parser Stmt
parseBlock = do
  stmts <- braces (many parseStmt)
  return $ SequenceStmt stmts

-- Storage
parseStorage :: Parser Storage
parseStorage = do
  _ <- reserved "storage"
  _ <- symbol ":"
  fields <- braces parseStorageFields
  _ <- symbol ";"
  return fields

parseStorageFields :: Parser Storage
parseStorageFields = sepBy parseStorageField (symbol ",")

parseStorageField :: Parser (Name, Type)
parseStorageField = do
  name <- parseName
  _ <- symbol ":"
  typ <- parseType
  return (name, typ)

-- Method decorators
parseMethodKind :: Parser MethodKind
parseMethodKind =
  (symbol "@originate" >> return Originate)
    <|> (symbol "@entrypoint" >> return EntryPoint)
    <|> (symbol "@private" >> return Private)

-- Formal parameters
parseFormalParameter :: Parser FormalParameter
parseFormalParameter = do
  name <- parseName
  _ <- symbol ":"
  FormalParameter name <$> parseType

parseFormalParameters :: Parser [FormalParameter]
parseFormalParameters = parens $ sepBy parseFormalParameter (symbol ",")

-- Methods
parseMethod :: Parser MethodDecl
parseMethod = do
  isView <- option False (True <$ reserved "view")
  decorators <- many parseMethodKind
  name <- parseName
  params <- parseFormalParameters
  _ <- symbol ":"
  returnType <- parseType
  body <- parseBlock
  let kind
        | Originate `elem` decorators = Originate
        | EntryPoint `elem` decorators = EntryPoint
        | otherwise = Private
  return $ MethodDecl kind name params returnType body

-- Contract
parseContract :: Parser Contract
parseContract = do
  _ <- reserved "contract"
  name <- parseName
  _ <- symbol "{"
  storage <- parseStorage
  methods <- many parseMethod
  _ <- symbol "}"
  return $ Contract name storage methods

-- Top-level parser
parseProgram :: Parser Contract
parseProgram = spaceConsumer >> parseContract <* eof

-- Public API
parseContractFromString :: String -> Either (ParseErrorBundle String Void) Contract
parseContractFromString = parse parseProgram ""

