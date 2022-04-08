-- | Parsing GAP files with Parsec
module AST where

import Control.Monad (void, when)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

type Op = String

data Expression

data Statement

data Variable

data Branch

data AST a where
  Variable :: String -> AST Variable
  Ident :: [AST Variable] -> (Maybe (AST Expression)) -> AST Expression
  StringLit :: String -> AST Expression
  IntegerLit :: Integer -> AST Expression
  DoubleLit :: Double -> AST Expression
  CharLit :: Char -> AST Expression
  AnonymousFunc :: AST Variable -> AST Expression -> AST Expression
  UnaryOp :: Op -> AST Expression -> AST Expression
  BinOp :: Op -> AST Expression -> AST Expression -> AST Expression
  Permutation :: [[AST Expression]] -> AST Expression
  List :: [AST Expression] -> AST Expression
  Record :: [AST Statement] -> AST Expression
  FuncCall :: AST Variable -> [AST Expression] -> AST Expression
  Func :: [AST Variable] -> [AST Variable] -> AST Statement -> AST Expression
  Assignment :: AST Variable -> AST Expression -> AST Statement
  While :: AST Expression -> AST Statement -> AST Statement
  For :: AST Variable -> AST Expression -> AST Statement -> AST Statement
  Repeat :: AST Statement -> AST Expression -> AST Statement
  If :: AST Expression -> AST Statement -> Maybe (AST Branch) -> AST Statement
  Elif :: AST Expression -> AST Statement -> Maybe (AST Branch) -> AST Branch
  Else :: AST Statement -> AST Branch
  Break :: AST Statement
  Continue :: AST Statement
  Return :: AST Expression -> AST Statement
  Expression :: AST Expression -> AST Statement
  Statements :: [AST Statement] -> AST Statement

instance Show (AST a) where
  show (Variable s) = "Variable \"" <> s <> "\""
  show (Ident s i) = "Ident " <> show s <> " (" <> show i <> ")"
  show (StringLit s) = "StringLit \"" <> s <> "\""
  show (IntegerLit i) = "IntegerLit " <> show i
  show (DoubleLit d) = "DoubleLit " <> show d
  show (CharLit c) = "CharLit '" <> show c <> "'"
  show (AnonymousFunc v expr) = "AnonymousFunc (" <> show v <> ") " <> "(" <> show expr <> ")"
  show (UnaryOp op e) = "Unary \"" <> op <> "\" " <> show e
  show (BinOp op left right) = "BinOp \"" <> op <> "\" (" <> show left <> ") (" <> show right <> ")"
  show (Permutation exprs) = "Permutation " <> show exprs
  show (List exprs) = "List " <> show exprs
  show (Record exprs) = "Record " <> show exprs
  show (FuncCall f exprs) = "FuncCall " <> "(" <> show f <> ") " <> show exprs
  show (Func vals locals stmts) = "Func " <> show vals <> " " <> show locals <> " " <> show stmts
  show (Assignment val expr) = "Assignment (" <> show val <> ") (" <> show expr <> ")"
  show (While expr stmts) = "While (" <> show expr <> ") " <> show stmts
  show (For var expr stmts) = "For (" <> show var <> ") (" <> show expr <> ") " <> show stmts
  show (Repeat stmts expr) = "Repeat " <> show stmts <> " (" <> show expr <> ")"
  show (If cond stmts branch) = "If (" <> show cond <> ") " <> show stmts <> " (" <> show branch <> ")"
  show (Elif cond stmts branch) = "Elif (" <> show cond <> ") " <> show stmts <> " (" <> show branch <> ")"
  show (Else stmts) = "Else " <> show stmts
  show Break = "Break"
  show Continue = "Continue"
  show (Return expr) = "Return (" <> show expr <> ")"
  show (Expression expr) = "Expression (" <> show expr <> ")"
  show (Statements stmts) = show stmts

stringLit :: Parser (AST Expression)
stringLit = StringLit <$> between (char '"') (char '"') (many stringChar)
  where
    stringChar = satisfy (/= '"')

intLit :: Parser (AST Expression)
intLit = IntegerLit . read <$> many1 digit

doubleLit :: Parser (AST Expression)
doubleLit = DoubleLit . read <$> float
  where
    float = do
      pre <- many1 digit
      point <- string "."
      post <- many digit
      return (pre <> point <> post)

charLit :: Parser (AST Expression)
charLit = CharLit <$> between (char '\'') (char '\'') (satisfy (/= '\''))

lit :: Parser (AST Expression)
lit = choice (map try [stringLit, charLit, doubleLit, intLit])

variable :: Parser (AST Variable)
variable =
  fmap Variable . (>>= checkReserved) $
    variableName <$> many digit <*> letter <*> many alphaNum
  where
    variableName pref l post = pref <> [l] <> post
    checkReserved :: String -> Parser String
    checkReserved varName = if varName `elem` reservedKeywords then fail ("Variable name \"" <> varName <> "\" is a reserved keyword.") else return varName
    reservedKeywords = ["Assert", "Info", "IsBound", "QUIT", "TryNextMethod", "Unbind", "and", "atomic", "break", "continue", "do", "elif", "else", "end", "false", "fi", "for", "function", "if", "in ", "local", "mod", "not", "od", "or", "quit", "readonly", "readwrite", "rec", "repeat", "return", "then", "true", "until", "while"]

stoken :: String -> Parser ()
stoken t = void $ spaces >> string t >> spaces

recordAccess :: Parser (AST Expression)
recordAccess = Ident <$> sepBy1 variable (try $ stoken ".") <*> ((<|> pure Nothing) . fmap Just . try $ between (stoken "[") (stoken "]") expression)

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (stoken ",")

lambda :: Parser (AST Expression)
lambda = AnonymousFunc <$> variable <*> (stoken "->" >> expression)

bracketList :: Parser a -> Parser [a]
bracketList p = stoken "(" >> (commaSep p <* stoken ")")

functionCall :: Parser (AST Expression)
functionCall = FuncCall <$> variable <*> bracketList expression

permutation :: Parser (AST Expression)
permutation = Permutation <$> many1 (bracketList expression)

record :: Parser (AST Expression)
record = Record <$> (stoken "rec" >> bracketList assignment)

list :: Parser (AST Expression)
list = List <$> between (stoken "[") (stoken "]") (sepBy expression (stoken ","))

table =
  [ [binary "^" AssocNone],
    [unary "+", unary "-"],
    [binary "*" AssocLeft, binary "/" AssocLeft, binary "mod" AssocLeft],
    [binary "+" AssocLeft, binary "-" AssocLeft],
    [binary "in" AssocNone, binary "=" AssocNone, binary "<" AssocNone, binary ">" AssocNone, binary "<=" AssocNone, binary ">=" AssocNone, binary "<>" AssocNone],
    [unary "not"],
    [binary "and" AssocLeft],
    [binary "or" AssocLeft]
  ]
  where
    binary op = Infix (do try (stoken op); return (BinOp op))
    unary op = Prefix (do try (stoken op); return (UnaryOp op))

expression :: Parser (AST Expression)
expression = buildExpressionParser table term <?> "expression"

bracketedExpr :: Parser (AST Expression)
bracketedExpr = between (stoken "(") (stoken ")") expression

term :: Parser (AST Expression)
term = (choice . map try) [bracketedExpr, functionDef, lambda, lit, functionCall, permutation, record, list, recordAccess] <?> "simple expression"

-- binOp :: Parser (AST Expression)
-- binOp = chainl1 (try binOpExpression) (BinOp <$> try op)
--   where
--     op = spaces >> choice (map (try . string) [">=", "<=", "=", "<>", "<", ">", "in", "+", "-", "/", "mod", "^", "and", "*", "or"]) <* spaces

-- binOpExpression :: Parser (AST Expression)
-- binOpExpression = choice (map try [lit, lambda, functionCall, permutation, record, list, bracketedExpr, recordAccess])

functionDef :: Parser (AST Expression)
functionDef = Func <$> (stoken "function" >> bracketList variable) <*> (try localDeclare <|> pure []) <*> (spaces >> functionStatements <* stoken "end")
  where
    localDeclare = do
      stoken "local"
      arr <- commaSep variable
      spaces
      many (char ';')
      return arr

-- expression :: Parser (AST Expression)
-- expression = choice (map try [binOp, functionDef, lambda, lit, functionCall, permutation, record, list, recordAccess])

loopStatements :: Parser (AST Statement)
loopStatements = Statements <$> many (try loopStatement)

comment :: Parser ()
comment = void $ stoken "#" >> manyTill anyChar (try $ void (char '\n') <|> eof)

assignment :: Parser (AST Statement)
assignment = Assignment <$> variable <*> (stoken ":=" >> expression) <?> "assignment"

for :: Parser (AST Statement)
for = For <$> (stoken "for" >> variable) <*> (stoken "in" >> expression) <*> (stoken "do" >> statements <* stoken "od") <?> "for statement"

while :: Parser (AST Statement)
while = While <$> (stoken "while" >> expression) <*> (stoken "do" >> loopStatements <* stoken "od") <?> "while statement"

repeatStmt :: Parser (AST Statement)
repeatStmt = Repeat <$> (stoken "repeat" >> loopStatements) <*> (stoken "until" >> expression) <?> "repeat statement"

elifStmt :: Parser (AST Branch)
elifStmt = Elif <$> (stoken "elif" >> expression) <*> (stoken "then" >> statements) <*> branch <?> "elif statement"

elseStmt :: Parser (AST Branch)
elseStmt = Else <$> (stoken "else" >> statements) <?> "else statement"

branch :: Parser (Maybe (AST Branch))
branch = (<|> pure Nothing) . fmap Just . choice . map try $ [elifStmt, elseStmt]

ifStmt :: Parser (AST Statement)
ifStmt = If <$> (stoken "if" >> expression) <*> (stoken "then" >> statements) <*> (branch <* stoken "fi") <?> "if statement"

lineOps :: [Parser (AST Statement)] -> Parser (AST Statement)
lineOps stmts = choice $ map (\p -> try $ (spaces >> p) <* ((spaces >> many1 (char ';') >> spaces) <?> ";")) stmts

statement :: Parser (AST Statement)
statement = lineOps [ifStmt, Expression <$> expression, assignment, for, while, repeatStmt] <?> "statement"

functionStatement :: Parser (AST Statement)
functionStatement = lineOps [ifStmt, Expression <$> expression, assignment, for, while, repeatStmt, returnStmt] <?> "statement"

functionStatements :: Parser (AST Statement)
functionStatements = Statements <$> many (try functionStatement) <?> "statement"

loopStatement :: Parser (AST Statement)
loopStatement = lineOps [Expression <$> expression, ifStmt, assignment, for, while, repeatStmt, breakStmt, continue] <?> "statement"

statements :: Parser (AST Statement)
statements = Statements <$> many (try statement) <?> "statement"

returnStmt :: Parser (AST Statement)
returnStmt = Return <$> (stoken "return" >> expression) <?> "return statement"

breakStmt :: Parser (AST Statement)
breakStmt = stoken "break" >> pure Break <?> "break statement"

continue :: Parser (AST Statement)
continue = stoken "continue" >> pure Continue <?> "continue statement"
