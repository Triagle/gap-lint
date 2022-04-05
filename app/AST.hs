-- | Parsing GAP files with Parsec
module AST where

import Control.Monad (void, when)
import Data.List (permutations)
import Text.Parsec
import Text.Parsec.String (Parser)

stringLit :: Parser ()
stringLit = void $ between (char '"') (char '"') (many stringChar)
  where
    stringChar = void (satisfy (/= '"'))

intLit :: Parser ()
intLit = void $ many1 digit

doubleLit :: Parser ()
doubleLit = void $ many1 digit >> char '.' >> many digit

charLit :: Parser ()
charLit = void $ between (char '\'') (char '\'') (void $ satisfy (/= '\''))

lit :: Parser ()
lit = choice (map try [stringLit, charLit, doubleLit, intLit])

variable :: Parser ()
variable =
  void . (>>= checkReserved) $
    variableName <$> many digit <*> letter <*> many alphaNum
  where
    variableName pref l post = pref <> [l] <> post
    checkReserved :: String -> Parser ()
    checkReserved varName = when (varName `elem` reservedKeywords) $ fail ("Variable name \"" <> varName <> "\" is a reserved keyword.")
    reservedKeywords = ["Assert", "Info", "IsBound", "QUIT", "TryNextMethod", "Unbind", "and", "atomic", "break", "continue", "do", "elif", "else", "end", "false", "fi", "for", "function", "if", "in ", "local", "mod", "not", "od", "or", "quit", "readonly", "readwrite", "rec", "repeat", "return", "then", "true", "until", "while"]

stoken :: String -> Parser ()
stoken t = void $ spaces >> string t >> spaces

recordAccess :: Parser ()
recordAccess = void $ sepBy1 variable (try $ stoken ".") >> optional (try $ between (stoken "[") (stoken "]") expression)

commaSep :: Parser () -> Parser ()
commaSep p = void $ sepBy p (stoken ",")

lambda :: Parser ()
lambda = void $ variable >> stoken "->" >> expression

bracketList :: Parser () -> Parser ()
bracketList p = stoken "(" >> commaSep p >> stoken ")"

functionCall :: Parser ()
functionCall = void $ variable >> bracketList expression

permutation :: Parser ()
permutation = void . many1 $ bracketList expression

record :: Parser ()
record = void $ stoken "rec" >> bracketList assignment

list :: Parser ()
list = void $ between (stoken "[") (stoken "]") (sepBy expression (stoken ","))

binOp :: Parser ()
binOp = void $ chainl1 (try binOpExpression) (try op >> return (const $ const ()))
  where
    op = spaces >> choice (map (try . string) [">=", "<=", "=", "<>", "<", ">", "in", "+", "-", "/", "mod", "^", "and", "*", "or"]) >> spaces

bracketedExpr :: Parser ()
bracketedExpr = between (stoken "(") (stoken ")") expression

binOpExpression :: Parser ()
binOpExpression = choice (map try [lit, lambda, functionCall, permutation, record, list, recordAccess, bracketedExpr])

functionDef :: Parser ()
functionDef = void $ stoken "function" >> bracketList variable >> optional (try localDeclare) >> spaces >> functionStatements >> stoken "end"
  where
    localDeclare = stoken "local" >> commaSep variable >> spaces >> many (char ';')

expression = choice (map try [binOp, functionDef, lambda, lit, recordAccess, functionCall, permutation, record, list])

loopStatements :: Parser ()
loopStatements = void $ many (try loopStatement)

comment :: Parser ()
comment = void $ stoken "#" >> manyTill anyChar (try $ void (char '\n') <|> eof)

assignment :: Parser ()
assignment = void $ variable >> stoken ":=" >> expression

for :: Parser ()
for = void $ stoken "for" >> variable >> stoken "in" >> expression >> stoken "do" >> statements >> stoken "od"

while :: Parser ()
while = void $ stoken "while" >> expression >> stoken "do" >> loopStatements >> stoken "od"

repeatStmt :: Parser ()
repeatStmt = void $ stoken "repeat" >> loopStatements >> stoken "until" >> expression

ifStmt :: Parser ()
ifStmt = void . (<?> "if statement") $ stoken "if" >> expression >> stoken "then" >> statements >> many (try elif) >> optional (try elseStmt) >> stoken "fi"
  where
    elif = stoken "elif" >> expression >> stoken "then" >> statements
    elseStmt = stoken "else" >> statements

statement :: Parser ()
statement = ((<?> "statement") . choice) $ map (try . (\p -> spaces >> p >> spaces >> many1 (char ';') >> spaces)) [ifStmt, expression, assignment, for, while, repeatStmt] <> [try comment]

functionStatement :: Parser ()
functionStatement = choice $ map (try . (\p -> spaces >> p >> spaces >> many1 (char ';') >> spaces)) [ifStmt, expression, assignment, for, while, repeatStmt, returnStmt] <> [try comment]

functionStatements :: Parser ()
functionStatements = void $ many (try functionStatement)

loopStatement :: Parser ()
loopStatement = choice $ map (try . (\p -> spaces >> p >> spaces >> many1 (char ';') >> spaces)) [ifStmt, expression, assignment, for, while, repeatStmt, breakStmt, continue] <> [try comment]

statements :: Parser ()
statements = void $ many (try statement)

returnStmt :: Parser ()
returnStmt = stoken "return" >> expression

breakStmt :: Parser ()
breakStmt = void $ stoken "break"

continue :: Parser ()
continue = void $ stoken "continue"
