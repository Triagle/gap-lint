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

recordAccess :: Parser ()
recordAccess = void $ sepBy1 variable (try $ char '.' >> spaces >> char '.') >> optional (try $ between (spaces >> char '[' >> spaces) (spaces >> char ']' >> spaces) intLit)

commaSep :: Parser () -> Parser ()
commaSep p = void $ sepBy p (spaces >> char ',' >> spaces)

lambda :: Parser ()
lambda = void $ variable >> spaces >> string "->" >> spaces >> expression

functionCall :: Parser ()
functionCall = void $ variable >> spaces >> char '(' >> sepBy expression (spaces >> char ',' >> spaces) >> char ')'

permutation :: Parser ()
permutation = void . many1 $ spaces >> char '(' >> sepBy expression (spaces >> char ',' >> spaces) >> char ')' >> spaces

record :: Parser ()
record = void $ string "rec" >> spaces >> char '(' >> spaces >> sepBy1 assignment (spaces >> char ',' >> spaces) >> spaces >> char ')'

list :: Parser ()
list = void $ between (char '[') (char ']') (sepBy expression (spaces >> char ',' >> spaces))

binOp :: Parser ()
binOp = void $ chainl1 (try binOpExpression) (try op >> return (const $ const ()))
  where
    op = spaces >> choice (map (try . string) [">=", "<=", "=", "<>", "<", ">", "in", "+", "-", "/", "mod", "^", "and", "*", "or"]) >> spaces

bracketedExpr :: Parser ()
bracketedExpr = between (char '(') (char ')') expression

binOpExpression :: Parser ()
binOpExpression = choice (map try [lit, lambda, functionCall, permutation, record, list, recordAccess, bracketedExpr])

functionDef :: Parser ()
functionDef = void $ string "function" >> between (spaces >> char '(' >> spaces) (spaces >> char ')' >> spaces) (commaSep variable) >> optional (try localDeclare) >> spaces >> functionStatements >> spaces >> string "end"
  where
    localDeclare = spaces >> string "local" >> spaces >> commaSep variable >> spaces >> many (char ';')

expression = choice (map try [binOp, functionDef, lambda, lit, recordAccess, functionCall, permutation, record, list])

loopStatements :: Parser ()
loopStatements = void $ many (try loopStatement)

comment :: Parser ()
comment = void $ spaces >> char '#' >> spaces >> manyTill anyChar (try $ void (char '\n') <|> eof)

assignment :: Parser ()
assignment = void $ variable >> spaces >> string ":=" >> spaces >> expression

for :: Parser ()
for = void $ string "for" >> spaces >> variable >> spaces >> string "in" >> spaces >> expression >> spaces >> string "do" >> spaces >> statements >> spaces >> string "od"

while :: Parser ()
while = void $ string "while" >> spaces >> expression >> spaces >> string "do" >> spaces >> loopStatements >> spaces >> string "od"

repeatStmt :: Parser ()
repeatStmt = void $ string "repeat" >> spaces >> loopStatements >> spaces >> string "until" >> spaces >> expression

ifStmt :: Parser ()
ifStmt = void . (<?> "if statement") $ string "if" >> spaces >> expression >> spaces >> string "then" >> spaces >> statements >> many (try elif) >> optional (try elseStmt) >> spaces >> string "fi"
  where
    elif = spaces >> string "elif" >> spaces >> expression >> spaces >> string "then" >> spaces >> statements
    elseStmt = spaces >> string "else" >> spaces >> statements

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
returnStmt = string "return" >> spaces >> expression

breakStmt :: Parser ()
breakStmt = void $ string "break"

continue :: Parser ()
continue = void $ string "continue"
