module Output where

import Control.Applicative (many, optional, some, (<|>))
import Instances (ParseResult (..), Parser (..))
import Parser
    ( alpha
    , charTok
    , eof
    , int
    , is
    , string
    , stringTok
    , tok
    )
-- only importing some things from prelude to minimise conflicts with builtins
import Prelude (Char, Int, Maybe, Show, String, show, (<$>), (<*), (<*>))

runParser :: Show a => Parser a -> String -> String
runParser p s = case parse (p <* eof) s of
    Result _ a -> show a
    Error _ -> "Parse Error"

data Program = Program1 Stmt Program
             | Program2 Stmt
    deriving Show

data Stmt = Stmt1 Ternary
          | Stmt2 Assign
    deriving Show

data Ternary = Ternary1 Var String Stmt String Stmt
    deriving Show

data Assign = Assign1 Var String Expr String
    deriving Show

data Expr = Expr1 Term ExprTail
    deriving Show

data ExprTail = ExprTail1 String Term ExprTail
              | ExprTail2 String
    deriving Show

data Term = Term1 Var
          | Term2 Num
    deriving Show

newtype Var = Var String
    deriving Show

newtype Num = Num Int
    deriving Show

program :: Parser Program
program = Program1 <$> stmt <*> program
        <|> Program2 <$> stmt

stmt :: Parser Stmt
stmt = Stmt1 <$> ternary
     <|> Stmt2 <$> assign

ternary :: Parser Ternary
ternary = Ternary1 <$> var <*> (string "?") <*> stmt <*> (string ":") <*> stmt

assign :: Parser Assign
assign = Assign1 <$> var <*> (string "=") <*> expr <*> (string ";")

expr :: Parser Expr
expr = Expr1 <$> term <*> exprTail

exprTail :: Parser ExprTail
exprTail = ExprTail1 <$> (string "+") <*> term <*> exprTail
         <|> ExprTail2 <$> (string "")

term :: Parser Term
term = Term1 <$> var
     <|> Term2 <$> num

var :: Parser Var
var = Var <$> (some alpha)

num :: Parser Num
num = Num <$> int
