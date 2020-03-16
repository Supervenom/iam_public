-- Adapted from http://dev.stephendiehl.com/fun/lambda_calculus.html

module Parser where

import Data.Char
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

type Name = [Char]

data Term = Var Name | Abs Name Term | App Term Term | Es Term Name Term deriving Eq


instance Show Term where
    show (Var x) = x
    show (Abs y x) = "λ" ++ y ++ "." ++ (show x)
    show (App (Var x) (Var y)) = x ++ y
    show (App x (Var y)) = "(" ++ (show x) ++ ")" ++ y
    show (App x y) = "(" ++ (show x) ++ ")" ++ "(" ++ (show y) ++ ")"
    show (Es x y z) = "(" ++ (show x) ++ ")" ++ "[" ++ y ++ "←" ++ (show z) ++ "]"

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["\\","<-"]
        names = []
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

square :: Parser a -> Parser a
square = Tok.brackets lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

variable :: Parser Term
variable = do
  x <- identifier
  return (Var x)

lambda :: Parser Term
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "."
  body <- expr
  return $ foldr Abs body args


es :: Parser Term
es = do
  term1 <- expr
  reservedOp ","
  arg <- identifier
  reservedOp "<-"
  body <- expr
  return (Es term1 arg body)


term :: Parser Term
term =  parens expr
    <|> variable
    <|> lambda
    <|> square es

expr :: Parser Term
expr = do
  es <- many1 term
  return (foldl1 App es)

parseExpr :: String -> Either ParseError Term
parseExpr input = parse (contents expr) "" input
