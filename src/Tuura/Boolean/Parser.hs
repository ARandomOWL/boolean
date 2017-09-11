{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tuura.Boolean.Parser (
    Expr (..),
    parseExpr, parseWrapper, simplify, partialEval) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Prelude hiding (not, or, and)

data Expr a = Val Bool
          | Var a
          | Not (Expr a)
          | And (Expr a) (Expr a)
          | Or (Expr a) (Expr a)
          | SubExpr (Expr a)
            deriving (Functor, Foldable, Traversable, Show, Eq)

instance Applicative Expr where
    pure  = Var
    (<*>) = ap

instance Monad Expr where
    return = pure

    Val bool  >>= _ = Val bool
    Var a     >>= f = f a
    Not x     >>= f = Not     (x >>= f)
    And x y   >>= f = And     (x >>= f) (y >>= f)
    Or  x y   >>= f = Or      (x >>= f) (y >>= f)
    SubExpr x >>= f = SubExpr (x >>= f)

partialEval :: (a -> Maybe Bool) -> Expr a -> Expr a
partialEval f expr = expr >>= substitute
  where
    substitute x = case f x of
        Just bool -> Val bool
        Nothing   -> Var x

simplify :: Eq a => Expr a -> Expr a
simplify (Val bool)            = Val bool
simplify (Var x)               = Var x
simplify (Not x)               = Not x
simplify (And (Val x) (Val y)) = Val $ x && y
simplify (And (Val x) y      ) = if' x        y (Val False)
simplify (And x       (Val y)) = if' y        x (Val False)
simplify (And x       y      ) = if' (x == y) x (And x y)
simplify (Or  (Val x) (Val y)) = Val $ x || y
simplify (Or  (Val x) y      ) = if' x        (Val True) y
simplify (Or  x       (Val y)) = if' y        (Val True) x
simplify (Or  x       y      ) = if' (x == y) x (Or x y)
simplify (SubExpr x)           = simplify x

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

parseExpr :: String -> Either ParseError (Expr String)
parseExpr = parse expr ""
  where expr = buildExpressionParser operators term <?> "compound expression"
        term = parens expr <|> variable <?> "full expr ession"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not),
                       Prefix (string "!" >> return Not),
                       Postfix (string "'" >> return Not)]
                    , [binary "AND" And, binary "*" And, binary "&" And],
                      [binary "OR" Or, binary "|" Or, binary "+" Or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var <$> (varParser <* spaces) <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces)
                           <?> "parens"
        varParser = liftM2 (++) (many1 letter) (many alphaNum)

parseWrapper :: String -> Expr String
parseWrapper expr =
  case parseExpr expr of
    Right x -> x
    Left _ -> error $ "Error parsing expression " ++ expr
