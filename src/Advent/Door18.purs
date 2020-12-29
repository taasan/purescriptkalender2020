module Advent.Door18 (open) where

import Prelude hiding (between)
import Advent.Lib (lines, (*>+), (<$?>))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..), hush, isLeft)
import Data.Foldable (find, sum)
import Data.Identity (Identity)
import Data.List (List(..), many, (:))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Text.Parsing.Parser (ParseError, Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy1, sepEndBy1)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), OperatorTable, buildExprParser)
import Text.Parsing.Parser.String (char, oneOf, string)
import Text.Parsing.Parser.Token (GenLanguageDef(..), makeTokenParser)

{- https://adventofcode.com/2020/day/18

--- Day 18: Operation Order ---

The rules of operator precedence have changed. Rather than evaluating
multiplication before addition, the operators have the same
precedence, and are evaluated left-to-right regardless of the order in
which they appear.

Evaluate the expression on each line of the homework; what is the sum
of the resulting values?

--- Part Two ---

Now, addition and multiplication have different precedence levels, but
they're not the ones you're familiar with. Instead, addition is
evaluated before multiplication.

What do you get if you add up the results of evaluating the homework
problems using these new rules?

-}
open ∷ String → Either String String
open input = evaluation
  where
  evaluation =
    if isLeft part1 || isLeft part2 then
      Left $ show $ [ part1, part2 ]
    else
      Right $ show $ hush <$?> [ part1, part2 ]

  part1 = case runParser input $ operatorList `sepEndBy1` char '\n' of
    Right xs → pure $ sum $ eval <$?> xs
    Left err → Left err

  part2 =
    parse
      [ [ Infix add AssocLeft ]
      , [ Infix mul AssocLeft ]
      ]

  add = string "+" $> (+)

  mul = string "*" $> (*)

  parse table = case find isLeft parsed of
    Just x → x
    _ → pure $ sum $ hush <$?> parsed
    where
    xs ∷ List String
    xs = lines $ trim input

    parsed ∷ List (Either ParseError Number)
    parsed = flip runParser (expressionParser table) <$> xs

-- Kun tallene fra 1 til 9 fins i input
number ∷ Parser String Number
number =
  (string "1" *>+ 1.0)
    <|> (string "2" *>+ 2.0)
    <|> (string "3" *>+ 3.0)
    <|> (string "4" *>+ 4.0)
    <|> (string "5" *>+ 5.0)
    <|> (string "6" *>+ 6.0)
    <|> (string "7" *>+ 7.0)
    <|> (string "8" *>+ 8.0)
    <|> (string "9" *>+ 9.0)

{--- PART 1 ---

Bruker en stakk og tar unna etter hvert.

-}
data Expr
  = Add
  | Mul
  | Paren (List Expr)
  | Num Number

operatorList ∷ Parser String (List Expr)
-- Lazy.fix for rekursiv parsing
operatorList = fix \_ → operator `sepBy1` many (char ' ')

operator ∷ Parser String Expr
operator = add <|> mul <|> num <|> paren
  where
  add = string "+" *>+ Add

  mul = string "*" *>+ Mul

  num = Num <$> number

  -- Lazy.fix for rekursiv parsing
  paren = Paren <$> fix \_ → between (char '(') (char ')') operatorList

eval ∷ List Expr → Maybe Number
eval (Num x : Nil) = Just x

eval (Paren xs : Nil) = eval xs

eval (x : op : y : zs) = do
  let
    op' Add = Just (+)

    op' Mul = Just (*)

    op' _ = Nothing
  f ← op' op
  y' ← eval $ y : Nil
  x' ← eval $ x : Nil
  eval $ Num (x' `f` y') : zs -- Evaluer forfra

eval _ = do
  Nothing

{--- Bygger parser med Text.Parsing.Parser.Expr.OperatorTable

-}
expressionParser ∷ (OperatorTable Identity String Number) → Parser String Number
expressionParser table = buildExprParser table expr
  where
  expr =
    fix \p → do -- Må bruke fix (Lazy) for rekursiv parsing
      void $ many (char ' ') -- Fjern ledende mellomrom. Trengs bare her.
      tokenParser.parens (buildExprParser table p)
        <|> tokenParser.lexeme number

  tokenParser = makeTokenParser languageDef

  languageDef =
    LanguageDef
      { commentStart: ""
      , commentEnd: ""
      , commentLine: ""
      , nestedComments: false
      , identStart: fail "identStart"
      , identLetter: fail "identLetter"
      , opStart: oneOf [ '+', '*' ]
      , opLetter: fail "opLetter"
      , reservedNames: []
      , reservedOpNames: []
      , caseSensitive: true
      }
