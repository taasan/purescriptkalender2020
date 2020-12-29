module Advent.Door18 (open) where

import Prelude
import Advent.Lib ((*>+), (<$?>), (∘))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..), hush, isLeft)
import Data.Foldable (any, sum)
import Data.Identity (Identity)
import Data.List (many)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), OperatorTable, buildExprParser)
import Text.Parsing.Parser.String (char, eof, oneOf, string)
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
    if any isLeft solution then
      (Left ∘ show) solution
    else
      (Right ∘ show) (hush <$?> solution)

  solution =
    map parse
      [ [ [ Infix (add <|> mul) AssocLeft ] ] -- Part 1
      , [ [ Infix add AssocLeft ], [ Infix mul AssocLeft ] ] -- Part 2
      ]
    where
    add = string "+" $> (+)

    mul = string "*" $> (*)

  parse operatorTable = case runParser input parser of
    Right xs → (Right ∘ sum) xs
    Left err → Left err
    where
    parser = expressionParser operatorTable `many1Till` eof

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
