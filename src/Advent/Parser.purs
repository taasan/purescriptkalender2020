module Advent.Parser where

import Prelude hiding (between, when)
import Advent.Lib (fromCharList, (∘))
import Data.Array (foldMap)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Int (fromString)
import Data.List (List, many, some, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as S
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.String (char, oneOf, satisfy)

type Parser
  = P.Parser String

fromChars ∷ ∀ f. Foldable f ⇒ f Char → String
fromChars = foldMap S.singleton

someChar ∷ Parser Char → Parser String
someChar p = fromChars <$> some p

word ∷ Parser String
word = someChar (satisfy (not ∘ (_ == ' ')))

words ∷ Parser (List String)
words = word `sepBy` some (char ' ')

unsigned ∷ Parser Int
unsigned = do
  n ← oneOf digitsNotZero
  ns ← many (oneOf digits)
  case fromString $ fromCharList (n : ns) of
    Just x → pure x
    _ → fail "Invalid number"

digitsNotZero ∷ Array Char
digitsNotZero = [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

digits ∷ Array Char
digits = Array.cons '0' digitsNotZero
