module Advent.Parser where

import Prelude hiding (zero)
import Text.Parsing.Parser.Combinators (optional, sepBy)
import Advent.Lib (fromCharList, (∘))
import Control.Alternative ((<|>))
import Data.Array (foldMap)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Int (fromString)
import Data.List (List, many, some, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as S
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser as P
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

zero ∷ Parser Int
zero = char '0' $> 0

integer ∷ Parser Int
integer =
  (*)
    <$> ((char '-' $> -1) <|> (optional (char '+') $> 1))
    <*> unsigned

unsigned ∷ Parser Int
unsigned = natural <|> zero

natural ∷ Parser Int
natural = do
  n ← oneOf digitsNotZero
  ns ← many (oneOf digits)
  case fromString $ fromCharList (n : ns) of
    Just x → pure x
    _ → fail "Invalid number"

digitsNotZero ∷ Array Char
digitsNotZero = [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

digits ∷ Array Char
digits = Array.cons '0' digitsNotZero
