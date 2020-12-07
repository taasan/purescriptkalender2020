module Advent.Door5 (open) where

{- https://adventofcode.com/2020/day/5

--- Day 5: Binary Boarding ---

A seat might be specified like FBFBBFFRLR, where F means "front", B
means "back", L means "left", and R means "right".

The first 7 characters will either be F or B; these specify exactly
one of the 128 rows on the plane (numbered 0 through 127). Each letter
tells you which half of a region the given seat is in. Start with the
whole list of rows; the first letter indicates whether the seat is in
the front (0 through 63) or the back (64 through 127). The next letter
indicates which half of that region the seat is in, and so on until
you're left with exactly one row.

The last three characters will be either L or R; these specify exactly
one of the 8 columns of seats on the plane (numbered 0 through 7). The
same process as above proceeds again, this time with only three
steps. L means to keep the lower half, while R means to keep the upper
half.

As a sanity check, look through your list of boarding passes. What is
the highest seat ID on a boarding pass?

--- Part Two ---


It's a completely full flight, so your seat should be the only missing
boarding pass in your list. However, there's a catch: some of the
seats at the very front and back of the plane don't exist on this
aircraft, so they'll be missing from your list as well.

Your seat wasn't at the very front or back, though; the seats with IDs
+1 and -1 from yours will be in your list.

What is the ID of your seat?
-}
import Prelude
import Advent.Lib ((*>+), (∘))
import Advent.Parser (Parser)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (foldl, maximum, minimum)
import Data.List (List(..), difference, drop, length, many, take, (..), (:))
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicateA)
import Text.Parsing.Parser (ParseError(..), fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (sepEndBy)
import Text.Parsing.Parser.Pos (initialPos)
import Text.Parsing.Parser.String (char)

open ∷ String → Either String String
open input = do
  case doOpen of
    Right x → pure x
    Left err → Left $ parseErrorMessage err
  where
  doOpen ∷ Either ParseError String
  doOpen = do
    xs ← runParser input positions
    let
      seats = seat <$> xs
    maxSeat ← fromMaybe "maxSeat" $ maximum seats
    minSeat ← fromMaybe "minSeat" $ minimum seats
    let
      availableSeats = difference (minSeat .. maxSeat) seats
    mySeat ← case availableSeats of
      (x : Nil) → pure x
      _ → fromMaybe "Expected singleton list for mySeat" $ Nothing
    (pure ∘ show) [ maxSeat, mySeat ]
    where
    seat { row, col } = row * 8 + col

  fromMaybe ∷ ∀ a. String → Maybe a → Either ParseError a
  fromMaybe reason Nothing = Left $ ParseError reason $ initialPos

  fromMaybe _ (Just x) = pure x

data Partition
  = First
  | Last

type Position
  = { row ∷ Int, col ∷ Int }

rowPartition ∷ Parser Partition
rowPartition =
  (char 'F' *>+ First)
    <|> (char 'B' *>+ Last)

colPartition ∷ Parser Partition
colPartition =
  (char 'L' *>+ First)
    <|> (char 'R' *>+ Last)

position ∷ Parser Position
position = do
  row ← replicateA 7 rowPartition >>= getPos (0 .. 127)
  col ← replicateA 3 colPartition >>= getPos (0 .. 7)
  pure $ { col, row }
  where
  getPos ∷ List Int → List Partition → Parser Int
  getPos ys ps = f $ foldl partition ys ps
    where
    -- We should have a singleton here
    f (x : Nil) = pure x

    f _ = fail "Expected a singleton list"

  partition ∷ List Int → Partition → List Int
  partition seats p = (f p) n seats
    where
    n = (length seats) / 2

    f First = take

    f _ = drop

positions ∷ Parser (List Position)
positions = position `sepEndBy` many (char '\n')
