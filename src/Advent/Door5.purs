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
import Advent.Lib (lines, (∘), (<$?>))
import Data.Either (Either(..))
import Data.Foldable (maximum, minimum)
import Data.Int (binary, fromStringAs)
import Data.List (List(..), difference, (..), (:))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String as String

open ∷ String → Either String String
open input = do
  case doOpen of
    Just x → pure x
    _ → Left "No answer for you!"
  where
  xs = parseLine <$?> lines input

  parseLine l = case parseRow l, parseCol l of
    Just row, Just col -> Just { row, col }
    _, _ -> Nothing

  parseRow = parse String.take "F" "B"

  parseCol = parse String.drop "L" "R"

  parse f off on = fromStringAs binary ∘ toBinaryDigits f off on

  toBinaryDigits f off on = f 7 ∘ replaceAll (Pattern off) zero ∘ replaceAll (Pattern on) one

  zero = Replacement "0"

  one = Replacement "1"

  doOpen ∷ Maybe String
  doOpen = do
    let
      seats = seat <$> xs
    maxSeat ← maximum seats
    minSeat ← minimum seats
    let
      availableSeats = difference (minSeat .. maxSeat) seats
    mySeat ← case availableSeats of
      (x : Nil) → pure x
      _ → Nothing
    (pure ∘ show) [ maxSeat, mySeat ]
    where
    seat { row, col } = row * 8 + col

type Position
  = { row ∷ Int, col ∷ Int }
