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
import Advent.Lib (lines, (<$$>))
import Data.Array (difference, drop, head, length, take, (..))
import Data.Foldable (foldl, maximum, minimum)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)

open :: String -> Maybe String
open input = do
  maxSeat <- maximum seats
  minSeat <- minimum seats
  let
    availableSeats = difference (minSeat .. maxSeat) seats
  mySeat <- head availableSeats
  (pure <<< show) [ maxSeat, mySeat ]
  where
  seats = ((map seat) <<< findPosition <<< toCharArray) <$$> lines input

  seat { row, col } = row * 8 + col

data Partition
  = First
  | Last

type Position
  = { row :: Int, col :: Int }

findPosition :: Array Char -> Maybe Position
findPosition xs = do
  row <- verifyLength 7 (parseRow <$$> take 7 xs) >>= getPos (0 .. 127)
  col <- verifyLength 3 (parseCol <$$> drop 7 xs) >>= getPos (0 .. 7)
  pure { col, row }
  where
  getPos :: Array Int -> Array Partition -> Maybe Int
  getPos ys ps = f $ foldl partition ys ps
    where
    -- We should have a singleton here
    f [ x ] = pure x

    f _ = Nothing

  verifyLength :: forall a. Int -> Array a -> Maybe (Array a)
  verifyLength n ys = if length ys /= n then Nothing else pure ys

  partition :: Array Int -> Partition -> Array Int
  partition seats p = (f p) n seats
    where
    n = (length seats) / 2

    f First = take

    f _ = drop

  parseRow 'F' = pure First

  parseRow 'B' = pure Last

  parseRow _ = Nothing

  parseCol 'L' = pure First

  parseCol 'R' = pure Last

  parseCol _ = Nothing
