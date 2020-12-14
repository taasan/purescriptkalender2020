module Advent.Door9 (open) where

import Prelude
import Advent.Lib (lines, (!!), (<$?>), (∘))
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (all, maximum, minimum, notElem, sum)
import Data.List (List(..), take, (:))
import Data.Maybe (Maybe(..), isJust)
import Data.String (trim)
import Data.Tuple (Tuple(..), uncurry)
import Global (readFloat)

{- https://adventofcode.com/2020/day/9

--- Day 9: Encoding Error ---

35  |
20  |
15  |  Preamble
25  |
47  |
-----
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576

In this example, after the 5-number preamble, almost every number is
the sum of two of the previous 5 numbers; the only number that does
not follow this rule is 127.

Find the first number in the list (after the preamble) which is not
the sum of two of the 25 numbers before it. What is the first number
that does not have this property?

--- Part Two ---

You must find a contiguous set of at least two numbers in your list
which sum to the invalid number from step 1.

Add together the smallest and largest number in this contiguous range.

-}
open ∷ String → Either String String
open input = (evaluation ∘ show) $ identity <$?> answer
  where
  preambleLength = 25

  answer = [ part1, part2 ]

  evaluation = if all isJust answer then Right else Left

  numbers = readFloat <$> (lines ∘ trim) input

  part1 = go numbers
    where
    preambleSums = sums preambleLength ∘ take preambleLength

    sums ∷ Int → List Number → List Number
    sums len xs = uncurry (+) <$> pairs len xs

    pairs ∷ Int → List Number → List (Tuple Number Number)
    pairs len xs = filter (uncurry (/=)) $ Tuple <$> xs' <*> xs'
      where
      xs' = take len xs

    go Nil = Nothing

    go all@(_ : ns) = do
      x <- all !! preambleLength
      if x `notElem` (preambleSums all) then
        pure x
      else
        go ns

  part2 = do
    needle <- part1
    list <- search 2 needle numbers
    mx <- maximum list
    mn <- minimum list
    pure $ mn + mx
    where
    consecutives _ Nil = Nil

    consecutives 0 _ = Nil

    consecutives n (x : xs) = take n xs : consecutives (n - 1) xs

    search _ _ Nil = Nothing

    search n needle xs = case go $ consecutives n xs of
      Just zs → pure zs -- Done
      _ → search (n + 1) needle xs
      where
      go Nil = Nothing

      go (ys : yys) =
        if sum ys == needle then
          pure ys -- Done
        else
          go yys
