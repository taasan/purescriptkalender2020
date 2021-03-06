module Advent.Door6 (open) where

import Prelude
import Advent.Lib (fromFoldable, intersections, (∘))
import Data.Either (Either)
import Data.Foldable (sum)
import Data.Set (size)
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (toCharArray)

{- https://adventofcode.com/2020/day/6

--- Day 6: Custom Customs ---

The form asks a series of 26 yes-or-no questions marked a through
z. All you need to do is identify the questions for which anyone in
the group answers "yes". For each of the people in their group, you
write down the questions for which they answer "yes", one per
line. For example:

abcx
abcy
abcz

In this group, there are 6 questions to which anyone answered "yes":
a, b, c, x, y, and z. (Duplicate answers to the same question don't
count extra; each question counts at most once.)

For each group, count the number of questions to which anyone answered
"yes". What is the sum of those counts?

--- Part Two ---

As you finish the last group's customs declaration, you notice that
you misread one word in the instructions:

You don't need to identify the questions to which anyone answered
"yes"; you need to identify the questions to which everyone answered
"yes"!

For each group, count the number of questions to which everyone
answered "yes". What is the sum of those counts?
-}
open ∷ String → Either String String
open input = (pure ∘ show) answers
  where
  -- join is for part 1, intersections is for part 2
  answers = sum ∘ for groups ∘ collector <$> [ join, intersections ]

  for = flip map

  -- use Set to remove duplicates
  collector f = size ∘ fromFoldable ∘ f

  groups ∷ Array (Array (Array Char))
  groups =
    ((map toCharArray) ∘ split (Pattern "\n"))
      <$> split (Pattern "\n\n") (trim input)
