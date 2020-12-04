module Advent.Door1 (open) where

import Prelude
import Advent.Lib (choose, lines)
import Data.Foldable (find, product, sum)
import Data.Int (fromString)
import Data.List (mapMaybe)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)

{- https://adventofcode.com/2020/day/1

--- Part One ---
Before you leave, the Elves in accounting just need you to fix your
expense report (your puzzle input); apparently, something isn't quite
adding up.

Specifically, they need you to find the two entries that sum to 2020
and then multiply those two numbers together.

For example, suppose your expense report contained the following:

    1721
    979
    366
    299
    675
    1456

In this list, the two entries that sum to 2020 are 1721 and
299. Multiplying them together produces 1721 * 299 = 514579, so the
correct answer is 514579.

--- Part Two ---

The Elves in accounting are thankful for your help; one of them even
offers you a starfish coin they had left over from a past
vacation. They offer you a second one if you can find three numbers in
your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are
979, 366, and 675. Multiplying them together produces the answer,
241861950.

In your expense report, what is the product of the three entries that
sum to 2020?
-}
open :: String -> Maybe String
open input = (sequence $ calculate <$> [ 2, 3 ]) >>= pure <<< show
  where
  calculate n = do
    ys <- find ((_ == 2020) <<< sum) $ choose n xs
    pure $ product ys

  xs = mapMaybe fromString $ lines input
