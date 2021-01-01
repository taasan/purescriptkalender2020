module Advent.Door1 (open) where

import Prelude
import Advent.Lib ((<$?>), (∘), lines)
import Data.Either (Either)
import Data.Foldable (find, product, sum)
import Data.Int (fromString)
import Data.List (List(..), (:))

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
open ∷ String → Either String String
open input = pure $ show $ calculate <$?> [ 2, 3 ]
  where
  calculate n = do
    ys ← find ((_ == 2020) ∘ sum) $ choose n xs
    pure $ product ys

  xs = fromString <$?> lines input

-- | All possible ways to choose `k` elements from a list, without
-- | repetitions. "Antisymmetric power" for lists. Synonym for `kSublists`.
-- |
-- | Stolen from https://hackage.haskell.org/package/combinat-0.2.9.0/docs/src/Math.Combinat.Sets.html#choose
choose ∷ ∀ a. Int → List a → List (List a)
choose 0 _ = Nil : Nil

choose _ Nil = Nil

choose k _
  | k < 1 = Nil

choose k (x : xs) = map ((:) x) (choose (k - 1) xs) <> choose k xs
