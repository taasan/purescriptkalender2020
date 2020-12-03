module Advent.Lib where

import Prelude
import Data.List (List(..), fromFoldable, (:))
import Data.String (split, Pattern(..))

lines :: String -> List String
lines = fromFoldable <<< split (Pattern "\n")

{-
Stolen from https://hackage.haskell.org/package/combinat-0.2.9.0/docs/src/Math.Combinat.Sets.html#choose
-}
-- | All possible ways to choose @k@ elements from a list, without
-- repetitions. \"Antisymmetric power\" for lists. Synonym for 'kSublists'.
choose :: forall a. Int -> List a -> List (List a)
choose 0 _ = Nil : Nil

choose k (x : xs) = map ((:) x) (choose (k - 1) xs) <> choose k xs

choose _ _ = Nil
