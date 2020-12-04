module Advent.Lib where

import Prelude
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, indexl)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.String (split, Pattern(..))

lines :: String -> Array String
lines = split (Pattern "\n")

{-
Stolen from https://hackage.haskell.org/package/combinat-0.2.9.0/docs/src/Math.Combinat.Sets.html#choose
-}
-- | All possible ways to choose @k@ elements from a list, without
-- repetitions. \"Antisymmetric power\" for lists. Synonym for 'kSublists'.
choose :: forall a. Int -> List a -> List (List a)
choose 0 _ = Nil : Nil

choose k (x : xs) = map ((:) x) (choose (k - 1) xs) <> choose k xs

choose _ _ = Nil

head :: forall a f. Foldable f => f a -> Maybe a
head = indexl 0

-- | Generic version of index
index :: forall a f. Foldable f => f a -> Int -> Maybe a
index = flip indexl

infixl 8 index as !!

infixl 4 filterMap as <$$>
