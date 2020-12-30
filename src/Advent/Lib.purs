module Advent.Lib where

import Prelude
import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, foldl, indexl)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Regex (split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

lines ∷ ∀ f. FromFoldable Array String f ⇒ String → f String
lines str = fromFoldable $ unsafeSplit (Pattern "\n") str

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

head ∷ ∀ a f. Foldable f ⇒ f a → Maybe a
head = indexl 0

-- | Generic version of index
index ∷ ∀ a f. Foldable f ⇒ f a → Int → Maybe a
index = flip indexl

infixl 8 index as !!

infixl 4 filterMap as <$?>

unsafeSplit ∷ Pattern → String → Array String
unsafeSplit (Pattern pattern) = split re
  where
  re = unsafeRegex pattern noFlags

fromCharList ∷ List Char → String
fromCharList = fromCharArray ∘ fromFoldable

-- | Intersection of multiple foldables
intersections ∷
  ∀ a f g.
  Ord a ⇒
  Bind f ⇒
  Foldable f ⇒
  FromFoldable f a g ⇒
  FromFoldable Set a f ⇒
  f (f a) → g a
intersections xs = fromFoldable $ foldl intersect (join xs) xs

intersect ∷
  ∀ f1 f2 f3 a.
  Ord a ⇒ Foldable f1 ⇒ Foldable f2 ⇒ FromFoldable Set a f3 ⇒ f1 a → f2 a → f3 a
intersect xs ys = fromFoldable $ Set.intersection xs' ys'
  where
  xs' = Set.fromFoldable xs

  ys' = Set.fromFoldable ys

class FromFoldable f a g where
  fromFoldable ∷ Foldable f ⇒ f a → g a

instance fromFoldableIdentity ∷ FromFoldable f a f where
  fromFoldable = identity
else instance fromFoldableArray ∷ FromFoldable f a Array where
  fromFoldable = Array.fromFoldable
else instance fromFoldableList ∷ FromFoldable f a List where
  fromFoldable = List.fromFoldable
else instance fromFoldableLazyList ∷ FromFoldable f a LazyList.List where
  fromFoldable = LazyList.fromFoldable
else instance fromFoldableSet ∷ Ord a ⇒ FromFoldable f a Set where
  fromFoldable = Set.fromFoldable

inRange ∷ ∀ a. Ord a ⇒ a → a → a → Boolean
inRange a b x
  | a > b = inRange b a x

inRange a b x = x >= a && x <= b

infixl 4 inRange as ..?

infixr 9 compose as ∘

mapAfterMap ∷ ∀ g b a f. Functor f ⇒ Functor g ⇒ (a → b) → f (g a) → f (g b)
mapAfterMap = map ∘ map

infixl 4 mapAfterMap as <<$>>
