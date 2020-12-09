module Advent.Door7 (open) where

{- https://adventofcode.com/2020/day/7

--- Day 7: Handy Haversacks ---

Due to recent aviation regulations, many rules (your puzzle input) are
being enforced about bags and their contents; bags must be color-coded
and must contain specific quantities of other color-coded
bags.

For example, consider the following rules:

  light red bags contain 1 bright white bag, 2 muted yellow bags.
  dark orange bags contain 3 bright white bags, 4 muted yellow bags.
  bright white bags contain 1 shiny gold bag.
  muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
  shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
  dark olive bags contain 3 faded blue bags, 4 dotted black bags.
  vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
  faded blue bags contain no other bags.
  dotted black bags contain no other bags.

These rules specify the required contents for 9 bag types. In this
example, every faded blue bag is empty, every vibrant plum bag
contains 11 bags (5 faded blue and 6 dotted black), and so on.

How many bag colors can eventually contain at least one shiny gold
bag? (The list of rules is quite long; make sure you get all of it.)

--- Part Two ---

How many individual bags are required inside your single shiny gold bag?
-}
import Prelude
import Advent.Lib (fromFoldable, (<$?>), (∘))
import Advent.Parser (Parser, unsigned, word)
import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (any, sum)
import Data.List (List(..))
import Data.Map (Map, keys, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (sepEndBy)
import Text.Parsing.Parser.String (char, string)

type Color
  = String

type Content
  = { color ∷ Color
    , count ∷ Int
    }

type Rule
  = { color ∷ Color
    , content ∷ List Content
    }

data Branch a
  = Branch a (List (Branch a))

type Bag
  = Branch (Tuple Int Color)

instance showTree ∷ Show a ⇒ Show (Branch a) where
  show (Branch m xs) = "(Branch " <> show m <> show xs <> ")"

derive instance branchFunctor ∷ Functor Branch

mapValue ∷ ∀ a. (a → a) → Branch a → Branch a
mapValue f (Branch x xs) = Branch (f x) xs

open ∷ String → Either String String
open input = case bags input of
  Left err → Left $ show err
  Right bs → case lookup target rules of
    Nothing → Left $ show [ part1 ]
    Just filled → (pure ∘ show) [ part1, sumBranch $ fst <$> filled ]
    where
    rules = fillBags bs

    part1 = (Set.size ∘ canContain rules) target

    target = "shiny gold"

sumBranch ∷ Branch Int → Int
sumBranch (Branch _ Nil) = 1

sumBranch m = f m - 1 -- avoid beeing counted twice
  where
  f ∷ Branch Int → Int
  f (Branch _ Nil) = 1

  f (Branch _ xs) = ((1 + _) ∘ sum) $ f' <$> xs

  f' b@(Branch n _) = n * (f b)

bags ∷ String → Either String (Map Color Rule)
bags input = case parse of
  Left err → (Left ∘ parseErrorMessage) err
  Right m → (pure ∘ Map.fromFoldable) $ (\b@{ color } → Tuple color b) <$> m
  where
  parse = runParser input (bag `sepEndBy` char '\n')

canContain ∷ Map Color Bag → Color → Set Color
canContain rules color = (keys ∘ filter (contains color)) rules

contains ∷ Color → Bag → Boolean
contains _ (Branch _ Nil) = false

contains needle (Branch (Tuple _ color) _)
  | needle == color = false

contains needle m = f m
  where
  f ∷ Bag → Boolean
  f (Branch (Tuple _ c) xs) =
    if c == needle then
      true
    else
      any identity $ f <$> xs

fillBags ∷ Map Color Rule → Map Color Bag
fillBags rules = Map.fromFoldable xs
  where
  xs ∷ List (Tuple Color Bag)
  xs = (\c → fillBag c >>= pure ∘ Tuple c) <$?> fromFoldable (keys rules)

  fillBag ∷ Color → Maybe Bag
  fillBag c = lookup c rules >>= \{ content } → (pure ∘ Branch (Tuple 1 c)) (f <$?> content)
    where
    f { color, count } =
      lookup color rules
        *> fillBag color
        >>= (pure ∘ mapValue (lmap (const count)))

-- PARSERS
colorName ∷ Parser String
colorName = word <> string " " <> word

contentListEntry ∷ Parser Content
contentListEntry = do
  count ← unsigned
  void (char ' ')
  color ← colorName
  void $ (string " bags") <|> (string " bag")
  pure { color, count }

contentList ∷ Parser (List Content)
contentList = contentListEntry `sepEndBy` ((string ", ") <|> (string "."))

emptyContentList ∷ Parser (List Content)
emptyContentList = do
  void (string "no other bags.")
  pure Nil

bag ∷ Parser Rule
bag = do
  color ← colorName
  void (string " bags contain ")
  content ← emptyContentList <|> contentList
  pure { color, content }
