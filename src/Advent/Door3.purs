module Advent.Door3 (open) where

{- https://adventofcode.com/2020/day/3

--- Day 3: Toboggan Trajectory ---

..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#

# = Tree

The same pattern repeats to the right many times.

You start on the open square (.) in the top-left corner and need to
reach the bottom (below the bottom-most row on your map).

From your starting position at the top-left, check the position that
is right 3 and down 1. Then, check the position that is right 3 and
down 1 from there, and so on until you go past the bottom of the map.

Starting at the top-left corner of your map and following a slope of
right 3 and down 1, how many trees would you encounter?

--- Part Two ---

Determine the number of trees you would encounter if, for each of the
following slopes, you start at the top-left corner and traverse the
map all the way to the bottom:

    Right 1, down 1.
    Right 3, down 1. (This is the slope you already checked.)
    Right 5, down 1.
    Right 7, down 1.
    Right 1, down 2.

What do you get if you multiply together the number of trees
encountered on each of the listed slopes?
-}
import Prelude
import Advent.Lib (index, lines)
import Data.Array (fromFoldable, (..))
import Data.Filterable (compact, filter)
import Data.Foldable (length, product)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.String.CodeUnits (toCharArray)

data Content
  = Tree
  | OpenSquare

derive instance eqContent :: Eq Content

type Slope
  = { deltaCol :: Int, deltaRow :: Int }

type Coordinate
  = { col :: Int, row :: Int }

mkSlope :: Int -> Int -> Slope
mkSlope deltaCol deltaRow = { deltaCol, deltaRow }

infixl 8 mkSlope as !

open :: String -> Maybe String
open str = pure $ show $ [ toNumber part1, part2 ]
  where
  countTrees slope = (length <<< filter ((==) Tree)) $ collect slope

  part1 = countTrees { deltaCol: 3, deltaRow: 1 }

  part2 = product $ (toNumber <<< countTrees) <$> slopes
    where
    slopes =
      [ 1 ! 1
      , 3 ! 1
      , 5 ! 1
      , 7 ! 1
      , 1 ! 2
      ]

  grid :: Array (Array Content)
  grid = parseLines <$> fromFoldable (lines str)

  path :: Slope -> Array Coordinate
  path slope = getCoordinate slope <$> 0 .. length grid

  collect :: Slope -> Array Content
  collect slope = compact $ getContent <$> path slope

  getContent :: Coordinate -> Maybe Content
  getContent { row, col } = do
    rowC <- index grid row
    index' rowC col

  parseLines :: String -> Array Content
  parseLines xs = parseContent <$> toCharArray xs

  parseContent :: Char -> Content
  parseContent '#' = Tree

  parseContent _ = OpenSquare

getCoordinate :: Slope -> Int -> Coordinate
getCoordinate { deltaCol, deltaRow } row = { col: row * deltaCol, row: row * deltaRow }

index' :: forall a. Array a -> Int -> Maybe a
index' arr idx = index arr $ idx `mod` (length arr)
