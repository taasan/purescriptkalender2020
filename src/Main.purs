module Main where

import Prelude
import Advent (Door(..), open)
import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, underline, withGraphics)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  result <- openDoors
  traverse_ log result

openDoors :: Effect (Array String)
openDoors = traverseWithIndex (\i -> openDoor (i + 1)) doors

doors :: Array (Tuple (String -> Door) (Maybe String))
doors =
  [ Tuple Door1 (Just "[1020036,286977330]")
  , Tuple Door2 (Just "[645,737]")
  , Tuple Door3 (Just "[169.0,7560370818.0]")
  ]

openDoor :: Int -> Tuple (String -> Door) (Maybe String) -> Effect String
openDoor day (Tuple door correct) = do
  input <- getInput day
  let
    answer = open (door input)
  let
    color =
      if correct == Nothing then
        BrightYellow
      else if answer == correct then
        BrightGreen
      else
        BrightRed
  let
    ansi = withGraphics (bold <> underline <> foreground color)
  pure $ ansi $ show day <> "\t" <> output answer
  where
  output (Just x) = x

  output x = show x

getInput :: Int -> Effect String
getInput day = readTextFile UTF8 $ "input/" <> show day
