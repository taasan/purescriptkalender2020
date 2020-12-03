module Main where

import Prelude
import Advent (Door(..), open)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
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

doors :: Array (String -> Door)
doors = [ Door1, Door2, Door3 ]

openDoor :: Int -> (String -> Door) -> Effect String
openDoor day door = do
  input <- getInput day
  pure $ show day <> "\t" <> output (open (door input))
  where
  output (Just x) = x

  output x = show x

getInput :: Int -> Effect String
getInput day = readTextFile UTF8 $ "input/" <> show day
