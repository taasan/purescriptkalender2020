module Main where

import Prelude
import Advent (Door(..), open)
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
doors = [ Door1 ]

openDoor :: Int -> (String -> Door) -> Effect String
openDoor day door = do
  input <- getInput day
  pure $ show day <> "\t" <> open (door input)

getInput :: Int -> Effect String
getInput day = readTextFile UTF8 $ "input/" <> show day
