module Main where

import Prelude
import Advent (Door(..), open)
import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, underline, withGraphics)
import Data.Either (Either(..), isLeft)
import Data.Foldable (elem)
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (exit)

data Result
  = Unknown
  | Correct
  | Wrong

derive instance resultEq :: Eq Result

main :: Effect Unit
main = do
  results <- openDoors
  let
    exitCode = if Wrong `elem` (snd <$> results) then 1 else 0
  traverse_ print results
  exit exitCode
  where
  color result = case result of
    Unknown -> BrightYellow
    Correct -> BrightGreen
    Wrong -> BrightRed

  ansi :: Color -> String -> String
  ansi x = withGraphics (bold <> underline <> foreground x)

  print (Tuple output result) = log $ ansi (color result) output

openDoors :: Effect (Array (Tuple String Result))
openDoors = traverseWithIndex (\i -> openDoor (i + 1)) doors

doors :: Array (Tuple (String -> Door) (Either String String))
doors =
  [ Tuple Door1 (Right "[1020036,286977330]")
  , Tuple Door2 (Right "[645,737]")
  , Tuple Door3 (Right "[169.0,7560370818.0]")
  , Tuple Door4 (Right "[213,147]")
  , Tuple Door5 (Right "[974,646]")
  , Tuple Door6 (Right "[6775,3356]")
  ]

openDoor :: Int -> Tuple (String -> Door) (Either String String) -> Effect (Tuple String Result)
openDoor day (Tuple door correct) = do
  input <- getInput day
  let
    answer = open (door input)

    ok =
      if isLeft correct then
        Unknown
      else if answer == correct then
        Correct
      else
        Wrong
  pure $ Tuple (show day <> "\t" <> output answer) ok
  where
  output (Right x) = x

  output x = show x

getInput :: Int -> Effect String
getInput day = readTextFile UTF8 $ "input/" <> show day
