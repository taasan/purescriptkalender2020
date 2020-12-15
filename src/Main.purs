module Main where

import Prelude
import Advent.Door (Door(..), answer, open)
import Advent.Lib (head, (<$?>))
import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, underline, withGraphics)
import Control.Monad.Error.Class (try)
import Data.Either (Either(..), isLeft)
import Data.Enum (fromEnum, toEnum, upFromIncluding)
import Data.Foldable (elem)
import Data.Int (fromString)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Data.Unfoldable1 (singleton)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message)
import Node.Process (argv, exit)

data Result
  = Unknown
  | Correct
  | Wrong

derive instance resultEq ∷ Eq Result

main ∷ Effect Unit
main = do
  args' <- argv
  let
    args = fromString <$?> args'

    mn = head args
  results ← go $ head args >>= toEnum
  let
    exitCode = if Wrong `elem` (snd <$> results) then 1 else 0
  traverse_ print results
  exit exitCode
  where
  color result = case result of
    Unknown → BrightYellow
    Correct → BrightGreen
    Wrong → BrightRed

  ansi ∷ Color → String → String
  ansi x = withGraphics (bold <> underline <> foreground x)

  print (Tuple output result) = log $ ansi (color result) output

  log' s = log $ withGraphics (foreground Blue) s

  go ∷ Maybe Door → Effect (List (Tuple String Result))
  go md = do
    case md of
      Just door → do
        res <- openDoor door
        log' $ "Opening " <> show door <> "…"
        pure $ singleton res
      _ → do
        log' $ "Opening all doors…"
        openDoors

openDoors ∷ Effect (List (Tuple String Result))
openDoors = traverse openDoor $ upFromIncluding Door1

openDoor ∷ Door → Effect (Tuple String Result)
openDoor door = open'
  where
  correct = answer door

  open' ∷ Effect (Tuple String Result)
  open' = do
    opened <- try $ open door
    case opened of
      Left err -> return (message err) Unknown
      Right answer -> do
        let
          ok =
            if isLeft correct then
              Unknown
            else if answer == correct then
              Correct
            else
              Wrong

          value (Right x) = x

          value (Left x) = x
        return (value answer) ok

  return answer result = pure $ Tuple (show (fromEnum door) <> "\t" <> answer) result
