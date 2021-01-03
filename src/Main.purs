module Main where

import Prelude
import Advent.Door (Door(..), answer, open)
import Advent.Lib ((<$?>), (∘))
import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, italic, underline, withGraphics)
import Control.Monad.Error.Class (try)
import Data.Bifoldable (bifold)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), hush, isLeft, isRight)
import Data.Enum (fromEnum, toEnum, upFromIncluding)
import Data.Filterable (filter)
import Data.Foldable (any)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Now (now)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile)
import Node.Process (argv, exit, stdout)
import Node.Stream (writeString)

data Result
  = Unknown
  | Correct
  | Wrong

derive instance resultEq ∷ Eq Result

main ∷ Effect Unit
main = do
  args' ← argv
  let
    xs ∷ Array Door
    xs = case (=<<) toEnum ∘ fromString <$?> args' of
      [] → filter (isRight ∘ answer) $ upFromIncluding Door1
      xs' → xs'
  results ← traverse (printAndCollect ∘ openDoor) xs
  exit if any (_ == Wrong) results then 1 else 0
  where
  color result = case result of
    Unknown → BrightYellow
    Correct → BrightGreen
    Wrong → BrightRed

  printAndCollect ∷ Effect (Tuple String Result) → Effect Result
  printAndCollect x = do
    start ← now
    (Tuple output result) ← x
    end ← now
    let
      duration = (unwrap ∘ unInstant) end - (unwrap ∘ unInstant) start
    log
      $ ansi (color result) output
      <> withGraphics (foreground Magenta <> italic)
          (" (" <> show duration <> " ms)")
    pure result

ansi ∷ Color → String → String
ansi x = withGraphics (bold <> underline <> foreground x)

log' ∷ String → Effect Unit
log' s = log $ withGraphics (foreground Blue) s

log ∷ String → Effect Unit
log s = (void ∘ writeString stdout UTF8 (s <> "\n")) (pure unit)

openDoor ∷ Door → Effect (Tuple String Result)
openDoor door = open'
  where
  correct = answer door

  open' ∷ Effect (Tuple String Result)
  open' =
    bind (getInput door) case _ of
      Just input → return (bifold answer) ok
        where
        answer = open door input

        ok =
          if isLeft correct then
            Unknown
          else if answer == correct then
            Correct
          else
            Wrong
      Nothing → return ("No input found for " <> show door) Unknown

  return answer result = pure $ Tuple (show (fromEnum door) <> "\t" <> answer) result

getInput ∷ Door → Effect (Maybe String)
getInput n = do
  let
    filename = "input/" <> (show ∘ fromEnum) n
  e ← exists filename
  if e then do
    res ← try (readTextFile UTF8 $ filename)
    void case res of
      Left err → log $ withGraphics (foreground Red) $ show err
      x → pure unit
    (pure ∘ hush) res
  else
    pure Nothing
