module Advent.Door25 (open) where

import Prelude
import Advent.Lib ((∘))
import Advent.Parser (natural)
import Data.Either (Either(..), note)
import Data.Enum (succ)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math ((%))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (char)

{-

https://adventofcode.com/2020/day/25

--- Day 25: Combo Breaker ---

The handshake used by the card and the door involves an operation that
transforms a subject number. To transform a subject number, start with
the value 1. Then, a number of times called the loop size, perform the
following steps:

    Set the value to itself multiplied by the subject number.
    Set the value to the remainder after dividing the value by 20201227.

The card always uses a specific, secret loop size when it transforms a
subject number. The door always uses a different, secret loop size.

For example, suppose you know that the card's public key is
5764801. With a little trial and error, you can work out that the
card's loop size must be 8, because transforming the initial subject
number of 7 with a loop size of 8 produces 5764801.

Then, suppose you know that the door's public key is 17807724. By the
same process, you can determine that the door's loop size is 11,
because transforming the initial subject number of 7 with a loop size
of 11 produces 17807724.

At this point, you can use either device's loop size with the other
device's public key to calculate the encryption key. Transforming the
subject number of 17807724 (the door's public key) with a loop size of
8 (the card's loop size) produces the encryption key,
14897079. (Transforming the subject number of 5764801 (the card's
public key) with a loop size of 11 (the door's loop size) produces the
same encryption key: 14897079.)

What encryption key is the handshake trying to establish?
-}
magicConstant ∷ Number
magicConstant = 20201227.0

open ∷ String → Either String String
open input = do
  case runParser input (Tuple <$> natural <* char '\n' <*> natural) of
    Right (Tuple card door) → do
      let
        door' = toNumber door

        card' = toNumber card
      key1 ← show <$> (note "key1" $ transform door' <$> findLoopSize card')
      key2 ← show <$> (note "key2" $ transform card' <$> findLoopSize door')
      if key1 == key2 then
        pure key1
      else
        Left ("Key mismatch: " <> key1 <> " /= " <> key2)
    Left err → (Left ∘ show) err

transform ∷ Number → Int → Number
transform subject ls = go ls 1.0
  where
  go loopSize acc
    | loopSize <= 0 = acc

  go loopSize acc = go (loopSize - 1) next
    where
    next = acc * subject % magicConstant

findLoopSize ∷ Number → Maybe Int
findLoopSize key = go 0 1.0
  where
  go loopSize acc = case succ loopSize of
    Just x → if key == next then Just x else go x next
    _ → Nothing
    where
    next = acc * 7.0 % magicConstant
