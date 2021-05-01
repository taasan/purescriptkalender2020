module Advent.Door8 (open) where

{- https://adventofcode.com/2020/day/8

--- Day 8: Handheld Halting ---

Consider the following program:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6

These instructions are visited in this order:

nop +0  | 1
acc +1  | 2, 8(!)
jmp +4  | 3
acc +3  | 6
jmp -3  | 7
acc -99 |
acc +1  | 4
jmp -4  | 5
acc +6  |

This is an infinite loop.

Run your copy of the boot code. Immediately before any instruction is
executed a second time, what value is in the accumulator?

--- Part Two ---

Fix the program so that it terminates normally by changing exactly one
jmp (to nop) or nop (to jmp). What is the value of the accumulator
after the program terminates?

-}
import Prelude
import Advent.Lib (fromFoldable, (!!), (∘), (<<$>>))
import Advent.Parser (integer)
import Control.Alt ((<|>))
import Data.Array (all, updateAt)
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap)
import Data.Foldable (length)
import Data.List (List(..), (..), (:))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Additive (Additive)
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParseError, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (char, eof, string)

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  | End

data ProgramState
  = Running Int
  | Terminated Int ExitStatus

data ExitStatus
  = Ok
  | Error Exception

data Exception
  = SegFault
  | InfiniteLoop
  | IllegalState

type Program result
  = { acc ∷ result
    , instructions ∷ Array Instruction
    , programState ∷ ProgramState
    }

-- Fun with type classes
class IsInstruction result a where
  execute ∷ a → result → result

instance additiveIntIsInstruction ∷ IsInstruction (Additive Int) Instruction where
  execute (Acc x) acc = acc <> wrap x
  execute _ acc = acc

open ∷ String → Either String String
open input = (evaluation ∘ show) $ filterMap identity answer
  where
  answer = unwrap <<$>> [ part1, part2 ]

  evaluation = if all isJust answer then Right else Left

  program ∷ Array Instruction
  program = case parse input of
    Right instructions → fromFoldable instructions
    _ → []

  part1 = case run' program of
    Left (Tuple InfiniteLoop { acc }) → pure $ acc
    _ → Nothing

  part2 = go $ filter shouldSwap (0 .. (length program))
    where
    shouldSwap line = case program !! line of
      Just (Nop x) → x /= 0 && x /= 1
      Just (Jmp x) → x /= 1
      _ → false

    go Nil = Nothing

    go (line : lines) = case run' swap of
      Right { acc } → pure acc
      Left (Tuple InfiniteLoop _) → go lines
      _ → Nothing
      where
      swap = case program !! line of
        Just (Jmp x) → doInsert (Nop x)
        Just (Nop x) → doInsert (Jmp x)
        _ → program

      doInsert new = fromMaybe program $ updateAt line new program

  run' ∷ _ → _ (Program (Additive Int))
  run' = run mempty

run ∷
  ∀ result.
  IsInstruction result Instruction ⇒
  result →
  Array Instruction →
  Either (Tuple Exception (Program result)) (Program result)
run acc instructions = go mempty $ (mkProgram acc) { instructions = instructions }
  where
  go _ current@{ programState: Terminated _ Ok } = pure current

  go _ current@{ programState: Terminated _ (Error ex) } = Left $ Tuple ex current

  go visited current@{ programState: Running line } =
    if Set.member (getLine next) visited then
      Left $ Tuple InfiniteLoop current
    else
      go (Set.insert line visited) next
    where
    next = step current

    getLine { programState: Running l } = l

    getLine { programState: Terminated l _ } = l

step ∷
  ∀ result.
  IsInstruction result Instruction ⇒
  Program result → Program result
step current@{ programState: Terminated line Ok } = updateState current (Terminated line (Error IllegalState))

step current@{ programState: Terminated _ _ } = current

step current@{ acc, instructions, programState: Running line } = case instructions !! line of
  Nothing → updateState' $ Terminated line (Error SegFault)
  Just instruction → case instruction of
    Nop _ → jump 1
    Jmp x → jump x
    Acc _ → accumulate instruction
    End → updateState' $ Terminated line Ok
  where
  updateState' = updateState current

  jump x = updateState' $ Running (line + x)

  accumulate i = (jump 1) { acc = execute i acc }

updateState ∷ ∀ result. Program result → ProgramState → Program result
updateState old new = old { programState = new }

mkProgram ∷
  ∀ result.
  result →
  Program result
mkProgram acc =
  { acc: acc
  , instructions: mempty
  , programState: Running 0
  }

parse ∷ String → Either ParseError (List Instruction)
parse input = runParser input program
  where
  program = (instruction <|> end) `sepBy1` (char '\n')

  end = End <$ eof

  instruction =
    ( (Acc <$ string "acc")
        <|> (Jmp <$ string "jmp")
        <|> (Nop <$ string "nop")
    )
      <* char ' '
      <*> integer
