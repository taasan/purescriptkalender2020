module Advent.Door2 (open) where

import Prelude
import Advent.Lib (head, lines, unsafeSplit, (!!), (..?), (<$$>), (∘))
import Data.Array (sort)
import Data.Either (Either)
import Data.Filterable (compact, filter)
import Data.Foldable (length)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.String (Pattern(..))
import Data.String.CodeUnits (charAt, toCharArray)

{- https://adventofcode.com/2020/day/2

--- Part One ---

To try to debug the problem, they have created a list (your puzzle
input) of passwords (according to the corrupted database) and the
corporate policy when that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The
password policy indicates the lowest and highest number of times a
given letter must appear for the password to be valid. For example,
1-3 a means that the password must contain a at least 1 time and at
most 3 times.

In the above example, 2 passwords are valid. The middle password,
cdefg, is not; it contains no instances of b, but needs at least
1. The first and third passwords are valid: they contain one a or nine
c, both within the limits of their respective policies.

How many passwords are valid according to their policies?

--- Part Two ---

While it appears you validated the passwords correctly, they don't
seem to be what the Official Toboggan Corporate Authentication System
is expecting.

The shopkeeper suddenly realizes that he just accidentally explained
the password policy rules from his old job at the sled rental place
down the street! The Official Toboggan Corporate Policy actually works
a little differently.

Each policy actually describes two positions in the password, where 1
means the first character, 2 means the second character, and so
on. (Be careful; Toboggan Corporate Policies have no concept of "index
zero"!) Exactly one of these positions must contain the given
letter. Other occurrences of the letter are irrelevant for the
purposes of policy enforcement.

Given the same example list from above:

    1-3 a: abcde is valid: position 1 contains a and position 3 does not.
    1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
    2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

How many passwords are valid according to the new interpretation of
the policies?
-}
type PasswordEntry
  = { num1 ∷ Int
    , num2 ∷ Int
    , char ∷ Char
    , password ∷ String
    }

parsePassword ∷ String → Maybe PasswordEntry
parsePassword str = do
  n1 ← head xs >>= fromString
  n2 ← xs !! 1 >>= fromString
  c ← xs !! 2 >>= charAt 0
  p ← xs !! 3
  pure { num1: n1, num2: n2, char: c, password: p }
  where
  xs = (filter ((/=) "") ∘ unsafeSplit (Pattern """[-: ]""")) str

open ∷ String → Either String String
open input = pure ∘ show $ countValid <$> [ isValidPart1, isValidPart2 ]
  where
  countValid ∷ (PasswordEntry → Boolean) → Int
  countValid p = (length ∘ filter p) passwords

  passwords ∷ Array PasswordEntry
  passwords = parsePassword <$$> lines input

  isValidPart1 ∷ PasswordEntry → Boolean
  isValidPart1 x = (x.num1 ..? x.num2) len
    where
    len = (length ∘ filter ((==) x.char)) $ toCharArray x.password

  isValidPart2 ∷ PasswordEntry → Boolean
  isValidPart2 x = result $ (sort ∘ map ((==) x.char)) chars
    where
    chars = compact $ [ charAt (x.num1 - 1) password, charAt (x.num2 - 1) password ]

    result [ false, true ] = true

    result _ = false

    password = x.password
