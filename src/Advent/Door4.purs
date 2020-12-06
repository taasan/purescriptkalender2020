module Advent.Door4 (open) where

{- https://adventofcode.com/2020/day/4

--- Day 4: Passport Processing ---

The expected fields are as follows:

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each
passport is represented as a sequence of key:value pairs separated by
spaces or newlines. Passports are separated by blank lines.

Count the number of valid passports - those that have all required
fields. Treat cid as optional. In your batch file, how many passports
are valid?

--- Part Two ---

You can continue to ignore the cid field, but each other field has
strict rules about what values are valid for automatic validation:

    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.

Count the number of valid passports - those that have all required
fields and valid values. Continue to treat cid as optional. In your
batch file, how many passports are valid?
-}
import Prelude hiding (between, when)
import Advent.Lib (fromCharList, fromFoldable, (<$$>), (*>+))
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Filterable (partitionMap)
import Data.Foldable (length)
import Data.Int (fromString)
import Data.List (List, many, (:))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Combinators (optional, sepBy)
import Text.Parsing.Parser.String (char, eof, oneOf, noneOf, string)
import Text.Parsing.Parser as P
import Text.Parsing.Parser (ParseError, fail, runParser)

open :: String -> Either String String
open input =
  if length errors /= 0 then
    Left $ show errors
  else
    pure $ show answer
  where
  -- Litt klumsete her.  Fant ikke ut hvordan jeg skulle separere
  -- passene med blank linje samtidig som \n også er feltseparator.
  xs :: List (Either ParseError PassportMap)
  xs = fromFoldable $ ((flip runParser) fields <<< trim) <$> (split (Pattern "\n\n") input)

  { left: errors, right: passportMaps } = partitionMap identity $ xs

  passports = fromMap <$$> passportMaps

  answer :: Array Int
  answer = [ length passports, length $ toTypedPassport <$$> passports ]

data Key
  = Byr
  | Iyr
  | Eyr
  | Hgt
  | Hcl
  | Ecl
  | Pid
  | Cid

derive instance eqKey :: Eq Key

derive instance ordKey :: Ord Key

type Field
  = Tuple Key String

data Height
  = Cm Int
  | In Int

type HairColour
  = String

data EyeColour
  = Amb
  | Blu
  | Brn
  | Gry
  | Grn
  | Hzl
  | Oth

type PassportId
  = String

type Passport
  = { byr :: String
    , iyr :: String
    , eyr :: String
    , hgt :: String
    , hcl :: String
    , ecl :: String
    , pid :: String
    , cid :: Maybe String
    }

type TypedPassport
  = { byr :: Int
    , iyr :: Int
    , eyr :: Int
    , hgt :: Height
    , hcl :: HairColour
    , ecl :: EyeColour
    , pid :: PassportId
    , cid :: Maybe String
    }

type PassportMap
  = Map Key String

toTypedPassport :: Passport -> Maybe TypedPassport
toTypedPassport p = do
  byr <- fromString' 1920 2002 p.byr
  iyr <- fromString' 2010 2020 p.iyr
  eyr <- fromString' 2020 2030 p.eyr
  hgt <- hush $ runParser p.hgt height
  hcl <- hush $ runParser p.hcl hairColour
  ecl <- toEyeColour p.ecl
  pid <- hush $ runParser p.pid passportId
  pure
    { byr
    , iyr
    , eyr
    , hgt
    , hcl
    , ecl
    , pid
    , cid: p.cid
    }
  where
  fromString' a b str = fromString str >>= \x -> if x < a || x > b then Nothing else Just x

  toEyeColour str = case str of
    "amb" -> pure Amb
    "blu" -> pure Blu
    "brn" -> pure Brn
    "gry" -> pure Gry
    "grn" -> pure Grn
    "hzl" -> pure Hzl
    "oth" -> pure Oth
    _ -> Nothing

fromMap :: PassportMap -> Maybe Passport
fromMap m = do
  byr <- M.lookup Byr m
  iyr <- M.lookup Iyr m
  eyr <- M.lookup Eyr m
  hgt <- M.lookup Hgt m
  hcl <- M.lookup Hcl m
  ecl <- M.lookup Ecl m
  pid <- M.lookup Pid m
  pure
    { byr
    , iyr
    , eyr
    , hgt
    , hcl
    , ecl
    , pid
    , cid: M.lookup Cid m
    }

-- PARSERS
type Parser
  = P.Parser String

key :: Parser Key
key =
  (string "byr" *>+ Byr)
    <|> (string "iyr" *>+ Iyr)
    <|> (string "eyr" *>+ Eyr)
    <|> (string "hgt" *>+ Hgt)
    <|> (string "hcl" *>+ Hcl)
    <|> (string "ecl" *>+ Ecl)
    <|> (string "pid" *>+ Pid)
    <|> (optional (string "cid") *>+ Cid)

field :: Parser Field
field = do
  key' <- key
  void $ char ':'
  value <- many $ noneOf [ ' ', '\n' ]
  pure $ Tuple key' $ fromCharList value

fields :: Parser PassportMap
fields = do
  xs <- field `sepBy` oneOf [ ' ', '\n' ]
  (pure <<< M.fromFoldable) xs

unsigned :: Parser Int
unsigned = do
  n <- oneOf digitsNotZero
  ns <- many (oneOf digits)
  case fromString $ (fromCharArray <<< fromFoldable) $ n : ns of
    Just x -> pure x
    _ -> fail "Invalid number"

hairColour :: Parser HairColour
hairColour = do
  n <- char '#'
  ns <- many $ oneOf $ digits <> [ 'a', 'b', 'c', 'd', 'e', 'f' ]
  void eof
  let
    x = (fromCharArray <<< fromFoldable) (n : ns)
  if length ns /= 6 then fail x else pure x

passportId :: Parser PassportId
passportId = do
  ns <- many $ oneOf digits
  void eof
  let
    x = (fromCharArray <<< fromFoldable) ns
  if length ns /= 9 then fail x else pure x

height :: Parser Height
height = do
  h <- unsigned
  u <- parseUnit
  void eof
  let
    height' = u h
  if validHeight height' then pure height' else fail "Invalid height"
  where
  parseUnit :: Parser (Int -> Height)
  parseUnit = ((string "in" *>+ In) <|> (string "cm" *>+ Cm))

  validHeight (Cm x) = x >= 150 && x <= 193

  validHeight (In x) = x >= 59 && x <= 76

digitsNotZero :: Array Char
digitsNotZero = [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

digits :: Array Char
digits = Array.snoc digitsNotZero '0'
