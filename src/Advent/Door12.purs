module Advent.Door12 where

import Prelude
import Advent.Lib ((∘))
import Advent.Parser (Parser, integer)
import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.Either as Either
import Data.Enum
  ( class BoundedEnum
  , class Enum
  , defaultCardinality
  , defaultFromEnum
  , defaultToEnum
  , pred
  , succ
  )
import Data.Function (applyN)
import Data.Int (round, toNumber)
import Data.List (foldl)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (abs)
import Data.String (trim)
import Math (cos, pi, sin)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Combinators (sepEndBy1)
import Text.Parsing.Parser.String (char)

{-
--- Day 12: Rain Risk ---

The navigation instructions (your puzzle input) consists of a sequence
of single-character actions paired with integer input values. After
staring at them for a few minutes, you work out what they probably
mean:

    Action N means to move north by the given value.
    Action S means to move south by the given value.
    Action E means to move east by the given value.
    Action W means to move west by the given value.
    Action L means to turn left the given number of degrees.
    Action R means to turn right the given number of degrees.
    Action F means to move forward by the given value in the direction
      the ship is currently facing.

The ship starts by facing east. Only the L and R actions change the
direction the ship is facing. (That is, if the ship is facing east and
the next instruction is N10, the ship would move north 10 units, but
would still move east if the following action were F.)


At the end of these instructions, the ship's Manhattan distance (sum
of the absolute values of its east/west position and its north/south
position) from its starting position is 17 + 8 = 25.

Figure out where the navigation instructions lead. What is the
Manhattan distance between that location and the ship's starting
position?

--- Part Two ---

Before you can give the destination to the captain, you realize that
the actual action meanings were printed on the back of the
instructions the whole time.

Almost all of the actions indicate how to move a waypoint which is
relative to the ship's position:

    Action N means to move the waypoint north by the given value.
    Action S means to move the waypoint south by the given value.
    Action E means to move the waypoint east by the given value.
    Action W means to move the waypoint west by the given value.
    Action L means to rotate the waypoint around the ship left
      (counter-clockwise) the given number of degrees.
    Action R means to rotate the waypoint around the ship right
      (clockwise) the given number of degrees.
    Action F means to move forward to the waypoint a number of times
      equal to the given value.

The waypoint starts 10 units east and 1 unit north relative to the
ship. The waypoint is relative to the ship; that is, if the ship
moves, the waypoint moves with it.

Figure out where the navigation instructions actually lead. What is
the Manhattan distance between that location and the ship's starting
position?

-}
open ∷ String → Either String String
open input = case (runParser ∘ trim) input $ instruction `sepEndBy1` char '\n' of
  Either.Right xs → (pure ∘ show) [ go xs (newNavigable ∷ Ship), go xs (newNavigable ∷ WaypointShip Ship) ]
  Either.Left err → (Either.Left ∘ show) err
  where
  go ∷ ∀ a. Navigable a ⇒ NonEmptyList Instruction → a → Int
  go instructions f = manhattanDistance $ foldl (flip navigate) f instructions

  manhattanDistance ∷ ∀ a. Navigable a ⇒ a → Int
  manhattanDistance ship = abs latitude + abs longitude
    where
    { latitude, longitude } = getPosition ship

  navigate ∷ ∀ a. Navigable a ⇒ Instruction → a → a
  navigate (Instruction (Head heading) distance) = head heading distance

  navigate (Instruction (Turn dir) deg) = turn dir deg

  navigate (Instruction Forward distance) = forward distance

data Instruction
  = Instruction Action Int

data Action
  = Head Heading
  | Turn Direction
  | Forward

data Direction
  = Left
  | Right

type Position
  = { latitude ∷ Int
    , longitude ∷ Int
    }

data Ship
  = Ship Position Heading

newtype Waypoint
  = Waypoint Position

data WaypointShip a
  = WaypointShip Waypoint a

data Heading
  = North
  | East
  | South
  | West

move ∷ Heading → Int → Position → Position
move North a p@{ latitude } = p { latitude = latitude + a }

move South a p = move North (negate a) p

move East a p@{ longitude } = p { longitude = longitude + a }

move West a p = move East (negate a) p

-- Classes and instances
class Navigable a where
  newNavigable ∷ a
  turn ∷ Direction → Int → a → a
  forward ∷ Int → a → a
  head ∷ Heading → Int → a → a
  getPosition ∷ a → Position
  setPosition ∷ Position → a → a
  getHeading ∷ a → Heading

instance navigableShip ∷ Navigable Ship where
  newNavigable = Ship { latitude: 0, longitude: 0 } East
  getPosition (Ship x _) = x
  setPosition position (Ship _ x) = Ship position x
  getHeading (Ship _ x) = x
  turn dir deg ship =
    let
      heading' = applyN (turn' dir) (deg / 90) $ getHeading ship
        where
        turn' Left = prev

        turn' Right = next
    in
      Ship (getPosition ship) heading'
  forward distance ship = head (getHeading ship) distance ship
  head heading distance ship = setPosition (move heading distance $ getPosition ship) ship

getNavigable ∷ ∀ a. Navigable a ⇒ WaypointShip a → a
getNavigable (WaypointShip _ x) = x

getWaypoint ∷ ∀ a. WaypointShip a → Waypoint
getWaypoint (WaypointShip x _) = x

setWaypoint ∷ ∀ a. Waypoint → WaypointShip a → WaypointShip a
setWaypoint x (WaypointShip _ y) = WaypointShip x y

instance navigableWaypointShip ∷ Navigable a ⇒ Navigable (WaypointShip a) where
  newNavigable = WaypointShip (wrap { latitude: 1, longitude: 10 }) newNavigable
  getPosition = getPosition ∘ getNavigable
  setPosition position ship = WaypointShip (getWaypoint ship) (setPosition position $ getNavigable ship)
  getHeading = getHeading ∘ getNavigable
  turn dir degrees ship =
    let
      rotate ∷ Direction → Int → Waypoint
      rotate Right d = rotate Left (negate d)

      rotate Left d = wrap { latitude, longitude }
        where
        wp = (unwrap ∘ getWaypoint) ship

        translated =
          { latitude: toNumber wp.latitude
          , longitude: toNumber wp.longitude
          }

        latitude = round $ sin deg * translated.longitude + cos deg * translated.latitude

        longitude = round $ cos deg * translated.longitude - sin deg * translated.latitude

        deg = (pi / 180.0) * toNumber d
    in
      setWaypoint (rotate dir degrees) ship
  forward distance ship =
    let
      p = getPosition ship

      wp = (unwrap ∘ getWaypoint) ship

      pos =
        { latitude: p.latitude + (distance * wp.latitude)
        , longitude: p.longitude + (distance * wp.longitude)
        }
    in
      setPosition pos ship
  head heading distance ship =
    let
      waypoint' = wrap $ move heading distance $ (unwrap ∘ getWaypoint) ship
    in
      setWaypoint waypoint' ship

derive instance newtypeWaypoint ∷ Newtype Waypoint _

derive instance eqHeading ∷ Eq Heading

derive instance ordHeading ∷ Ord Heading

instance enumHeading ∷ Enum Heading where
  succ North = Just East
  succ East = Just South
  succ South = Just West
  succ West = Nothing
  pred North = Nothing
  pred East = Just North
  pred South = Just East
  pred West = Just South

instance boundedHeading ∷ Bounded Heading where
  bottom = North
  top = West

instance boundedEnumHeading ∷ BoundedEnum Heading where
  cardinality = defaultCardinality
  toEnum = defaultToEnum
  fromEnum = defaultFromEnum

-- Fun with circular enums
class
  (Eq a, Bounded a, Enum a) ⇐ PerpetualEnum a where
  next ∷ a → a
  prev ∷ a → a

instance perpetualEnumHeading ∷ PerpetualEnum Heading where
  next = defaultNext
  prev = defaultPrev

defaultNext ∷ ∀ a. Eq a ⇒ Bounded a ⇒ Enum a ⇒ a → a
defaultNext x
  | x == top = bottom

defaultNext x = unsafePartial $ fromJust $ succ x

defaultPrev ∷ ∀ a. Eq a ⇒ Bounded a ⇒ Enum a ⇒ a → a
defaultPrev x
  | x == bottom = top

defaultPrev x = unsafePartial $ fromJust $ pred x

-- PARSER
instruction ∷ Parser Instruction
instruction = Instruction <$> action <*> integer
  where
  action ∷ Parser Action
  action = head' <|> turn' <|> forward'
    where
    head' = Head <$> heading

    turn' = Turn <$> direction

    forward' = Forward <$ char 'F'

  heading ∷ Parser Heading
  heading =
    (North <$ char 'N')
      <|> (East <$ char 'E')
      <|> (South <$ char 'S')
      <|> (West <$ char 'W')

  direction ∷ Parser Direction
  direction = (Left <$ char 'L') <|> (Right <$ char 'R')
