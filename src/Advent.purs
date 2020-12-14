module Advent where

import Data.Either (Either)
import Advent.Door1 as D1
import Advent.Door2 as D2
import Advent.Door3 as D3
import Advent.Door4 as D4
import Advent.Door5 as D5
import Advent.Door6 as D6
import Advent.Door7 as D7
import Advent.Door8 as D8
import Advent.Door9 as D9

data Door
  = Door1 String
  | Door2 String
  | Door3 String
  | Door4 String
  | Door5 String
  | Door6 String
  | Door7 String
  | Door8 String
  | Door9 String

open ∷ Door → Either String String
open (Door1 x) = D1.open x

open (Door2 x) = D2.open x

open (Door3 x) = D3.open x

open (Door4 x) = D4.open x

open (Door5 x) = D5.open x

open (Door6 x) = D6.open x

open (Door7 x) = D7.open x

open (Door8 x) = D8.open x

open (Door9 x) = D9.open x
