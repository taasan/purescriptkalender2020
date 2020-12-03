module Advent where

import Data.Maybe (Maybe)
import Advent.Door1 as D1
import Advent.Door2 as D2
import Advent.Door3 as D3
import Advent.Lib (lines)

data Door
  = Door1 String
  | Door2 String
  | Door3 String

open :: Door -> Maybe String
open (Door1 x) = D1.open x

open (Door2 x) = D2.open x

open (Door3 x) = D3.open x
