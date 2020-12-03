module Advent where

import Advent.Door1 as D1
import Advent.Door2 as D2

data Door
  = Door1 String
  | Door2 String

open :: Door -> String
open (Door1 x) = D1.open x

open (Door2 x) = D2.open x
