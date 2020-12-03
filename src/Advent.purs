module Advent where

import Advent.Door1 as Door1

data Door
  = Door1 String

open :: Door -> String
open (Door1 x) = Door1.open x
