module Advent.Door where

import Advent.Lib ((∘))
import Prelude (class Bounded, class Eq, class Ord, class Show, bind, pure, show, ($), (<>))
import Advent.Door1 as D1
import Advent.Door2 as D2
import Advent.Door3 as D3
import Advent.Door4 as D4
import Advent.Door5 as D5
import Advent.Door6 as D6
import Advent.Door7 as D7
import Advent.Door8 as D8
import Advent.Door9 as D9
import Advent.Door12 as D12
import Advent.Door25 as D25
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Door
  = Door1
  | Door2
  | Door3
  | Door4
  | Door5
  | Door6
  | Door7
  | Door8
  | Door9
  | Door10
  | Door11
  | Door12
  | Door13
  | Door14
  | Door15
  | Door16
  | Door17
  | Door18
  | Door19
  | Door20
  | Door21
  | Door22
  | Door23
  | Door24
  | Door25

class HasAnswer a b where
  answer ∷ a → b

instance hasAnswerDoor ∷ HasAnswer Door (Either String String) where
  answer Door1 = Right "[1020036,286977330]"
  answer Door2 = Right "[645,737]"
  answer Door3 = Right "[169.0,7560370818.0]"
  answer Door4 = Right "[213,147]"
  answer Door5 = Right "[974,646]"
  answer Door6 = Right "[6775,3356]"
  answer Door7 = Right "[316,11310]"
  answer Door8 = Right "[1930,1688]"
  answer Door9 = Right "[18272118.0,2186361.0]"
  answer Door12 = Right "[904,18747]"
  answer Door25 = Right "11576351.0"
  answer _ = Left "Nothing yet"

derive instance genericDoor ∷ Generic Door _

derive instance eqDoor ∷ Eq Door

derive instance ordDoor ∷ Ord Door

instance showDoor ∷ Show Door where
  show = genericShow

instance enumDoor ∷ Enum Door where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedDoor ∷ Bounded Door where
  bottom = Door1
  top = Door25

instance boundedEnumDoor ∷ BoundedEnum Door where
  cardinality = Cardinality 25
  toEnum 1 = pure Door1
  toEnum 2 = pure Door2
  toEnum 3 = pure Door3
  toEnum 4 = pure Door4
  toEnum 5 = pure Door5
  toEnum 6 = pure Door6
  toEnum 7 = pure Door7
  toEnum 8 = pure Door8
  toEnum 9 = pure Door9
  toEnum 10 = pure Door10
  toEnum 11 = pure Door11
  toEnum 12 = pure Door12
  toEnum 13 = pure Door13
  toEnum 14 = pure Door14
  toEnum 15 = pure Door15
  toEnum 16 = pure Door16
  toEnum 17 = pure Door17
  toEnum 18 = pure Door18
  toEnum 19 = pure Door19
  toEnum 20 = pure Door20
  toEnum 21 = pure Door21
  toEnum 22 = pure Door22
  toEnum 23 = pure Door23
  toEnum 24 = pure Door24
  toEnum 25 = pure Door25
  toEnum _ = Nothing
  fromEnum Door1 = 1
  fromEnum Door2 = 2
  fromEnum Door3 = 3
  fromEnum Door4 = 4
  fromEnum Door5 = 5
  fromEnum Door6 = 6
  fromEnum Door7 = 7
  fromEnum Door8 = 8
  fromEnum Door9 = 9
  fromEnum Door10 = 10
  fromEnum Door11 = 11
  fromEnum Door12 = 12
  fromEnum Door13 = 13
  fromEnum Door14 = 14
  fromEnum Door15 = 15
  fromEnum Door16 = 16
  fromEnum Door17 = 17
  fromEnum Door18 = 18
  fromEnum Door19 = 19
  fromEnum Door20 = 20
  fromEnum Door21 = 21
  fromEnum Door22 = 22
  fromEnum Door23 = 23
  fromEnum Door24 = 24
  fromEnum Door25 = 25

open ∷ Door → Effect (Either String String)
open door = do
  input <- getInput door
  doOpen door input

doOpen ∷ Door → String → Effect (Either String String)
doOpen Door1 = pure ∘ D1.open

doOpen Door2 = pure ∘ D2.open

doOpen Door3 = pure ∘ D3.open

doOpen Door4 = pure ∘ D4.open

doOpen Door5 = pure ∘ D5.open

doOpen Door6 = pure ∘ D6.open

doOpen Door7 = pure ∘ D7.open

doOpen Door8 = pure ∘ D8.open

doOpen Door9 = pure ∘ D9.open

doOpen Door12 = pure ∘ D12.open

doOpen Door25 = pure ∘ D25.open

doOpen _ = \_ → (pure ∘ Left) "Nothing yet"

getInput ∷ Door → Effect String
getInput n = readTextFile UTF8 $ "input/" <> (show ∘ fromEnum) n
