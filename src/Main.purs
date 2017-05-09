module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readInt, readNull, tagOf, unsafeFromForeign)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)

data MiddleRail = OneHundred | TwoHundred | TwoHundredAndFifty

newtype Door = Door {
  bob :: Int,
  middleRail :: Maybe MiddleRail
}

readMiddleRail :: Foreign -> F MiddleRail
readMiddleRail value
  | unsafeFromForeign value == 100 = pure OneHundred
  | unsafeFromForeign value == 200 = pure TwoHundred
  | unsafeFromForeign value == 250 = pure TwoHundredAndFifty
  | otherwise = fail $ TypeMismatch "MiddleRail" (tagOf value)

readDoor :: Foreign -> F Door
readDoor value = do 
  bob <- readInt =<< readProp "bob" value
  middleRail <- traverse readMiddleRail =<< readNull =<< readProp "middleRail" value
  pure $ Door { bob, middleRail }

isValidMiddleRail :: MiddleRail -> Boolean
isValidMiddleRail OneHundred = true
isValidMiddleRail _ = false

isValidDoor :: Door -> Boolean 
isValidDoor (Door door) = maybe false isValidMiddleRail door.middleRail

isValidDoorJS :: Foreign -> F Boolean
isValidDoorJS foreignDoor = isValidDoor <$> (readDoor foreignDoor)

getMiddleRailHeight :: MiddleRail -> Int
getMiddleRailHeight OneHundred = 100
getMiddleRailHeight TwoHundred = 200
getMiddleRailHeight TwoHundredAndFifty = 250

testDoor1 :: Door
testDoor1 = Door { bob: 100, middleRail: Just TwoHundred}

testDoor2 :: Door 
testDoor2 = Door { bob: 250, middleRail : Nothing }

showDoor :: Door -> String
showDoor (Door door) = "Door " <> show (getMiddleRailHeight <$> door.middleRail)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ showDoor testDoor1
  log $ showDoor testDoor2
