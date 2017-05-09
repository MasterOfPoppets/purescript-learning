module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readInt, readNull, readNullOrUndefined, tagOf, unsafeFromForeign)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)

data MiddleRail = OneHundred | TwoHundred | TwoHundredAndFifty

type SashHeight = Int

newtype Door = Door {
  sashHeight :: SashHeight,
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
  sashHeight <- readInt =<< readProp "sashHeight" value
  middleRail <- traverse readMiddleRail =<< readNullOrUndefined =<< readProp "middleRail" value
  pure $ Door { sashHeight, middleRail }

isValidMiddleRail :: SashHeight -> MiddleRail -> Boolean
isValidMiddleRail height OneHundred
  | height >= 2100 = true
  | otherwise = false
isValidMiddleRail height TwoHundred
  | height >= 2200 = true 
  | otherwise = false
isValidMiddleRail _ _ = false

isValidDoor :: Door -> Boolean 
isValidDoor (Door door) = maybe false (isValidMiddleRail door.sashHeight) door.middleRail 

isValidDoorJS :: Foreign -> F Boolean
isValidDoorJS foreignDoor = isValidDoor <$> (readDoor foreignDoor)

getMiddleRailHeight :: MiddleRail -> Int
getMiddleRailHeight OneHundred = 100
getMiddleRailHeight TwoHundred = 200
getMiddleRailHeight TwoHundredAndFifty = 250

testDoor1 :: Door
testDoor1 = Door { sashHeight: 2300, middleRail: Just TwoHundred}

testDoor2 :: Door 
testDoor2 = Door { sashHeight: 2100, middleRail : Nothing }

showDoor :: Door -> String
showDoor (Door door) = "Door " <> show (getMiddleRailHeight <$> door.middleRail)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ showDoor testDoor1
  log $ showDoor testDoor2
