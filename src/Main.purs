module Main where

import Prelude
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readInt, readNullOrUndefined, tagOf, unsafeFromForeign)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe, maybe)
import Data.Traversable (traverse)

data MiddleRail = OneHundred | TwoHundred | TwoHundredAndFifty

data TopRail = Standard | Compact

type BottomRail = Int
type SashHeight = Int

newtype Panel = Panel {
  x :: Int, 
  y :: Int
}

newtype Middle = Middle {
  panels :: Array Panel,
  rail :: MiddleRail
}

newtype Door = Door {
  bottomRail :: BottomRail,
  sashHeight :: SashHeight,
  middle :: Maybe Middle,
  topRail :: TopRail
}

readDoor :: Foreign -> F Door
readDoor value = do 
  bottomRail <- readProp "bottomRail" value >>= readInt
  middle <- readProp "middle" value >>= readNullOrUndefined >>= traverse readMiddle
  sashHeight <- readProp "sashHeight" value >>= readInt
  topRail <- readProp "topRail" value >>= readTopRail
  pure $ Door { bottomRail, middle, sashHeight, topRail }

readMiddle :: Foreign -> F Middle 
readMiddle value = do 
  panels <- readProp "panels" value  >>= readArray >>= traverse readPanel
  rail <- readProp "rail" value >>= readMiddleRail
  pure $ Middle { panels, rail }

readMiddleRail :: Foreign -> F MiddleRail
readMiddleRail value = readMiddleRail' $ unsafeFromForeign value
  where readMiddleRail' unsafeValue 
          | unsafeValue == 100 = pure OneHundred
          | unsafeValue == 200 = pure TwoHundred
          | unsafeValue == 250 = pure TwoHundredAndFifty
          | otherwise = fail $ TypeMismatch "MiddleRail" (tagOf value)

readPanel :: Foreign -> F Panel 
readPanel value = do 
  x <- readProp "x" value >>= readInt
  y <- readProp "y" value  >>= readInt
  pure $ Panel { x, y }

readTopRail :: Foreign -> F TopRail
readTopRail value = readTopRail' $ unsafeFromForeign value
  where readTopRail' unsafeValue
          | unsafeValue == "Standard" = pure Standard 
          | unsafeValue == "Compact" = pure Compact
          | otherwise = fail $ TypeMismatch "TopRail" (tagOf value)

isValidRails :: BottomRail -> TopRail -> Boolean 
isValidRails _ Standard = true 
isValidRails bottomRail Compact 
  | bottomRail >= 100 = true 
  | otherwise = false

isValidMiddle :: SashHeight -> Middle -> Boolean 
isValidMiddle height (Middle middle) = isValidMiddleRail height middle.rail

isValidMiddleRail :: SashHeight -> MiddleRail -> Boolean
isValidMiddleRail height OneHundred
  | height >= 2100 = true
  | otherwise = false
isValidMiddleRail height TwoHundred
  | height >= 2200 = true 
  | otherwise = false
isValidMiddleRail _ _ = false

isValidDoor :: Door -> Boolean 
isValidDoor (Door door) = validMiddle && validRails
  where validMiddle = maybe false (isValidMiddle door.sashHeight) door.middle
        validRails = isValidRails door.bottomRail door.topRail

isValidDoorJS :: Foreign -> F Boolean
isValidDoorJS foreignDoor = isValidDoor <$> (readDoor foreignDoor)

getMiddleRailHeight :: MiddleRail -> Int
getMiddleRailHeight OneHundred = 100
getMiddleRailHeight TwoHundred = 200
getMiddleRailHeight TwoHundredAndFifty = 250
