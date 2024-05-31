{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Extra
import Iced.Subscription qualified as Subscription
import Iced.Time
import Iced.Widget

data Model = Model {
  timePassed :: Integer,
  lastTick :: Integer,
  running :: Bool
}

data Message = Toggle | Tick Integer

update :: Model -> Message -> IO Model
update model Toggle = do
  lastTick <- microsSinceStart
  pure model { running = not model.running, lastTick = lastTick }
update model (Tick micros) =
  -- after we click the button subscription stops producing new events,
  -- but old events are still be delivered
  if model.running
  then do
    let diff = micros - model.lastTick
    pure model {
      timePassed = model.timePassed + diff,
      lastTick = micros
    }
  else
    pure model

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  column [spacing 25, alignItems Center] [
    label model.timePassed,
    button [onPress Toggle] (if model.running then "Stop" else "Start")
  ]

label :: Integer -> Element
label micros = text [size 30] $ formatTime millis
  where (millis, _) = divMod micros 1000

formatTime :: Integer -> String
formatTime micros =
  (showLeading minutes) ++ ":" ++
  (showLeading seconds) ++ "," ++
  (showLeading millis)
  where
    (s, ms) = divMod micros 1000
    (millis, _) = divMod ms 10
    (minutes, seconds) = divMod s 60

showLeading :: Integer -> String
showLeading n =
  if n < 10
  then "0" ++ show n
  else show n

subscriptionFn :: Model -> IO (Subscription Message)
subscriptionFn model = do
  if model.running
  then every (durationFromMillis 10) Tick
  else Subscription.none

main :: IO ()
main = Iced.run [subscription subscriptionFn] "Stopwatch" model update view
  where model = Model { timePassed = 0, lastTick = 0, running = False }
