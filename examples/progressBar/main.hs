{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Word

import Iced
import Iced.Attribute
import Iced.Command
import Iced.Time
import Iced.Widget

data Model = Model {
  value :: Float,
  running :: Bool
}

data Message = StartTimer | Tick

tick :: Word64 -> Command Message
tick n = Perform future
  where
    duration = durationFromMillis n
    future = sleep duration Tick

update :: Model -> Message -> (Model, Command Message)
update model StartTimer = if model.running
  then (model, None)
  else (model { running = True, value = 0 }, tick 15)
update model Tick = if model.value > 100
  then (model { running = False }, None)
  else (model { value = model.value + 0.5 }, tick 15)

view :: Model -> Element
view model =
  container [centerX, centerY, width Fill, height Fill] $
  column [alignItems Center, spacing 50] [
    progressBar [width $ Fixed 300, height $ Fixed 30] 0 100 model.value,
    button [onPressIf (not model.running) StartTimer] "Start"
  ]

main :: IO ()
main = Iced.run [] "ProgressBar" model update view
  where model = Model { value = 0, running = False }
