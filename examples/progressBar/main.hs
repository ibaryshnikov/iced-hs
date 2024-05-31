{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Command qualified as Command
import Iced.Extra
import Iced.Time
import Iced.Widget

data Model = Model {
  value :: Float,
  running :: Bool
}

data Message = StartTimer | Tick

tick :: Command Message
tick = Command.perform $ do
  sleep $ durationFromMillis 15
  pure Tick

update :: Model -> Message -> (Model, Command Message)
update model StartTimer = if model.running
  then (model, Command.none)
  else (model { running = True, value = 0 }, tick)
update model Tick = if model.value > 100
  then (model { running = False }, Command.none)
  else (model { value = model.value + 0.5 }, tick)

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  column [alignItems Center, spacing 50] [
    progressBar [width $ Fixed 300, height $ Fixed 30] 0 100 model.value,
    button [onPressIf (not model.running) StartTimer] "Start"
  ]

main :: IO ()
main = Iced.run [] "ProgressBar" model update view
  where model = Model { value = 0, running = False }
