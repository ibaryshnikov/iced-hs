{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Task qualified as Task
import Iced.Theme
import Iced.Time
import Iced.Widget
import Iced.Widget.Button qualified as Button
import Iced.Widget.ProgressBar qualified as ProgressBar

data Model = Model
  { value :: Float
  , running :: Bool
  }

data Message = StartTimer | Tick

tick :: Task Message
tick = Task.perform $ do
  delayMillis 15
  pure Tick

startTimer :: Model -> (Model, Task Message)
startTimer model =
  if model.running
    then (model, Task.none)
    else (model{running = True, value = 0}, tick)

updateOnTick :: Model -> (Model, Task Message)
updateOnTick model =
  if model.value > 100
    then (model{running = False}, Task.none)
    else (model{value = model.value + 0.5}, tick)

update :: Message -> Model -> (Model, Task Message)
update StartTimer = startTimer
update Tick = updateOnTick

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
    column
      [alignX Center, spacing 50]
      [ progressBar
          [width $ Fixed 300, height $ Fixed 30, style ProgressBar.Danger]
          0
          100
          model.value
      , button [onPressIf (not model.running) StartTimer, style Button.Danger] "Start"
      ]

main :: IO ()
main = Iced.run [theme Oxocarbon] "ProgressBar" model update view
 where
  model = Model{value = 0, running = False}
