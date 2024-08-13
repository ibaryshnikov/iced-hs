{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Command qualified as Command
import Iced.Theme
import Iced.Time
import Iced.Widget
import Iced.Widget.Button qualified as Button
import Iced.Widget.ProgressBar qualified as ProgressBar

data Model = Model {
  value :: Float,
  running :: Bool
}

data Message = StartTimer | Tick

tick :: Command Message
tick = Command.perform $ do
  delayMillis 15
  pure Tick

startTimer :: Model -> (Model, Command Message)
startTimer model = if model.running
  then (model, Command.none)
  else (model { running = True, value = 0 }, tick)

updateOnTick :: Model -> (Model, Command Message)
updateOnTick model = if model.value > 100
  then (model { running = False }, Command.none)
  else (model { value = model.value + 0.5 }, tick)

update :: Message -> Model -> (Model, Command Message)
update StartTimer = startTimer
update Tick = updateOnTick

view :: Model -> Element
view model =
  center [] $
  column [alignItems Center, spacing 50] [
    progressBar [width $ Fixed 300, height $ Fixed 30, style ProgressBar.Danger] 0 100 model.value,
    button [onPressIf (not model.running) StartTimer, style Button.Danger] "Start"
  ]

main :: IO ()
main = Iced.run [theme Oxocarbon] "ProgressBar" model update view
  where model = Model { value = 0, running = False }
