module Main where

import Iced
import Iced.Attribute
import Iced.Command
import Iced.Time
import Iced.Widget

data Message = StartTimer | Tick

update :: Int -> Message -> (Int, Command Message)
update value StartTimer = (value, Perform future)
  where duration = durationFromSecs 2
        future = sleep duration Tick
update value Tick = (value + 1, None)

view :: Int -> Element
view value =
  container [centerX, centerY, width Fill, height Fill] $
  column [alignItems Center, spacing 10] [
    text [size 50] $ show value,
    button [onPress StartTimer] "Start timer"
  ]

main :: IO ()
main = Iced.run [] "Perform Async" 0 update view
