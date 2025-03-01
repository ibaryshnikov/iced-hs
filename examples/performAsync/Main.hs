module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Command qualified as Command
import Iced.Time
import Iced.Widget

data Message = StartTimer | Tick

tick :: Command Message
tick = Command.perform $ do
  delaySecs 2
  pure Tick

update :: Message -> Int -> (Int, Command Message)
update StartTimer = (,tick)
update Tick = (,Command.none) . (+ 1)

view :: Int -> Element
view value =
  container [centerX Fill, centerY Fill] $
    column
      [alignX Center, spacing 10]
      [ text [size 50] $ show value
      , button [onPress StartTimer] "Start timer"
      ]

main :: IO ()
main = Iced.run [] "Perform Async" 0 update view
