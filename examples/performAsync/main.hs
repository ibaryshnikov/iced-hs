module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Command qualified as Command
import Iced.Extra
import Iced.Time
import Iced.Widget

data Message = StartTimer | Tick

tick :: Command Message
tick = Command.perform $ do
  delaySecs 2
  pure Tick

update :: Int -> Message -> (Int, Command Message)
update value StartTimer = (value, tick)
update value Tick = (value + 1, Command.none)

view :: Int -> Element
view value =
  center [] $
  column [alignItems Center, spacing 10] [
    text [size 50] $ show value,
    button [onPress StartTimer] "Start timer"
  ]

main :: IO ()
main = Iced.run [] "Perform Async" 0 update view
