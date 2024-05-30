module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Widget

data Message = Inc | Dec

update :: Int -> Message -> Int
update value Inc = value + 1
update value Dec = value - 1

view :: Int -> Element
view value =
  container [centerX Fill, centerY Fill] $
  column [alignItems Center, spacing 10] [
    button [onPress Inc] "Increment",
    text [size 50] $ show value,
    button [onPress Dec] "Decrement"
  ]

main :: IO ()
main = Iced.run [] "Counter" 0 update view
