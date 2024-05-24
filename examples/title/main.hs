module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Message = Inc | Dec

title :: Int -> String
title value = "Counter " ++ show value

update :: Int -> Message -> Int
update value Inc = value + 1
update value Dec = value - 1

view :: Int -> Element
view _value =
  container [centerX Fill, centerY Fill] $
  column [alignItems Center, spacing 10] [
    button [onPress Inc] "Increment",
    text [size 20] "The value is in title",
    button [onPress Dec] "Decrement"
  ]

main :: IO ()
main = Iced.run [] title 0 update view
