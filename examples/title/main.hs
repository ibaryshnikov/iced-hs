module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Widget

data Message = Inc | Dec

title :: Int -> String
title value = "Counter " ++ show value

update :: Message -> Int -> Int
update Inc = succ
update Dec = pred

view :: Int -> Element
view _value =
  center [] $
  column [alignItems Center, spacing 10] [
    button [onPress Inc] "Increment",
    text [size 20] "The value is in title",
    button [onPress Dec] "Decrement"
  ]

main :: IO ()
main = Iced.run [] title 0 update view
