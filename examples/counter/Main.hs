module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget

data Message = Inc | Dec

update :: Message -> Int -> Int
update Inc = succ
update Dec = pred

view :: Int -> Element
view value =
  container [centerX Fill, centerY Fill] $
    column
      [alignX Center, spacing 10]
      [ button [onPress Inc] "Increment"
      , text [size 50] $ show value
      , button [onPress Dec] "Decrement"
      ]

main :: IO ()
main = Iced.run [theme GruvboxLight] "Counter" 0 update view
