module Main where

import Iced
import Iced.Widget

data Message = Inc | Dec

update :: Int -> Message -> Int
update value Inc = value + 1
update value Dec = value - 1

view :: Int -> Element
view value = column [] [
    button [onClick Inc] "Inc",
    text [] $ show value,
    button [onClick Dec] "Dec"
  ]

main :: IO ()
main = Iced.run "Counter" 0 update view
