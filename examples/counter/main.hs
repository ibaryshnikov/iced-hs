module Main where

import Iced
import Iced.Alignment
import Iced.Widget
import qualified Iced.Widget.Column as Column
import qualified Iced.Widget.Text as Text

data Message = Inc | Dec

update :: Int -> Message -> Int
update value Inc = value + 1
update value Dec = value - 1

view :: Int -> Element
view value = column [Column.padding 20, Column.alignItems Center] [
    button [onClick Inc] "Inc",
    text [Text.size 50] $ show value,
    button [onClick Dec] "Dec"
  ]

main :: IO ()
main = Iced.run [] "Counter" 0 update view
