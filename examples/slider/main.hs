module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Message = Changed Int

update :: Int -> Message -> Int
update _oldValue (Changed value) = value

view :: Int -> Element
view value =
  container [centerX, centerY, width Fill, height Fill] $
  column [alignItems Center] [
    text [] $ show value,
    slider [width (Fixed 100)] 0 100 value Changed
  ]

main :: IO ()
main = Iced.run [] "Slider" 50 update view
