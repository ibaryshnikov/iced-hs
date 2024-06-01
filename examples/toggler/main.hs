module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Message = Toggle Bool

update :: Bool -> Message -> Bool
update _oldValue (Toggle value) = value

view :: Bool -> Element
view value =
  center [] $
  toggler [width Shrink] (lights value) value Toggle

lights :: Bool -> String
lights True = "Lights on"
lights False = "Lights off"

main :: IO ()
main = Iced.run [] "Toggler" False update view
