module Main where

import Iced
import Iced.Widget
import Iced.Widget.Canvas.Shape

data Model = Model
data Message

update :: Model -> Message -> Model
update model _message = model

view :: Model -> Element
view _model = canvas [] shapes

shapes :: [Shape]
shapes = [
    moveTo 300 300,
    lineTo 300 350,
    lineTo 350 350,
    circle 500 500 50,
    circle 200 200 10,
    rectangle 30 30 50 50
  ]

main :: IO ()
main = Iced.run "Canvas" Model update view
