{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget
import Iced.Widget.Canvas.Shape

data Model = Model { state :: CanvasState }

data Message

update :: Model -> Message -> Model
update model _message = model

view :: Model -> Element
view model = canvas [width Fill, height Fill] shapes model.state

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
main = do
  state <- newCanvasState
  let model = Model { state = state }
  Iced.run [] "Canvas" model update view
  freeCanvasState state
