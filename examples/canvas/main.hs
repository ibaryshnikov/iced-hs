{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Color
import Iced.Widget
import Iced.Widget.Canvas qualified as Canvas
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.FrameAction

data Model = Model { state :: Canvas.State }

data Message

update :: Model -> Message -> Model
update model _message = model

view :: Model -> Element
view model = canvas [width Fill, height Fill] shapes model.state

shapes :: [FrameAction]
shapes = [
    fill [
      moveTo 300 300,
      lineTo 300 350,
      lineTo 350 350,
      circle 500 500 50,
      circle 200 200 10,
      rectangle 30 30 50 50
    ] $ rgb8 0 0 0,
    fill [
      circle 700 700 50
    ] $ rgb8 150 200 50,
    fill [
      rectangle 500 200 100 100
    ] $ rgba8 255 0 0 0.4
  ]

main :: IO ()
main = do
  state <- Canvas.newState
  let model = Model { state = state }
  Iced.run [] "Canvas" model update view
