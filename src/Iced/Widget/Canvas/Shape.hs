{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas.Shape (
  Shape,
  addToPath,
  circle,
  lineTo,
  moveTo,
  rectangle,
) where

import Iced.Widget.Canvas.PathBuilder (PathBuilderPtr)
import Iced.Widget.Canvas.PathBuilder qualified as PathBuilder

data Shape
  = Circle {x :: Float, y :: Float, radius :: Float}
  | LineTo {x :: Float, y :: Float}
  | MoveTo {x :: Float, y :: Float}
  | Rectangle
      { top_left_x :: Float
      , top_left_y :: Float
      , width :: Float
      , height :: Float
      }

addToPath :: Shape -> PathBuilderPtr -> IO ()
addToPath shape builder = do
  case shape of
    Circle{..} -> PathBuilder.circle builder x y radius
    LineTo{..} -> PathBuilder.lineTo builder x y
    MoveTo{..} -> PathBuilder.moveTo builder x y
    Rectangle{..} ->
      PathBuilder.rectangle builder top_left_x top_left_y width height

circle :: Float -> Float -> Float -> Shape
circle = Circle

lineTo :: Float -> Float -> Shape
lineTo = LineTo

moveTo :: Float -> Float -> Shape
moveTo = MoveTo

rectangle :: Float -> Float -> Float -> Float -> Shape
rectangle = Rectangle
