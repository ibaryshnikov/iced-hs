{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Iced.Widget.Canvas.PathBuilder as PathBuilder

data Shape =
  Circle { x :: Float, y :: Float, radius :: Float } |
  LineTo { x :: Float, y :: Float } |
  MoveTo { x :: Float, y :: Float } |
  Rectangle {
    top_left_x :: Float,
    top_left_y :: Float,
    width :: Float,
    height :: Float
  }

addToPath :: Shape -> PathBuilderPtr -> IO ()
addToPath shape builder = do
  case shape of
    Circle { .. } -> PathBuilder.circle builder x y radius
    LineTo { .. } -> PathBuilder.lineTo builder x y
    MoveTo { .. } -> PathBuilder.moveTo builder x y
    Rectangle { .. } ->
      PathBuilder.rectangle builder top_left_x top_left_y width height

circle :: Float -> Float -> Float -> Shape
circle x y radius = Circle { .. }

lineTo :: Float -> Float -> Shape
lineTo x y = LineTo { x, y }

moveTo :: Float -> Float -> Shape
moveTo x y = MoveTo { x, y }

rectangle :: Float -> Float -> Float -> Float -> Shape
rectangle top_left_x top_left_y width height = Rectangle { .. }
