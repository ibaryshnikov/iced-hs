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
  = Circle Float Float Float -- x y radius
  | LineTo Float Float -- x y
  | MoveTo Float Float -- x y
  | Rectangle Float Float Float Float -- topLeftX topLeftY width height

addToPath :: Shape -> PathBuilderPtr -> IO ()
addToPath shape builder = do
  case shape of
    Circle x y radius -> PathBuilder.circle builder x y radius
    LineTo x y -> PathBuilder.lineTo builder x y
    MoveTo x y -> PathBuilder.moveTo builder x y
    Rectangle topLeftX topLeftY width height ->
      PathBuilder.rectangle builder topLeftX topLeftY width height

-- x y radius
circle :: Float -> Float -> Float -> Shape
circle = Circle

-- x y
lineTo :: Float -> Float -> Shape
lineTo = LineTo

-- x y
moveTo :: Float -> Float -> Shape
moveTo = MoveTo

-- topLeftX topLeftY width height
rectangle :: Float -> Float -> Float -> Float -> Shape
rectangle = Rectangle
