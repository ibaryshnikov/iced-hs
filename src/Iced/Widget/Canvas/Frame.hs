module Iced.Widget.Canvas.Frame (
  Action,
  drawImage,
  fill,
  fillText,
  pushTransform,
  popTransform,
  rotate,
  scale,
  stroke,
) where

import Iced.Advanced.Image.Handle
import Iced.Color
import Iced.Widget.Canvas.FrameAction
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Text

drawImage
  :: IntoHandle a
  => Float -- top left x
  -> Float -- top left y
  -> Float -- width
  -> Float -- height
  -> a
  -> Action
drawImage x y width height input = DrawImage x y width height $ intoHandle input

fill :: [Shape] -> Color -> Action
fill shapes color = Fill shapes color

fillText :: Text -> Action
fillText = FillText

pushTransform :: Action
pushTransform = PushTransform

popTransform :: Action
popTransform = PopTransform

rotate :: Float -> Action
rotate = Rotate

scale :: Float -> Action
scale = Scale

stroke :: [Shape] -> Color -> Float -> Action
stroke shapes color width = Stroke shapes color width
