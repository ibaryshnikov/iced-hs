module Iced.Widget.Canvas.Frame (
  Action,
  drawImage,
  drawSvg,
  fill,
  fillText,
  pushTransform,
  popTransform,
  rotate,
  scale,
  stroke,
) where

import Iced.Advanced.Image.Handle qualified as Image
import Iced.Advanced.Svg.Handle qualified as Svg
import Iced.Color
import Iced.Widget.Canvas.FrameAction
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Text

drawImage
  :: Image.IntoHandle a
  => Float -- top left x
  -> Float -- top left y
  -> Float -- width
  -> Float -- height
  -> a
  -> Action
drawImage x y width height input = DrawImage x y width height $ Image.intoHandle input

drawSvg
  :: Svg.IntoHandle a
  => Float -- top left x
  -> Float -- top left y
  -> Float -- width
  -> Float -- height
  -> a
  -> Action
drawSvg x y width height input = DrawSvg x y width height $ Svg.intoHandle input

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
