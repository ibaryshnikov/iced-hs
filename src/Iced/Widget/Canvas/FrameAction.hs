module Iced.Widget.Canvas.FrameAction where

import Iced.Color
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Text

data FrameAction
  = FrameFill [Shape] Color
  | FrameFillText Text
  | FramePushTransform
  | FramePopTransform
  | FrameRotate Float
  | FrameScale Float
  | FrameStroke [Shape] Color Float

fill :: [Shape] -> Color -> FrameAction
fill shapes color = FrameFill shapes color

fillText :: Text -> FrameAction
fillText = FrameFillText

pushTransform :: FrameAction
pushTransform = FramePushTransform

popTransform :: FrameAction
popTransform = FramePopTransform

rotate :: Float -> FrameAction
rotate = FrameRotate

scale :: Float -> FrameAction
scale = FrameScale

stroke :: [Shape] -> Color -> Float -> FrameAction
stroke shapes color width = FrameStroke shapes color width
