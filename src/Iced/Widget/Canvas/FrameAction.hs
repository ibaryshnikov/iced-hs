module Iced.Widget.Canvas.FrameAction where

import Iced.Color
import Iced.Widget.Canvas.Shape

data FrameAction
  = FrameFill [Shape] Color
  | FrameStroke [Shape] Color Float

fill :: [Shape] -> Color -> FrameAction
fill shapes color = FrameFill shapes color

stroke :: [Shape] -> Color -> Float -> FrameAction
stroke shapes color width = FrameStroke shapes color width
