module Iced.Widget.Canvas.FrameAction where

import Iced.Color
import Iced.Widget.Canvas.Shape

data FrameAction = FrameFill [Shape] Color

fill :: [Shape] -> Color -> FrameAction
fill shapes color = FrameFill shapes color
