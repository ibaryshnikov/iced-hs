module Iced.Widget.Canvas.FrameAction (
  Action (..),
) where

import Iced.Advanced.Image.Handle qualified as Image
import Iced.Advanced.Svg.Handle qualified as Svg
import Iced.Color (Color)
import Iced.Widget.Canvas.Shape (Shape)
import Iced.Widget.Canvas.Text (Text)

data Action
  = DrawImage Float Float Float Float (IO Image.Handle)
  | DrawSvg Float Float Float Float (IO Svg.Handle)
  | Fill [Shape] Color
  | FillText Text
  | PushTransform
  | PopTransform
  | Rotate Float
  | Scale Float
  | Stroke [Shape] Color Float
