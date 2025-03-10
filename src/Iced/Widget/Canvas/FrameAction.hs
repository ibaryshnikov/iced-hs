module Iced.Widget.Canvas.FrameAction (
  Action (..),
) where

import Iced.Advanced.Image.Handle (Handle)
import Iced.Color (Color)
import Iced.Widget.Canvas.Shape (Shape)
import Iced.Widget.Canvas.Text (Text)

data Action
  = DrawImage Float Float Float Float (IO Handle)
  | Fill [Shape] Color
  | FillText Text
  | PushTransform
  | PopTransform
  | Rotate Float
  | Scale Float
  | Stroke [Shape] Color Float
