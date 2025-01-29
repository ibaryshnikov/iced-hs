module Iced.Widget.Canvas.Frame where

import Foreign.C.Types

import Iced.Attribute.Text
import Iced.Color
import Iced.Widget.Canvas.Fill
import Iced.Widget.Canvas.FramePtr
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Path
import Iced.Widget.Canvas.Stroke
import Iced.Widget.Canvas.Style

foreign import ccall "canvas_frame_fill"
  canvas_frame_fill :: FramePtr -> PathPtr -> FillPtr -> IO ()

foreign import ccall "canvas_frame_fill_rectangle"
  canvas_frame_fill_rectangle
    :: FramePtr
    -> CFloat -- top_left x
    -> CFloat -- top_left y
    -> CFloat -- size width
    -> CFloat -- size height
    -> FillPtr
    -> IO ()

foreign import ccall "canvas_frame_stroke"
  canvas_frame_stroke :: FramePtr -> PathPtr -> StrokePtr -> IO ()

--data FrameAction
--  = FrameFill Path Fill
  -- | FrameFillRectangle { topLeft :: Point, size :: Size, fill :: Fill }
  -- | FrameStroke Path Stroke
  -- | FrameFillText Text

data Text = Text {
    content :: String,
    -- position :: Point,
    color :: Color,
    size :: Float,
    lineHeight :: LineHeight,
    -- font :: Font
    horizontalAlignment :: Horizontal,
    verticalAlignment :: Vertical,
    shaping :: Shaping
}

data LineHeight  = Relative Float | Absolute Float

data Horizontal = Left | HCenter | Right
data Vertical = Top | VCenter | Bottom

frameFill :: FramePtr -> [Shape] -> Color -> IO ()
frameFill framePtr shapes color = do
  path <- newPath shapes
  style <- solid color
  fill <- newCanvasFill style NonZero
  canvas_frame_fill framePtr path fill

frameStroke :: FramePtr -> [Shape] -> Color -> Float -> IO ()
frameStroke framePtr shapes color width = do
  path <- newPath shapes
  style <- solid color
  stroke <- newCanvasStroke style width Butt Miter
  canvas_frame_stroke framePtr path stroke
