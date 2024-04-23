module Iced.Widget.Canvas.Frame where

import Foreign.C.Types

import Iced.Color
import Iced.Widget.Canvas.Fill
import Iced.Widget.Canvas.FramePtr
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Path

foreign import ccall safe "canvas_frame_fill"
  canvas_frame_fill :: FramePtr -> PathPtr -> FillPtr -> IO ()

foreign import ccall safe "canvas_frame_fill_rectangle"
  canvas_frame_fill_rectangle
    :: FramePtr
    -> CFloat -- top_left x
    -> CFloat -- top_left y
    -> CFloat -- size width
    -> CFloat -- size height
    -> FillPtr
    -> IO ()

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

data Shaping = Basic | Advanced

frameFill :: FramePtr -> [Shape] -> Color -> IO ()
frameFill framePtr shapes color = do
  path <- newPath shapes
  style <- canvasStyleSolid color
  fill <- newCanvasFill style NonZero
  canvas_frame_fill framePtr path fill
