module Iced.Widget.Canvas.Frame where

import Control.Monad
import Foreign.C.Types

import Iced.Color
import Iced.Widget.Canvas.Fill
import Iced.Widget.Canvas.FramePtr
import Iced.Widget.Canvas.Path
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Stroke
import Iced.Widget.Canvas.Style
import Iced.Widget.Canvas.TextFFI

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

foreign import ccall "canvas_frame_fill_text"
  canvas_frame_fill_text :: FramePtr -> TextPtr -> IO ()

foreign import ccall "canvas_frame_pop_transform"
  canvas_frame_pop_transform :: FramePtr -> IO ()

foreign import ccall "canvas_frame_push_transform"
  canvas_frame_push_transform :: FramePtr -> IO ()

foreign import ccall "canvas_frame_rotate"
  canvas_frame_rotate :: FramePtr -> CFloat -> IO ()

foreign import ccall "canvas_frame_scale"
  canvas_frame_scale :: FramePtr -> CFloat -> IO ()

foreign import ccall "canvas_frame_stroke"
  canvas_frame_stroke :: FramePtr -> PathPtr -> StrokePtr -> IO ()

frameFill :: FramePtr -> [Shape] -> Color -> IO ()
frameFill framePtr shapes color = do
  path <- newPath shapes
  style <- solid color
  fill <- newCanvasFill style NonZero
  canvas_frame_fill framePtr path fill

frameFillText :: FramePtr -> Text -> IO ()
frameFillText framePtr = canvas_frame_fill_text framePtr <=< newCanvasText

framePopTransform :: FramePtr -> IO ()
framePopTransform = canvas_frame_pop_transform

framePushTransform :: FramePtr -> IO ()
framePushTransform = canvas_frame_push_transform

frameRotate :: FramePtr -> Float -> IO ()
frameRotate framePtr = canvas_frame_rotate framePtr . CFloat

frameScale :: FramePtr -> Float -> IO ()
frameScale framePtr = canvas_frame_scale framePtr . CFloat

frameStroke :: FramePtr -> [Shape] -> Color -> Float -> IO ()
frameStroke framePtr shapes color width = do
  path <- newPath shapes
  style <- solid color
  stroke <- newCanvasStroke style width Butt Miter
  canvas_frame_stroke framePtr path stroke
