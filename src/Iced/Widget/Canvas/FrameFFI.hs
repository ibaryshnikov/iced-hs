module Iced.Widget.Canvas.FrameFFI (
  FramePtr,
  useActions,
) where

import Control.Monad
import Foreign
import Foreign.C.Types

import Iced.Advanced.Image (ImagePtr, imageNew)
import Iced.Advanced.Image.Handle qualified as Image
import Iced.Advanced.Svg (SvgPtr, svgNew)
import Iced.Advanced.Svg.Handle qualified as Svg
import Iced.Color
import Iced.Widget.Canvas.Fill
import Iced.Widget.Canvas.FrameAction
import Iced.Widget.Canvas.Path
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Stroke
import Iced.Widget.Canvas.Style
import Iced.Widget.Canvas.TextFFI

data NativeFrame
type FramePtr = Ptr NativeFrame

foreign import ccall "canvas_frame_draw_image"
  draw_image
    :: FramePtr
    -> CFloat -- top left x
    -> CFloat -- top left y
    -> CFloat -- width
    -> CFloat -- height
    -> ImagePtr
    -> IO ()

foreign import ccall "canvas_frame_draw_svg"
  draw_svg
    :: FramePtr
    -> CFloat -- top left x
    -> CFloat -- top left y
    -> CFloat -- width
    -> CFloat -- height
    -> SvgPtr
    -> IO ()

foreign import ccall "canvas_frame_fill"
  frame_fill :: FramePtr -> PathPtr -> FillPtr -> IO ()

foreign import ccall "canvas_frame_fill_rectangle"
  fill_rectangle
    :: FramePtr
    -> CFloat -- top left x
    -> CFloat -- top left y
    -> CFloat -- width
    -> CFloat -- height
    -> FillPtr
    -> IO ()

foreign import ccall "canvas_frame_fill_text"
  fill_text :: FramePtr -> TextPtr -> IO ()

foreign import ccall "canvas_frame_pop_transform"
  pop_transform :: FramePtr -> IO ()

foreign import ccall "canvas_frame_push_transform"
  push_transform :: FramePtr -> IO ()

foreign import ccall "canvas_frame_rotate"
  frame_rotate :: FramePtr -> CFloat -> IO ()

foreign import ccall "canvas_frame_scale"
  frame_scale :: FramePtr -> CFloat -> IO ()

foreign import ccall "canvas_frame_stroke"
  frame_stroke :: FramePtr -> PathPtr -> StrokePtr -> IO ()

useActions :: [Action] -> FramePtr -> IO ()
useActions [] _framePtr = pure ()
useActions (action : remaining) framePtr = do
  applyAction action framePtr
  useActions remaining framePtr

applyAction :: Action -> FramePtr -> IO ()
applyAction action framePtr = do
  case action of
    DrawImage x y width height handle -> drawImage framePtr x y width height =<< handle
    DrawSvg x y width height handle -> drawSvg framePtr x y width height =<< handle
    Fill shapes color -> frameFill framePtr shapes color
    FillText text -> fillText framePtr text
    PushTransform -> push_transform framePtr
    PopTransform -> pop_transform framePtr
    Rotate angle -> frameRotate framePtr angle
    Scale value -> frameScale framePtr value
    Stroke shapes color width -> frameStroke framePtr shapes color width

drawImage
  :: FramePtr
  -> Float -- top left x
  -> Float -- top left y
  -> Float -- width
  -> Float -- height
  -> Image.Handle
  -> IO ()
drawImage framePtr x y width height =
  draw_image framePtr (CFloat x) (CFloat y) (CFloat width) (CFloat height) <=< imageNew

drawSvg
  :: FramePtr
  -> Float -- top left x
  -> Float -- top left y
  -> Float -- width
  -> Float -- height
  -> Svg.Handle
  -> IO ()
drawSvg framePtr x y width height =
  draw_svg framePtr (CFloat x) (CFloat y) (CFloat width) (CFloat height) <=< svgNew

frameFill :: FramePtr -> [Shape] -> Color -> IO ()
frameFill framePtr shapes color = do
  path <- newPath shapes
  style <- solid color
  fill <- newCanvasFill style NonZero
  frame_fill framePtr path fill

fillText :: FramePtr -> Text -> IO ()
fillText framePtr = fill_text framePtr <=< newCanvasText

frameRotate :: FramePtr -> Float -> IO ()
frameRotate framePtr = frame_rotate framePtr . CFloat

frameScale :: FramePtr -> Float -> IO ()
frameScale framePtr = frame_scale framePtr . CFloat

frameStroke :: FramePtr -> [Shape] -> Color -> Float -> IO ()
frameStroke framePtr shapes color width = do
  path <- newPath shapes
  style <- solid color
  stroke <- newCanvasStroke style width Butt Miter
  frame_stroke framePtr path stroke
