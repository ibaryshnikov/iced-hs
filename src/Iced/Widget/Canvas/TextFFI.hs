{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas.TextFFI (
  TextPtr,
  newCanvasText,
  -- reexports
  module Iced.Widget.Canvas.Text,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.AlignmentFFI
import Iced.Attribute.Internal
import Iced.Attribute.LineHeightFFI
import Iced.ColorFFI
import Iced.Widget.Canvas.Text

data NativeText
type TextPtr = Ptr NativeText

-- content_ptr: *mut c_char,
-- position_x: c_float,
-- position_y: c_float,
-- color_ptr: *mut Color,
-- size: c_float,
-- line_height_ptr: *mut LineHeight,
-- horizontal_alignment_ptr: *mut Alignment,
-- vertical_alignment_ptr: *mut Alignment,
-- shaping_raw: c_uchar,

-- content position_x position_y color size
foreign import ccall "canvas_text_new"
  canvas_text_new
    :: CString -- content
    -> CFloat -- position x
    -> CFloat -- position y
    -> ColorPtr
    -> CFloat -- size
    -> LineHeightPtr
    -> AlignmentPtr -- horizontal
    -> AlignmentPtr -- vertical
    -> CUChar -- shaping
    -> IO TextPtr

newCanvasText
  :: Text
  -> IO TextPtr
newCanvasText Text{lineHeight = textLineHeight, ..} = do
  contentRaw <- newCString content
  colorPtr <- valueToNativeIO color
  lineHeightPtr <- valueToNativeIO textLineHeight
  horizontalRaw <- valueToNativeIO horizontal
  verticalRaw <- valueToNativeIO vertical
  canvas_text_new contentRaw (CFloat x) (CFloat y) colorPtr (CFloat size) lineHeightPtr horizontalRaw verticalRaw (valueToNative shaping)
