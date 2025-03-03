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
  canvas_text_new
    contentRaw
    (CFloat x)
    (CFloat y)
    colorPtr
    (CFloat size)
    lineHeightPtr
    horizontalRaw
    verticalRaw
    (valueToNative shaping)
