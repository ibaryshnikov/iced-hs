module Iced.Attribute.LineHeightFFI (
  LineHeightPtr,
  lineHeightToNative,
  -- reexports
  module Iced.Attribute.LineHeight,
  --
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.LineHeight

data NativeLineHeight
type LineHeightPtr = Ptr NativeLineHeight

foreign import ccall safe "line_height_relative"
  line_height_relative :: CFloat -> LineHeightPtr

foreign import ccall safe "line_height_absolute"
  line_height_absolute :: CFloat -> LineHeightPtr

lineHeightToNative :: LineHeight -> LineHeightPtr
lineHeightToNative (Relative value) = line_height_relative (CFloat value)
lineHeightToNative (Absolute value) = line_height_absolute (CFloat value)
