module Iced.Attribute.LineHeightFFI (
  LineHeightPtr,
  -- reexports
  module Iced.Attribute.LineHeight,
  --
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LineHeight

data NativeLineHeight
type LineHeightPtr = Ptr NativeLineHeight

foreign import ccall safe "line_height_relative"
  line_height_relative :: CFloat -> LineHeightPtr

foreign import ccall safe "line_height_absolute"
  line_height_absolute :: CFloat -> LineHeightPtr

instance ValueToNative LineHeight LineHeightPtr where
  valueToNative (Relative value) = line_height_relative (CFloat value)
  valueToNative (Absolute value) = line_height_absolute (CFloat value)
