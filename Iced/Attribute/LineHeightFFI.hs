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

foreign import ccall "line_height_relative"
  line_height_relative :: CFloat -> IO LineHeightPtr

foreign import ccall "line_height_absolute"
  line_height_absolute :: CFloat -> IO LineHeightPtr

instance ValueToNativeIO LineHeight LineHeightPtr where
  valueToNativeIO (Relative value) = line_height_relative (CFloat value)
  valueToNativeIO (Absolute value) = line_height_absolute (CFloat value)
