module Iced.ColorFFI where

import Foreign

data NativeColor
data ColorPtr = Ptr

-- r g b a
-- values must be in 0.0 - 1.0 range
-- in debug mode panics if any value is out of range
foreign import ccall "color_new"
  color_new :: CFloat -> CFloat -> CFloat -> CFloat -> ColorPtr

foreign import ccall "color_from_rgba"
  color_from_rgba :: CFloat -> CFloat -> CFloat -> CFloat -> ColorPtr
