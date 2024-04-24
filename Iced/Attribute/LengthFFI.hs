module Iced.Attribute.LengthFFI (
  LengthPtr,
  -- reexports
  module Iced.Attribute.Length,
  --
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.Length

data NativeLength
type LengthPtr = Ptr NativeLength

foreign import ccall safe "length_fill"
  length_fill :: LengthPtr

foreign import ccall safe "length_fill_portion"
  length_fill_portion :: CUShort -> LengthPtr

foreign import ccall safe "length_shrink"
  length_shrink :: LengthPtr

foreign import ccall safe "length_fixed"
  length_fixed :: CFloat -> LengthPtr

instance ValueToNative Length LengthPtr where
  valueToNative len = case len of
    Fill -> length_fill
    FillPortion value -> length_fill_portion (CUShort value)
    Shrink -> length_shrink
    Fixed value -> length_fixed (CFloat value)
