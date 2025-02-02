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

foreign import ccall "length_fill"
  length_fill :: IO LengthPtr

foreign import ccall "length_fill_portion"
  length_fill_portion :: CUShort -> IO LengthPtr

foreign import ccall "length_shrink"
  length_shrink :: IO LengthPtr

foreign import ccall "length_fixed"
  length_fixed :: CFloat -> IO LengthPtr

instance ValueToNativeIO Length LengthPtr where
  valueToNativeIO len = case len of
    Fill -> length_fill
    FillPortion value -> length_fill_portion (CUShort value)
    Shrink -> length_shrink
    Fixed value -> length_fixed (CFloat value)
