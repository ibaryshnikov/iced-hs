{-# LANGUAGE RecordWildCards #-}

module Iced.Attribute.PaddingFFI (
  PaddingPtr,
  -- reexports
  module Iced.Attribute.Padding,
  --
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.Padding

data NativePadding
type PaddingPtr = Ptr NativePadding

-- top right bottom left
foreign import ccall "padding_new"
  padding_new :: CFloat -> CFloat -> CFloat -> CFloat -> IO PaddingPtr

instance ValueToNativeIO Padding PaddingPtr where
  valueToNativeIO Padding { .. } = padding_new (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
