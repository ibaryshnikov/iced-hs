module Iced.Attribute.Text where

import Foreign.C.Types

import Iced.Attribute.Internal

data Shaping = Basic | Advanced

instance ValueToNative Shaping CUChar where
  valueToNative shaping = case shaping of
    Basic -> 0
    Advanced -> 1
