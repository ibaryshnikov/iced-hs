module Iced.Attribute.TextFFI (
  -- reexports
  module Iced.Attribute.Text,
  --
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.Text

instance ValueToNative Shaping CUChar where
  valueToNative shaping = case shaping of
    Basic    -> 0
    Advanced -> 1
