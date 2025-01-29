module Iced.Attribute.Icon where

import Data.Word

class UseIcon attribute where
  icon :: Word32 -> attribute
