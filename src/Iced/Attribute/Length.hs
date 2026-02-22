module Iced.Attribute.Length where

import Data.Word

data Length = Fill | FillPortion Word16 | Shrink | Fixed Float

class UseWidth value attribute where
  width :: value -> attribute

class UseHeight value attribute where
  height :: value -> attribute

class UseLength value attribute where
  addLength :: value -> attribute

class UseGirth value attribute where
  girth :: value -> attribute
