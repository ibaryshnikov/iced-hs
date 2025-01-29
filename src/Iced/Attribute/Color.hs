module Iced.Attribute.Color where

import Iced.Color

class UseColor a where
  color :: Color -> a
