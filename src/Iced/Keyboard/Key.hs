module Iced.Keyboard.Key where

import Iced.Keyboard.LogicalKey
import Iced.Keyboard.PhysicalKey

data Key = Key {
  logical :: LogicalKey,
  physical :: PhysicalKey
} deriving Show
