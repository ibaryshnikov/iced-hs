module Iced.Keyboard where

import Iced.Keyboard.Key

-- Enum is 0-indexed
keyFromInt :: Int -> Named
keyFromInt = toEnum
