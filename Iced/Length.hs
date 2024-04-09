module Iced.Length where

import Data.Word

data Length = Fill | FillPortion Word16 | Shrink | Fixed Float

class UseWidth a where
  width :: Length -> a

class UseHeight a where
  height :: Length -> a
