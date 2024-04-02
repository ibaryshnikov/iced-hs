module Iced.Length where

import Data.Word

data Length = Fill | FillPortion Word16 | Shrink | Fixed Float
