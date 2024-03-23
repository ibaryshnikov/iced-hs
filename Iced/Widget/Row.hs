module Iced.Widget.Row (row) where

import Foreign
import Foreign.C.Types

import Iced.Element

foreign import ccall safe "new_row"
  newRow :: CInt -> Ptr Element -> IO (Element)

row :: [Element] -> IO (Element)
row elements = let len = length elements in do
  elementsPtr <- newArray elements
  newRow (fromIntegral len) elementsPtr
