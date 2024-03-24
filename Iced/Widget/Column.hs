module Iced.Widget.Column (column) where

import Foreign
import Foreign.C.Types

import Iced.Element

foreign import ccall safe "new_column"
  new_column :: CInt -> Ptr Element -> IO (Element)

column :: [Element] -> IO (Element)
column elements = let len = length elements in do
  elementsPtr <- newArray elements
  columnPtr <- new_column (fromIntegral len) elementsPtr
  free elementsPtr
  return columnPtr
