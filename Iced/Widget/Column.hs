module Iced.Widget.Column (column) where

import Foreign
import Foreign.C.Types

import Iced.Element

foreign import ccall safe "new_column"
  newColumn :: CInt -> Ptr Element -> IO (Element)

column :: [Element] -> IO (Element)
column elements = let len = length elements in do
  elementsPtr <- newArray elements
  newColumn (fromIntegral len) elementsPtr
