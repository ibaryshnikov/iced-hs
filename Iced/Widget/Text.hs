module Iced.Widget.Text (text) where

import Foreign.C.String

import Iced.Element

foreign import ccall safe "new_text"
  newText :: CString -> IO (Element)

text :: String -> IO (Element)
text label = do
  labelPtr <- newCString label
  newText labelPtr
