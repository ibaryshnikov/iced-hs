module Iced.Widget.Button (button) where

import Foreign
import Foreign.C.String

import Iced.Element

foreign import ccall safe "new_button"
  newButton :: CString -> StablePtr a -> IO (Element)

button :: String -> a -> IO (Element)
button label message = do
  labelPtr <- newCString label
  messagePtr <- newStablePtr message
  newButton labelPtr messagePtr
