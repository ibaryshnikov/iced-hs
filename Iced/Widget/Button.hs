module Iced.Widget.Button (button) where

import Foreign
import Foreign.C.String

import Iced.Element

foreign import ccall safe "new_button"
  new_button :: CString -> StablePtr a -> IO (Element)

button :: String -> a -> IO (Element)
button label message = do
  labelPtr <- newCString label
  -- pass a callback instead which will create
  -- a new StablePtr for each message separately
  messagePtr <- newStablePtr message
  new_button labelPtr messagePtr
