{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Button (button, onClick) where

import Foreign
import Foreign.C.String

import Iced.Element

data NativeButton
type ButtonPtr = Ptr NativeButton

foreign import ccall safe "new_button"
  new_button :: CString -> IO (ButtonPtr)

foreign import ccall safe "button_on_press"
  button_on_press :: ButtonPtr -> StablePtr a -> IO (ButtonPtr)

foreign import ccall safe "button_into_element"
  button_into_element :: ButtonPtr -> IO (ElementPtr)

data Button = Button { label :: String, attributes :: [ButtonPtr -> IO ButtonPtr] }

instance IntoNative Button where
  toNative details = do
    labelPtr <- newCString details.label
    buttonPtr <- new_button labelPtr
    updatedButton <- applyAttributes buttonPtr details.attributes
    button_into_element updatedButton

button :: Show label => [ButtonPtr -> IO ButtonPtr] -> label -> Element
button attributes label = pack Button { label = show label, attributes = attributes }

onClick :: message -> ButtonPtr -> IO (ButtonPtr)
onClick message buttonPtr = do
  -- pass a callback instead which will create
  -- a new StablePtr for each message separately
  messagePtr <- newStablePtr message
  button_on_press buttonPtr messagePtr
