{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (button, onClick) where

import Foreign
import Foreign.C.String

import Iced.Element

data NativeButton
type SelfPtr = Ptr NativeButton
type Attribute = SelfPtr -> IO SelfPtr

foreign import ccall safe "new_button"
  new_button :: CString -> IO (SelfPtr)

foreign import ccall safe "button_on_press"
  button_on_press :: SelfPtr -> StablePtr a -> IO (SelfPtr)

foreign import ccall safe "button_into_element"
  button_into_element :: SelfPtr -> IO (ElementPtr)

data Button = Button { attributes :: [Attribute], label :: String }

instance IntoNative Button where
  toNative details = do
    labelPtr <- newCString details.label
    selfPtr <- new_button labelPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    button_into_element updatedSelf

button :: [Attribute] -> String -> Element
button attributes label = pack Button { .. }

onClick :: message -> Attribute
onClick message selfPtr = do
  -- pass a callback instead which will create
  -- a new StablePtr for each message separately
  messagePtr <- newStablePtr message
  button_on_press selfPtr messagePtr
