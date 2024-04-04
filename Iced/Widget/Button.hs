{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (button, onClick) where

import Foreign
import Foreign.C.String

import Iced.Element
import Iced.Length
import Iced.LengthFFI

data NativeButton
type SelfPtr = Ptr NativeButton
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute message = OnClick message | Height Length | Width Length

foreign import ccall safe "new_button"
  new_button :: CString -> IO (SelfPtr)

foreign import ccall safe "button_on_press"
  button_on_press :: SelfPtr -> StablePtr a -> IO (SelfPtr)

foreign import ccall safe "button_height"
  button_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "button_width"
  button_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "button_into_element"
  button_into_element :: SelfPtr -> IO (ElementPtr)

data Button message = Button {
  attributes :: [Attribute message],
  label :: String
}

instance IntoNative (Button message) where
  toNative details = do
    labelPtr <- newCString details.label
    selfPtr <- new_button labelPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    button_into_element updatedSelf

instance UseAttribute SelfPtr (Attribute message) where
  useAttribute selfPtr attribute = do
    case attribute of
      OnClick message -> useOnClick message selfPtr
      Height len -> useHeight len selfPtr
      Width len -> useWidth len selfPtr

instance UseLength (Attribute message) where
  width len = Width len
  height len = Height len

button :: [Attribute message] -> String -> Element
button attributes label = pack Button { .. }

onClick :: message -> Attribute message
onClick message = OnClick message

useOnClick :: message -> AttributeFn
useOnClick message selfPtr = do
  -- pass a callback instead which will create
  -- a new StablePtr for each message separately
  messagePtr <- newStablePtr message
  button_on_press selfPtr messagePtr

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  button_height selfPtr nativeLen

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  button_width selfPtr nativeLen
