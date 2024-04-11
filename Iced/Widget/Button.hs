{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (button, onPress) where

import Foreign
import Foreign.C.String

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeButton
type SelfPtr = Ptr NativeButton
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute message = OnPress message | Width Length | Height Length

foreign import ccall safe "button_new"
  button_new :: CString -> IO (SelfPtr)

foreign import ccall safe "button_on_press"
  button_on_press :: SelfPtr -> StablePtr a -> IO (SelfPtr)

foreign import ccall safe "button_width"
  button_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "button_height"
  button_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "button_into_element"
  button_into_element :: SelfPtr -> IO (ElementPtr)

data Button message = Button {
  attributes :: [Attribute message],
  label :: String
}

instance IntoNative (Button message) where
  toNative details = do
    labelPtr <- newCString details.label
    selfPtr <- button_new labelPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    button_into_element updatedSelf

instance UseAttribute SelfPtr (Attribute message) where
  useAttribute selfPtr attribute = do
    case attribute of
      OnPress message -> useOnPress message selfPtr
      Width len -> useWidth len selfPtr
      Height len -> useHeight len selfPtr

instance UseWidth (Attribute message) where
  width len = Width len

instance UseHeight (Attribute message) where
  height len = Height len

button :: [Attribute message] -> String -> Element
button attributes label = pack Button { .. }

onPress :: message -> Attribute message
onPress message = OnPress message

useOnPress :: message -> AttributeFn
useOnPress message selfPtr = do
  -- pass a callback instead which will create
  -- a new StablePtr for each message separately
  messagePtr <- newStablePtr message
  button_on_press selfPtr messagePtr

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  button_width selfPtr nativeLen

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  button_height selfPtr nativeLen
