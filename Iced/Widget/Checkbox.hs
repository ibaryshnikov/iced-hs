{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Checkbox (checkbox, onToggle) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element

data NativeCheckbox
type SelfPtr = Ptr NativeCheckbox
type Attribute = SelfPtr -> IO SelfPtr

foreign import ccall safe "new_checkbox"
  new_checkbox :: CString -> CBool -> IO (SelfPtr)

foreign import ccall safe "checkbox_on_toggle"
  checkbox_on_toggle :: SelfPtr -> FunPtr (NativeOnToggle a) -> IO (SelfPtr)

foreign import ccall safe "checkbox_into_element"
  checkbox_into_element :: SelfPtr -> IO (ElementPtr)

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle callback c_bool = do
  newStablePtr $ callback $ toBool c_bool

type OnToggle message = Bool -> message

data Checkbox = Checkbox {
  attributes :: [Attribute],
  label :: String,
  value :: Bool
}

instance IntoNative Checkbox where
  toNative details = do
    let checked = fromBool details.value
    labelPtr <- newCString details.label
    selfPtr <- new_checkbox labelPtr checked
    updatedSelf <- applyAttributes selfPtr details.attributes
    checkbox_into_element updatedSelf

checkbox :: [Attribute] -> String -> Bool -> Element
checkbox attributes label value = pack Checkbox { .. }

onToggle :: OnToggle message -> Attribute
onToggle callback selfPtr = do
  onTogglePtr <- makeCallback $ wrapOnToggle callback
  checkbox_on_toggle selfPtr onTogglePtr
