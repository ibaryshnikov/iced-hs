{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Checkbox (checkbox, onToggle) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element

data NativeCheckbox
type CheckboxPtr = Ptr NativeCheckbox

foreign import ccall safe "new_checkbox"
  new_checkbox :: CString -> CBool -> IO (CheckboxPtr)

foreign import ccall safe "checkbox_on_toggle"
  checkbox_on_toggle :: CheckboxPtr -> FunPtr (NativeOnToggle a) -> IO (CheckboxPtr)

foreign import ccall safe "checkbox_into_element"
  checkbox_into_element :: CheckboxPtr -> IO (ElementPtr)

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle callback c_bool = do
  newStablePtr $ callback $ toBool c_bool

type OnToggle message = Bool -> message

data Checkbox = Checkbox {
  label :: String,
  value :: Bool,
  attributes :: [CheckboxPtr -> IO CheckboxPtr]
}

instance IntoNative Checkbox where
  toNative details = do
    let checked = fromBool details.value
    labelPtr <- newCString details.label
    checkboxPtr <- new_checkbox labelPtr checked
    updatedCheckbox <- applyAttributes checkboxPtr details.attributes
    checkbox_into_element updatedCheckbox

checkbox :: Show label => [CheckboxPtr -> IO CheckboxPtr] -> label -> Bool -> Element
checkbox attributes label value = pack Checkbox { label = show label, value, attributes }

onToggle :: OnToggle message -> CheckboxPtr -> IO (CheckboxPtr)
onToggle callback checkboxPtr = do
  onTogglePtr <- makeCallback $ wrapOnToggle callback
  checkbox_on_toggle checkboxPtr onTogglePtr
