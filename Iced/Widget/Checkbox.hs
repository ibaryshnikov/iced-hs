{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Checkbox (
  checkbox,
  onToggle,
  onToggleIf,
  style,
  Style(..),
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element

data NativeCheckbox
type SelfPtr = Ptr NativeCheckbox
type AttributeFn = SelfPtr -> IO SelfPtr
data NativeStyle
type StylePtr = Ptr NativeStyle

data Attribute message = AddOnToggle (OnToggle message) | AddStyle Style | None

foreign import ccall safe "new_checkbox"
  new_checkbox :: CString -> CBool -> IO (SelfPtr)

foreign import ccall safe "checkbox_on_toggle"
  checkbox_on_toggle :: SelfPtr -> FunPtr (NativeOnToggle a) -> IO (SelfPtr)

foreign import ccall safe "checkbox_style"
  checkbox_style :: SelfPtr -> StylePtr -> IO (SelfPtr)

foreign import ccall safe "checkbox_into_element"
  checkbox_into_element :: SelfPtr -> IO (ElementPtr)

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

foreign import ccall safe "checkbox_primary"
  checkbox_primary :: StylePtr

foreign import ccall safe "checkbox_secondary"
  checkbox_secondary :: StylePtr

foreign import ccall safe "checkbox_success"
  checkbox_success :: StylePtr

foreign import ccall safe "checkbox_danger"
  checkbox_danger :: StylePtr

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle callback c_bool = do
  newStablePtr $ callback $ toBool c_bool

type OnToggle message = Bool -> message

data Style = Primary | Secondary | Success | Danger

data Checkbox message = Checkbox {
  attributes :: [Attribute message],
  label :: String,
  value :: Bool
}

instance IntoNative (Checkbox message) where
  toNative details = do
    let checked = fromBool details.value
    labelPtr <- newCString details.label
    selfPtr <- new_checkbox labelPtr checked
    updatedSelf <- applyAttributes selfPtr details.attributes
    checkbox_into_element updatedSelf

instance UseAttribute SelfPtr (Attribute message) where
  useAttribute selfPtr attribute = do
    case attribute of
      AddOnToggle message -> useOnToggle message selfPtr
      AddStyle value -> useStyle value selfPtr
      None -> return selfPtr

checkbox :: [Attribute message] -> String -> Bool -> Element
checkbox attributes label value = pack Checkbox { .. }

onToggle :: OnToggle message -> Attribute message
onToggle callback = AddOnToggle callback

onToggleIf :: Bool -> OnToggle message -> Attribute message
onToggleIf True callback = onToggle callback
onToggleIf False _ = None

useOnToggle :: OnToggle message -> AttributeFn
useOnToggle callback selfPtr = do
  onTogglePtr <- makeCallback $ wrapOnToggle callback
  checkbox_on_toggle selfPtr onTogglePtr

styleToNative :: Style -> StylePtr
styleToNative value = case value of
  Primary -> checkbox_primary
  Secondary -> checkbox_secondary
  Success -> checkbox_success
  Danger -> checkbox_danger

style :: Style -> Attribute message
style value = AddStyle value

useStyle :: Style -> AttributeFn
useStyle value selfPtr = do
  let stylePtr = styleToNative value
  checkbox_style selfPtr stylePtr
