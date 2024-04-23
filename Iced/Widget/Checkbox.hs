{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Checkbox (
  checkbox,
  onToggle,
  onToggleIf,
  icon,
  style,
  Style(..),
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element

data NativeCheckbox
type Self = Ptr NativeCheckbox
type AttributeFn = Self -> IO Self
data NativeStyle
type StylePtr = Ptr NativeStyle
data NativeIcon
type IconPtr = Ptr NativeIcon

data Attribute message
  = AddOnToggle (OnToggle message)
  | AddStyle Style
  | AddIcon Word32
  | None

-- label is_checked
foreign import ccall safe "checkbox_new"
  checkbox_new :: CString -> CBool -> IO Self

foreign import ccall safe "checkbox_icon"
  checkbox_icon :: Self -> IconPtr -> IO Self

foreign import ccall safe "checkbox_on_toggle"
  checkbox_on_toggle :: Self -> FunPtr (NativeOnToggle a) -> IO Self

foreign import ccall safe "checkbox_style"
  checkbox_style :: Self -> StylePtr -> IO Self

foreign import ccall safe "checkbox_into_element"
  into_element :: Self -> IO ElementPtr

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

-- use decimal code points
foreign import ccall safe "checkbox_icon_new"
  checkbox_icon_new :: CUInt -> IconPtr

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
    label <- newCString details.label
    checkbox_new label checked
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddOnToggle callback -> useOnToggle callback
    AddIcon codePoint -> useIcon codePoint
    AddStyle value -> useStyle value
    None -> do pure

checkbox :: [Attribute message] -> String -> Bool -> Element
checkbox attributes label value = pack Checkbox { .. }

onToggle :: OnToggle message -> Attribute message
onToggle = AddOnToggle

onToggleIf :: Bool -> OnToggle message -> Attribute message
onToggleIf True callback = onToggle callback
onToggleIf False _ = None

useOnToggle :: OnToggle message -> AttributeFn
useOnToggle callback self =
  makeCallback (wrapOnToggle callback)
    >>= checkbox_on_toggle self

styleToNative :: Style -> StylePtr
styleToNative value = case value of
  Primary -> checkbox_primary
  Secondary -> checkbox_secondary
  Success -> checkbox_success
  Danger -> checkbox_danger

icon :: Word32 -> Attribute message
icon = AddIcon

useIcon :: Word32 -> AttributeFn
useIcon codePoint self =
  let iconPtr = checkbox_icon_new (CUInt codePoint)
  in checkbox_icon self iconPtr

style :: Style -> Attribute message
style = AddStyle

useStyle :: Style -> AttributeFn
useStyle value self = checkbox_style self $ styleToNative value
