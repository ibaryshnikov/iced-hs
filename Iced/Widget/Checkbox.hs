{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Checkbox (
  checkbox,
  onToggle,
  onToggleIf,
  icon,
  textLineHeight,
  textShaping,
  textSize,
  Style(..),
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.LineHeightFFI
import Iced.Attribute.Size
import Iced.Attribute.Spacing
import Iced.Attribute.Style
import Iced.Attribute.TextFFI
import Iced.Element

data NativeCheckbox
type Self = Ptr NativeCheckbox
type AttributeFn = Self -> IO Self
data NativeStyle
type StylePtr = Ptr NativeStyle
data NativeIcon
type IconPtr = Ptr NativeIcon

data Attribute message
  = Icon Word32
  | AddOnToggle (OnToggle message)
  | Size Float
  | Spacing Float
  | AddStyle Style
  | TextLineHeight LineHeight
  | TextShaping Shaping
  | TextSize Float
  | Width Length
  | None

-- label is_checked
foreign import ccall "checkbox_new"
  checkbox_new :: CString -> CBool -> IO Self

foreign import ccall "checkbox_icon"
  checkbox_icon :: Self -> IconPtr -> IO Self

foreign import ccall "checkbox_on_toggle"
  checkbox_on_toggle :: Self -> FunPtr (NativeOnToggle a) -> IO Self

foreign import ccall "checkbox_size"
  checkbox_size :: Self -> CFloat -> IO Self

foreign import ccall "checkbox_spacing"
  checkbox_spacing :: Self -> CFloat -> IO Self

foreign import ccall "checkbox_style"
  checkbox_style :: Self -> StylePtr -> IO Self

foreign import ccall "checkbox_text_line_height"
  checkbox_text_line_height :: Self -> LineHeightPtr -> IO Self

foreign import ccall "checkbox_text_shaping"
  checkbox_text_shaping :: Self -> CUChar -> IO Self

foreign import ccall "checkbox_text_size"
  checkbox_text_size :: Self -> CFloat -> IO Self

foreign import ccall "checkbox_width"
  checkbox_width :: Self -> LengthPtr -> IO Self

foreign import ccall "checkbox_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

foreign import ccall "checkbox_primary"
  checkbox_primary :: IO StylePtr

foreign import ccall "checkbox_secondary"
  checkbox_secondary :: IO StylePtr

foreign import ccall "checkbox_success"
  checkbox_success :: IO StylePtr

foreign import ccall "checkbox_danger"
  checkbox_danger :: IO StylePtr

-- use decimal code points
foreign import ccall "checkbox_icon_new"
  checkbox_icon_new :: CUInt -> IO IconPtr

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle callback c_bool = do
  newStablePtr $ callback $ toBool c_bool

type OnToggle message = Bool -> message

data Style = Primary | Secondary | Success | Danger

data Checkbox = Checkbox {
  label :: String,
  value :: Bool
}

instance Builder Self where
  build = into_element

instance IntoNative Checkbox Self where
  toNative details = do
    let checked = fromBool details.value
    label <- newCString details.label
    checkbox_new label checked

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    Icon codePoint -> useIcon codePoint
    AddOnToggle callback -> useOnToggle callback
    Size value -> useFn checkbox_size value
    Spacing value -> useFn checkbox_spacing value
    AddStyle value -> useFnIO checkbox_style value
    TextLineHeight value -> useFnIO checkbox_text_line_height value
    TextShaping    value -> useFn checkbox_text_shaping     value
    TextSize       value -> useFn checkbox_text_size        value
    Width len -> useFnIO checkbox_width len
    None -> pure

instance UseSize (Attribute message) where
  size = Size

instance UseSpacing (Attribute message) where
  spacing = Spacing

instance UseStyle Style (Attribute message) where
  style = AddStyle

instance UseWidth Length (Attribute message) where
  width = Width

checkbox :: [Attribute message] -> String -> Bool -> Element
checkbox attributes label value = pack Checkbox { .. } attributes

onToggle :: OnToggle message -> Attribute message
onToggle = AddOnToggle

onToggleIf :: Bool -> OnToggle message -> Attribute message
onToggleIf True callback = onToggle callback
onToggleIf False _ = None

useOnToggle :: OnToggle message -> AttributeFn
useOnToggle callback self =
  makeCallback (wrapOnToggle callback)
    >>= checkbox_on_toggle self

instance ValueToNativeIO Style StylePtr where
  valueToNativeIO value = case value of
    Primary -> checkbox_primary
    Secondary -> checkbox_secondary
    Success -> checkbox_success
    Danger -> checkbox_danger

icon :: Word32 -> Attribute message
icon = Icon

useIcon :: Word32 -> AttributeFn
useIcon codePoint self = do
  iconPtr <- checkbox_icon_new (CUInt codePoint)
  checkbox_icon self iconPtr

textLineHeight :: LineHeight -> Attribute message
textLineHeight = TextLineHeight

textShaping :: Shaping -> Attribute message
textShaping = TextShaping

textSize :: Float -> Attribute message
textSize = TextSize
