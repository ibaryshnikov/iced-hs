{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Checkbox (
  checkbox,
  onToggle,
  onToggleIf,
  textLineHeight,
  textShaping,
  textSize,
  StyleAttribute,
  Status(..),
  StatusAttribute,
  BasicStyle(..),
) where

import Data.List
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Icon
import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.LineHeightFFI
import Iced.Attribute.Size
import Iced.Attribute.Spacing
import Iced.Attribute.Status
import Iced.Attribute.Style
import Iced.Attribute.TextFFI
import Iced.ColorFFI
import Iced.Element
import Iced.Theme

data NativeCheckbox
type Self = Ptr NativeCheckbox
type AttributeFn = Self -> IO Self
data NativeStyle
type Style = Ptr NativeStyle
data NativeIcon
type IconPtr = Ptr NativeIcon

data Background = BgColor Color -- | BgGradient Gradient

data Border = Border {
  color :: Color,
  width :: Float,
  radius :: Float
}

data StyleAttribute
 = Background Background
 | IconColor Color
 | BorderStyle Border
 | TextColor Color

data Status = Active | Hovered | Disabled deriving (Enum, Eq)

type StatusAttribute = (Status, [StyleAttribute])

data BasicStyle = Primary | Secondary | Success | Danger deriving Enum

data Attribute message
  = Icon Word32
  | AddOnToggle (OnToggle message)
  | Size Float
  | Spacing Float
  | BasicStyle BasicStyle
  | CustomStyle StyleCallback
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

foreign import ccall "checkbox_style_basic"
  checkbox_style_basic :: Self -> CUChar -> IO Self

foreign import ccall "checkbox_style_custom"
  checkbox_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

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

-- style color
foreign import ccall "checkbox_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

-- style color
foreign import ccall "checkbox_style_set_icon_color"
  set_icon_color :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "checkbox_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

-- style color
foreign import ccall "checkbox_style_set_text_color"
  set_text_color :: Style -> ColorPtr -> IO ()

type NativeOnToggle message = CBool -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnToggle message -> IO (FunPtr (NativeOnToggle message))

-- use decimal code points
foreign import ccall "checkbox_icon_new"
  checkbox_icon_new :: CUInt -> IO IconPtr

wrapOnToggle :: OnToggle message -> NativeOnToggle message
wrapOnToggle callback = newStablePtr . callback . toBool

type OnToggle message = Bool -> message

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
    BasicStyle value -> useBasicStyle value
    CustomStyle value -> useCustomStyle value
    TextLineHeight value -> useFnIO checkbox_text_line_height value
    TextShaping    value -> useFn checkbox_text_shaping     value
    TextSize       value -> useFn checkbox_text_size        value
    Width len -> useFnIO checkbox_width len
    None -> pure

instance UseIcon (Attribute message) where
  icon = Icon

instance UseSize (Attribute message) where
  size = Size

instance UseSpacing (Attribute message) where
  spacing = Spacing

instance IntoStyle value => UseStyle value (Attribute message) where
  style = intoStyle

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
useOnToggle callback self = checkbox_on_toggle self =<< makeCallback (wrapOnToggle callback)

useIcon :: Word32 -> AttributeFn
useIcon codePoint self = checkbox_icon self =<< checkbox_icon_new (CUInt codePoint)

textLineHeight :: LineHeight -> Attribute message
textLineHeight = TextLineHeight

textShaping :: Shaping -> Attribute message
textShaping = TextShaping

textSize :: Float -> Attribute message
textSize = TextSize

-- style theme status is_checked
type NativeStyleCallback = Style -> CUChar -> CUChar -> CBool -> IO ()

foreign import ccall "wrapper"
  makeStyleCallback :: NativeStyleCallback -> IO (FunPtr NativeStyleCallback)

wrapStyleCallback :: StyleCallback -> NativeStyleCallback
wrapStyleCallback callback appearance themeRaw statusRaw isCheckedRaw = do
  let theme = toEnum $ fromIntegral themeRaw
  let status = toEnum $ fromIntegral statusRaw
  let isChecked = toBool isCheckedRaw
  let attributes = callback theme status isChecked
  applyStyles attributes appearance

type StyleCallback = Theme -> Status -> Bool -> [StyleAttribute]

class IntoStyle value where
  intoStyle :: value -> Attribute message

instance IntoStyle BasicStyle where
  intoStyle = BasicStyle

instance IntoStyle [StyleAttribute] where
  intoStyle attributes = CustomStyle (\_theme _status _isChecked -> attributes)

selectStyles :: [StatusAttribute] -> Status -> [StyleAttribute]
selectStyles extra status = case find (\(s, _a) -> s == status) extra of
  Just (_s, attributes) -> attributes
  Nothing -> []

instance IntoStyle [StatusAttribute] where
  intoStyle extra = CustomStyle (\_theme status _isChecked -> selectStyles extra status)

instance IntoStyle ([StyleAttribute], [StatusAttribute]) where
  intoStyle (base, extra) = CustomStyle (\_theme status _isChecked -> base ++ selectStyles extra status)

instance IntoStyle (Status -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme status _isChecked -> callback status)

instance IntoStyle (Bool -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme _status -> callback)

instance IntoStyle (Status -> Bool -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme -> callback)

instance IntoStyle StyleCallback where
  intoStyle = CustomStyle

instance UseStyleAttribute Style StyleAttribute where
  useStyleAttribute attribute = case attribute of
    Background (BgColor color) -> useFnIO set_background color
    BorderStyle value -> useBorder value
    IconColor color -> useFnIO set_icon_color color
    TextColor color -> useFnIO set_text_color color

useBorder :: Border -> Style -> IO ()
useBorder Border { color, width = w, radius } appearance = do
  colorPtr <- valueToNativeIO color
  set_border appearance colorPtr (CFloat w) (CFloat radius)

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self = checkbox_style_custom self
  =<< makeStyleCallback (wrapStyleCallback callback)

useBasicStyle :: BasicStyle -> AttributeFn
useBasicStyle value self = checkbox_style_basic self $ fromIntegral $ fromEnum value

instance UseBackground StyleAttribute where
  background = Background . BgColor

instance UseBorder StyleAttribute where
  border color w radius = BorderStyle $ Border color w radius

instance UseTextColor StyleAttribute where
  textColor = TextColor

instance UseActive [StyleAttribute] StatusAttribute where
  active = (Active,)

instance UseHovered [StyleAttribute] StatusAttribute where
  hovered = (Hovered,)

instance UseDisabled [StyleAttribute] StatusAttribute where
  disabled = (Disabled,)
