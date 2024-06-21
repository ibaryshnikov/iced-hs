{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextInput (
  textInput,
  onInput,
  onSubmit,
  StyleAttribute,
  Status(..),
  StatusAttribute,
) where

import Control.Monad
import Data.List
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.OnInput
import Iced.Attribute.Padding
import Iced.Attribute.PaddingFFI
import Iced.Attribute.Shared
import Iced.Attribute.Status
import Iced.Attribute.Style
import Iced.Color
import Iced.ColorFFI
import Iced.Element
import Iced.Theme

data NativeTextInput
type Self = Ptr NativeTextInput
type AttributeFn = Self -> IO Self
data NativeStyle
type Style = Ptr NativeStyle

data Background = BgColor Color -- | BgGradient Gradient

data Border = Border {
  color :: Color,
  width :: Float,
  radius :: Float
}

data StyleAttribute
  = Background Background
  | AddBorder Border
  | Icon Color
  | Placeholder Color
  | Value Color
  | Selection Color

data Status = Active | Hovered | Focused | Disabled deriving (Enum, Eq)

type StatusAttribute = (Status, [StyleAttribute])

data Attribute message
  = AddOnInput (OnInput message)
  | AddOnSubmit message
  | AddPadding Padding
  | CustomStyle StyleCallback
  | Width Length

foreign import ccall "text_input_new"
  text_input_new :: CString -> CString -> IO Self

foreign import ccall "text_input_on_input"
  text_input_on_input :: Self -> FunPtr (NativeOnInput a) -> IO Self

foreign import ccall "text_input_on_submit"
  text_input_on_submit :: Self -> StablePtr a -> IO Self

foreign import ccall "text_input_padding"
  text_input_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "text_input_style_custom"
  text_input_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "text_input_width"
  text_input_width :: Self -> LengthPtr -> IO Self

foreign import ccall "text_input_into_element"
  into_element :: Self -> IO ElementPtr

foreign import ccall "text_input_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "text_input_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

foreign import ccall "text_input_style_set_icon"
  set_icon :: Style -> ColorPtr -> IO ()

foreign import ccall "text_input_style_set_placeholder"
  set_placeholder :: Style -> ColorPtr -> IO ()

foreign import ccall "text_input_style_set_value"
  set_value :: Style -> ColorPtr -> IO ()

foreign import ccall "text_input_style_set_selection"
  set_selection :: Style -> ColorPtr -> IO ()

type NativeOnInput message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnInput message -> IO (FunPtr (NativeOnInput message))

wrapOnInput :: OnInput message -> NativeOnInput message
wrapOnInput callback = newStablePtr . callback <=< peekCString

type OnInput message = String -> message

data TextInput = TextInput {
  placeholder_ :: String,
  value_ :: String
}

instance Builder Self where
  build = into_element

instance IntoNative TextInput Self where
  toNative details = do
    placeholder_ <- newCString details.placeholder_
    value_ <- newCString details.value_
    text_input_new placeholder_ value_

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddOnInput callback -> useOnInput callback
    AddOnSubmit message -> useOnSubmit message
    AddPadding value_ -> useFnIO text_input_padding value_
    CustomStyle value_ -> useCustomStyle value_
    Width  len -> useFnIO text_input_width  len

instance UseOnInput (OnInput message) (Attribute message) where
  onInput = AddOnInput

instance UsePadding (Attribute message) where
  padding = AddPadding . paddingFromOne

instance UsePadding2 (Attribute message) where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 (Attribute message) where
  padding4 top right bottom left = AddPadding Padding { .. }

instance IntoStyle value => UseStyle value (Attribute message) where
  style = intoStyle

instance UseWidth Length (Attribute message) where
  width = Width

textInput :: [Attribute message] -> String -> String -> Element
textInput attributes placeholder_ value_ = pack TextInput { .. } attributes

useOnInput :: OnInput message -> AttributeFn
useOnInput callback self =
  makeCallback (wrapOnInput callback)
    >>= text_input_on_input self

onSubmit :: message -> Attribute message
onSubmit = AddOnSubmit

useOnSubmit :: message -> AttributeFn
useOnSubmit message self =
    newStablePtr message
      >>= text_input_on_submit self

-- style theme status
type NativeStyleCallback = Style -> CUChar -> CUChar -> IO ()

foreign import ccall "wrapper"
  makeStyleCallback :: NativeStyleCallback -> IO (FunPtr NativeStyleCallback)

wrapStyleCallback :: StyleCallback -> NativeStyleCallback
wrapStyleCallback callback appearance themeRaw statusRaw = do
  let theme = toEnum $ fromIntegral themeRaw
  let status = toEnum $ fromIntegral statusRaw
  let attributes = callback theme status
  applyStyles attributes appearance

type StyleCallback = Theme -> Status -> [StyleAttribute]

class IntoStyle value where
  intoStyle :: value -> Attribute message

instance IntoStyle [StyleAttribute] where
  intoStyle attributes = CustomStyle (\_theme _status -> attributes)

selectStyles :: [StatusAttribute] -> Status -> [StyleAttribute]
selectStyles extra status = case find (\(s, _a) -> s == status) extra of
  Just (_s, attributes) -> attributes
  Nothing -> []

instance IntoStyle [StatusAttribute] where
  intoStyle extra = CustomStyle (\_theme -> selectStyles extra)

instance IntoStyle ([StyleAttribute], [StatusAttribute]) where
  intoStyle (base, extra) = CustomStyle (\_theme -> (base ++) . selectStyles extra)

instance IntoStyle (Status -> [StyleAttribute]) where
  intoStyle callback = CustomStyle (\_theme -> callback)

instance IntoStyle StyleCallback where
  intoStyle = CustomStyle

applyStyles :: [StyleAttribute] -> Style -> IO ()
applyStyles [] _appearance = pure ()
applyStyles (first:remaining) appearance = do
  case first of
    Background (BgColor color) -> do
      colorPtr <- valueToNativeIO color
      set_background appearance colorPtr
    AddBorder Border { color, width = w, radius } -> do
      colorPtr <- valueToNativeIO color
      set_border appearance colorPtr (CFloat w) (CFloat radius)
    Icon color -> do
      colorPtr <- valueToNativeIO color
      set_icon appearance colorPtr
    Placeholder color -> do
      colorPtr <- valueToNativeIO color
      set_placeholder appearance colorPtr
    Value color -> do
      colorPtr <- valueToNativeIO color
      set_value appearance colorPtr
    Selection color -> do
      colorPtr <- valueToNativeIO color
      set_selection appearance colorPtr
  applyStyles remaining appearance

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self = do
  callbackPtr <- makeStyleCallback $ wrapStyleCallback callback
  text_input_style_custom self callbackPtr

instance UseBackground StyleAttribute where
  background = Background . BgColor

instance UseBorder StyleAttribute where
  border color w radius = AddBorder $ Border color w radius

instance UseIcon Color StyleAttribute where
  icon = Icon

instance UsePlaceholder Color StyleAttribute where
  placeholder = Placeholder

instance UseValue StyleAttribute where
  value = Value

instance UseSelection StyleAttribute where
  selection = Selection

instance UseActive [StyleAttribute] StatusAttribute where
  active = (Active,)

instance UseHovered [StyleAttribute] StatusAttribute where
  hovered = (Hovered,)

instance UseFocused [StyleAttribute] StatusAttribute where
  focused = (Focused,)

instance UseDisabled [StyleAttribute] StatusAttribute where
  disabled = (Disabled,)
