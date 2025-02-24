{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (
  button,
  onPress,
  onPressIf,
  StyleAttribute,
  Status(..),
  StatusAttribute,
  BasicStyle(..),
) where

import Data.List
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.PaddingFFI
import Iced.Attribute.Status
import Iced.Attribute.Style
import Iced.ColorFFI
import Iced.Element
import Iced.Theme

data NativeButton
type Self = Ptr NativeButton
type AttributeFn = Self -> IO Self
data NativeStyle
type Style = Ptr NativeStyle

data Background = BgColor Color -- | BgGradient Gradient

data Border = Border {
  color :: Color,
  width :: Float,
  radius :: Float
}

--data Shadow = Shadow {
--  color :: Color,
--  offset :: (Float, Float),
--  blurRadius :: Float
--}

data StyleAttribute
  = Background Background
  | TextColor Color
  | BorderStyle Border
  -- | AddShadow Shadow
  -- | ShadowOffset Float Float

data Status = Active | Hovered | Pressed | Disabled deriving (Enum, Eq)

type StatusAttribute = (Status, [StyleAttribute])

data BasicStyle = Primary | Secondary | Success | Danger | Text deriving Enum

data Attribute message
  = AddPadding Padding
  | OnPress message
  | BasicStyle BasicStyle
  | CustomStyle StyleCallback
  | Width Length
  | Height Length
  | None

foreign import ccall "button_new"
  button_new :: CString -> IO Self

foreign import ccall "button_on_press"
  button_on_press :: Self -> StablePtr a -> IO Self

foreign import ccall "button_padding"
  button_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "button_style_basic"
  button_style_basic :: Self -> CUChar -> IO Self

foreign import ccall "button_style_custom"
  button_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "button_width"
  button_width :: Self -> LengthPtr -> IO Self

foreign import ccall "button_height"
  button_height :: Self -> LengthPtr -> IO Self

foreign import ccall "button_into_element"
  into_element :: Self -> IO ElementPtr

foreign import ccall "button_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "button_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

foreign import ccall "button_style_set_text_color"
  set_text_color :: Style -> ColorPtr -> IO ()

data Button = Button {
  label :: String
}

instance Builder Self where
  build = into_element

instance IntoNative Button Self where
  toNative details = do
    label <- newCString details.label
    button_new label

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    OnPress message -> useOnPress message
    AddPadding value -> useFnIO button_padding value
    BasicStyle value -> useBasicStyle value
    CustomStyle value -> useCustomStyle value
    Width  len -> useFnIO button_width  len
    Height len -> useFnIO button_height len
    None -> pure

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

instance UseHeight Length (Attribute message) where
  height = Height

button :: [Attribute message] -> String -> Element
button attributes label = pack Button { .. } attributes

onPress :: message -> Attribute message
onPress = OnPress

onPressIf :: Bool -> message -> Attribute message
onPressIf True message = onPress message
onPressIf False _ = None

useOnPress :: message -> AttributeFn
useOnPress message self = button_on_press self =<< newStablePtr message

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

instance IntoStyle BasicStyle where
  intoStyle = BasicStyle

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

instance UseStyleAttribute Style StyleAttribute where
  useStyleAttribute attribute = case attribute of
    -- ShadowOffset _x _y -> pure ()
    Background (BgColor color) -> useFnIO set_background color
    BorderStyle value -> useBorder value
    TextColor color -> useFnIO set_text_color color
    -- AddShadow _shadow -> pure ()

useBorder :: Border -> Style -> IO ()
useBorder Border { color, width = w, radius } appearance = do
  colorPtr <- valueToNativeIO color
  set_border appearance colorPtr (CFloat w) (CFloat radius)

useBasicStyle :: BasicStyle -> AttributeFn
useBasicStyle value self = button_style_basic self $ fromIntegral $ fromEnum value

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self = button_style_custom self
  =<< makeStyleCallback (wrapStyleCallback callback)

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

instance UsePressed [StyleAttribute] StatusAttribute where
  pressed = (Pressed,)

instance UseDisabled [StyleAttribute] StatusAttribute where
  disabled = (Disabled,)
