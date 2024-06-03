{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Button (
  button,
  onPress,
  onPressIf,
  background,
  border,
  textColor,
  active,
  hovered,
  pressed,
  disabled,
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
import Iced.Attribute.Padding
import Iced.Attribute.Style
import Iced.Color
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
 = ShadowOffset Float Float
 | AddBackground Background
 | TextColor Color
 | AddBorder Border
 -- | AddShadow Shadow

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

-- button top right bottom left
foreign import ccall "button_padding"
  button_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

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

-- style color
foreign import ccall "button_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "button_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

-- style color
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
    AddPadding value -> usePadding value
    BasicStyle value -> useBasicStyle value
    CustomStyle value -> useCustomStyle value
    Width  len -> useFnIO button_width  len
    Height len -> useFnIO button_height len
    None -> pure

instance PaddingToAttribute Padding (Attribute message) where
  paddingToAttribute = AddPadding

instance IntoStyle a => UseStyle a (Attribute message) where
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
useOnPress message self =
  newStablePtr message
    >>= button_on_press self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  button_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

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

class IntoStyle a where
  intoStyle :: a -> Attribute message

instance IntoStyle BasicStyle where
  intoStyle value = BasicStyle value

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
  intoStyle callback = CustomStyle callback

applyStyles :: [StyleAttribute] -> Style -> IO ()
applyStyles [] _appearance = pure ()
applyStyles (first:remaining) appearance = do
  case first of
    ShadowOffset _x _y -> pure ()
    AddBackground (BgColor color) -> do
      colorPtr <- valueToNativeIO color
      set_background appearance colorPtr
    TextColor color -> do
      colorPtr <- valueToNativeIO color
      set_text_color appearance colorPtr
    AddBorder Border { color, width = w, radius } -> do
      colorPtr <- valueToNativeIO color
      set_border appearance colorPtr (CFloat w) (CFloat radius)
    -- AddShadow _shadow -> pure ()
  applyStyles remaining appearance

border :: Color -> Float -> Float -> StyleAttribute
border color w radius = AddBorder $ Border color w radius

active :: [StyleAttribute] -> StatusAttribute
active = (Active,)

hovered :: [StyleAttribute] -> StatusAttribute
hovered = (Hovered,)

pressed :: [StyleAttribute] -> StatusAttribute
pressed = (Pressed,)

disabled :: [StyleAttribute] -> StatusAttribute
disabled = (Disabled,)

useBasicStyle :: BasicStyle -> AttributeFn
useBasicStyle value self = button_style_basic self $ fromIntegral $ fromEnum value

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self = do
  callbackPtr <- makeStyleCallback $ wrapStyleCallback callback
  button_style_custom self callbackPtr

textColor :: Color -> StyleAttribute
textColor = TextColor

background :: Color -> StyleAttribute
background = AddBackground . BgColor
