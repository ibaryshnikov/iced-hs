{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Container (
  container,
  centerX,
  centerY,
  StyleAttribute,
  BasicStyle(..),
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.PaddingFFI
import Iced.Attribute.Style
import Iced.ColorFFI
import Iced.Element
import Iced.Theme

data NativeContainer
type Self = Ptr NativeContainer
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
  = TextColor Color
  | Background Background
  | BorderStyle Border
  -- | AddShadow Shadow

data BasicStyle = BorderedBox | RoundedBox | Transparent deriving Enum

data Attribute
  = AddPadding Padding
  | CenterX Length
  | CenterY Length
  | BasicStyle BasicStyle
  | CustomStyle StyleCallback
  | Width Length
  | Height Length

foreign import ccall "container_new"
  container_new :: ElementPtr -> IO Self

-- self width
foreign import ccall "container_center_x"
  container_center_x :: Self -> LengthPtr -> IO Self

-- self height
foreign import ccall "container_center_y"
  container_center_y :: Self -> LengthPtr -> IO Self

-- container padding
foreign import ccall "container_padding"
  container_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "container_style_basic"
  container_style_basic :: Self -> CUChar -> IO Self

foreign import ccall "container_style_custom"
  container_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "container_width"
  container_width :: Self -> LengthPtr -> IO Self

foreign import ccall "container_height"
  container_height :: Self -> LengthPtr -> IO Self

foreign import ccall "container_into_element"
  into_element :: Self -> IO ElementPtr

-- style color
foreign import ccall "container_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "container_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

-- style color
foreign import ccall "container_style_set_text_color"
  set_text_color :: Style -> ColorPtr -> IO ()

data Container = Container { content :: Element }

instance Builder Self where
  build = into_element

instance IntoNative Container Self where
  toNative details = do
    content <- elementToNative details.content
    container_new content

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    AddPadding value -> useFnIO container_padding value
    CenterX len -> useFnIO container_center_x len
    CenterY len -> useFnIO container_center_y len
    BasicStyle value -> useBasicStyle value
    CustomStyle value -> useCustomStyle value
    Width  len -> useFnIO container_width  len
    Height len -> useFnIO container_height len

instance UsePadding Attribute where
  padding = AddPadding . paddingFromOne

instance UsePadding2 Attribute where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 Attribute where
  padding4 top right bottom left = AddPadding Padding { .. }

instance IntoStyle value => UseStyle value Attribute where
  style = intoStyle

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

container :: [Attribute] -> Element -> Element
container attributes content = pack Container { .. } attributes

centerX :: Length -> Attribute
centerX = CenterX

centerY :: Length -> Attribute
centerY = CenterY

-- style theme
type NativeStyleCallback = Style -> CUChar -> IO ()

foreign import ccall "wrapper"
  makeStyleCallback :: NativeStyleCallback -> IO (FunPtr NativeStyleCallback)

wrapStyleCallback :: StyleCallback -> NativeStyleCallback
wrapStyleCallback callback appearance themeRaw = do
  let theme = toEnum $ fromIntegral themeRaw
  let attributes = callback theme
  applyStyles attributes appearance

type StyleCallback = Theme -> [StyleAttribute]

class IntoStyle value where
  intoStyle :: value -> Attribute

instance IntoStyle BasicStyle where
  intoStyle = BasicStyle

instance IntoStyle [StyleAttribute] where
  intoStyle attributes = CustomStyle (\_theme -> attributes)

instance IntoStyle StyleCallback where
  intoStyle callback = CustomStyle callback

instance UseStyleAttribute Style StyleAttribute where
  useStyleAttribute attribute appearance = case attribute of
    Background (BgColor color) -> do
      colorPtr <- valueToNativeIO color
      set_background appearance colorPtr
    TextColor color -> do
      colorPtr <- valueToNativeIO color
      set_text_color appearance colorPtr
    BorderStyle Border { color, width = w, radius } -> do
      colorPtr <- valueToNativeIO color
      set_border appearance colorPtr (CFloat w) (CFloat radius)
    -- AddShadow _shadow -> pure ()

useBasicStyle :: BasicStyle -> AttributeFn
useBasicStyle value self = container_style_basic self $ fromIntegral $ fromEnum value

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self = do
  callbackPtr <- makeStyleCallback $ wrapStyleCallback callback
  container_style_custom self callbackPtr

instance UseBackground StyleAttribute where
  background = Background . BgColor

instance UseBorder StyleAttribute where
  border color w radius = BorderStyle $ Border color w radius

instance UseTextColor StyleAttribute where
  textColor = TextColor
