{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Text (
  text,
  StyleAttribute,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Color
import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Size
import Iced.Attribute.Style
import Iced.Color
import Iced.ColorFFI
import Iced.Element
import Iced.Theme

data NativeText
type Self = Ptr NativeText
type AttributeFn = Self -> IO Self
data NativeStyle
type Style = Ptr NativeStyle

data StyleAttribute
  = TextColor Color

data Attribute
  = AddColor Color
  | Size Float
  | CustomStyle StyleCallback
  | Width Length
  | Height Length

foreign import ccall "text_new"
  text_new :: CString -> IO Self

foreign import ccall "text_color"
  text_color :: Self -> ColorPtr -> IO Self

foreign import ccall "text_size"
  text_size :: Self -> CFloat -> IO Self

foreign import ccall "text_style_custom"
  text_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "text_width"
  text_width :: Self -> LengthPtr -> IO Self

foreign import ccall "text_height"
  text_height :: Self -> LengthPtr -> IO Self

foreign import ccall "text_into_element"
  into_element :: Self -> IO ElementPtr

-- style color
foreign import ccall "text_style_set_color"
  set_color :: Style -> ColorPtr -> IO ()

data Text = Text { value :: String }

instance Builder Self where
  build = into_element

instance IntoNative Text Self where
  toNative details = do
    value <- newCString details.value
    text_new value

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    AddColor value -> useFnIO text_color value
    CustomStyle value -> useCustomStyle value
    Size value -> useFn text_size value
    Width  len -> useFnIO text_width  len
    Height len -> useFnIO text_height len

instance UseColor Attribute where
  color = AddColor

instance UseSize Attribute where
  size = Size

instance IntoStyle value => UseStyle value Attribute where
  style = intoStyle

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

text :: [Attribute] -> String -> Element
text attributes value = pack Text { .. } attributes

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

instance IntoStyle [StyleAttribute] where
  intoStyle attributes = CustomStyle (\_theme -> attributes)

instance IntoStyle StyleCallback where
  intoStyle callback = CustomStyle callback

instance UseStyleAttribute Style StyleAttribute where
  useStyleAttribute attribute appearance = case attribute of
    TextColor value -> do
      colorPtr <- valueToNativeIO value
      set_color appearance colorPtr

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self = do
  callbackPtr <- makeStyleCallback $ wrapStyleCallback callback
  text_style_custom self callbackPtr

instance UseColor StyleAttribute where
  color = TextColor
