{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.ProgressBar (
  progressBar,
  StyleAttribute,
  BasicStyle(..),
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Style
import Iced.ColorFFI
import Iced.Element
import Iced.Theme

data NativeProgressBar
type Self = Ptr NativeProgressBar
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
  | Bar Background
  | BorderStyle Border

data BasicStyle = Primary | Secondary | Success | Danger deriving Enum

data Attribute
  = BasicStyle BasicStyle
  | CustomStyle StyleCallback
  | Width Length
  | Height Length

-- range_from range_to value
foreign import ccall "progress_bar_new"
  progress_bar_new :: CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall "progress_bar_style_basic"
  progress_bar_style_basic :: Self -> CUChar -> IO Self

foreign import ccall "progress_bar_style_custom"
  progress_bar_style_custom :: Self -> FunPtr NativeStyleCallback -> IO Self

foreign import ccall "progress_bar_width"
  progress_bar_width :: Self -> LengthPtr -> IO Self

foreign import ccall "progress_bar_height"
  progress_bar_height :: Self -> LengthPtr -> IO Self

foreign import ccall "progress_bar_into_element"
  into_element :: Self -> IO ElementPtr

foreign import ccall "progress_bar_style_set_background"
  set_background :: Style -> ColorPtr -> IO ()

foreign import ccall "progress_bar_style_set_bar"
  set_bar :: Style -> ColorPtr -> IO ()

-- style color width radius
foreign import ccall "progress_bar_style_set_border"
  set_border :: Style -> ColorPtr -> CFloat -> CFloat -> IO ()

data ProgressBar = ProgressBar {
  rangeFrom :: Float,
  rangeTo :: Float,
  value :: Float
}

instance Builder Self where
  build = into_element

instance IntoNative ProgressBar Self where
  toNative details = progress_bar_new rangeFrom rangeTo value
    where
      rangeFrom = CFloat details.rangeFrom
      rangeTo = CFloat details.rangeTo
      value = CFloat details.value

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    BasicStyle value -> useBasicStyle value
    CustomStyle value -> useCustomStyle value
    Width  len -> useFnIO progress_bar_width  len
    Height len -> useFnIO progress_bar_height len

instance IntoStyle value => UseStyle value Attribute where
  style = intoStyle

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

progressBar :: [Attribute] -> Float -> Float -> Float -> Element
progressBar attributes rangeFrom rangeTo value = pack ProgressBar { .. } attributes

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
  intoStyle = CustomStyle

instance UseStyleAttribute Style StyleAttribute where
  useStyleAttribute attribute = case attribute of
    Background (BgColor color) -> useFnIO set_background color
    Bar (BgColor color) -> useFnIO set_bar color
    BorderStyle value -> useBorder value

useBorder :: Border -> Style -> IO ()
useBorder Border { color, width = w, radius } appearance = do
  colorPtr <- valueToNativeIO color
  set_border appearance colorPtr (CFloat w) (CFloat radius)

useBasicStyle :: BasicStyle -> AttributeFn
useBasicStyle value self = progress_bar_style_basic self $ fromIntegral $ fromEnum value

useCustomStyle :: StyleCallback -> AttributeFn
useCustomStyle callback self = progress_bar_style_custom self
  =<< makeStyleCallback (wrapStyleCallback callback)

instance UseBackground StyleAttribute where
  background = Background . BgColor

instance UseBar StyleAttribute where
  bar = Bar . BgColor

instance UseBorder StyleAttribute where
  border color w radius = BorderStyle $ Border color w radius
