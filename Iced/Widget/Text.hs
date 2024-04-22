{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Text (
  text,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Size
import Iced.Element

data NativeText
type Self = Ptr NativeText
type AttributeFn = Self -> IO Self

data Attribute = Size Float | Width Length | Height Length

foreign import ccall safe "text_new"
  text_new :: CString -> IO Self

foreign import ccall safe "text_size"
  text_size :: Self -> CFloat -> IO Self

foreign import ccall safe "text_width"
  text_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "text_height"
  text_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "text_into_element"
  into_element :: Self -> IO ElementPtr

data Text = Text { attributes :: [Attribute], value :: String }

instance IntoNative Text where
  toNative details = do
    value <- newCString details.value
    text_new value
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self Attribute where
  useAttribute attribute = do
    case attribute of
      Size value -> useSize value
      Width len -> useWidth len
      Height len -> useHeight len

instance UseSize Attribute where
  size value = Size value

instance UseWidth Length Attribute where
  width len = Width len

instance UseHeight Length Attribute where
  height len = Height len

text :: [Attribute] -> String -> Element
text attributes value = pack Text { .. }

useSize :: Float -> AttributeFn
useSize value self = do
  text_size self (CFloat value)

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  text_width self nativeLen

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  text_height self nativeLen
