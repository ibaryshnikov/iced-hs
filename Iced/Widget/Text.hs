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
  useAttribute attribute = case attribute of
    Size value -> useSize value
    Width len -> useWidth len
    Height len -> useHeight len

instance UseSize Attribute where
  size = Size

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

text :: [Attribute] -> String -> Element
text attributes value = pack Text { .. }

useSize :: Float -> AttributeFn
useSize value self = text_size self (CFloat value)

useWidth :: Length -> AttributeFn
useWidth len self = text_width self $ lengthToNative len

useHeight :: Length -> AttributeFn
useHeight len self = text_height self $ lengthToNative len
