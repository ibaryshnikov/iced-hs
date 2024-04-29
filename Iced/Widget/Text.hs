{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Text (
  text,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Size
import Iced.Element

data NativeText
type Self = Ptr NativeText

data Attribute = Size Float | Width Length | Height Length

foreign import ccall "text_new"
  text_new :: CString -> IO Self

foreign import ccall "text_size"
  text_size :: Self -> CFloat -> IO Self

foreign import ccall "text_width"
  text_width :: Self -> LengthPtr -> IO Self

foreign import ccall "text_height"
  text_height :: Self -> LengthPtr -> IO Self

foreign import ccall "text_into_element"
  into_element :: Self -> IO ElementPtr

data Text = Text { value :: String }

instance Builder Self where
  build = into_element

instance IntoNative Text Self where
  toNative details = do
    value <- newCString details.value
    text_new value

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Size value -> useFn text_size value
    Width  len -> useFn text_width  len
    Height len -> useFn text_height len

instance UseSize Attribute where
  size = Size

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

text :: [Attribute] -> String -> Element
text attributes value = pack Text { .. } attributes
