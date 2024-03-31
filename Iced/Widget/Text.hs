{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Text (text) where

import Foreign
import Foreign.C.String

import Iced.Element

data NativeText
type SelfPtr = Ptr NativeText
type Attribute = SelfPtr -> IO SelfPtr

foreign import ccall safe "new_text"
  new_text :: CString -> IO (SelfPtr)

foreign import ccall safe "text_into_element"
  text_into_element :: SelfPtr -> IO (ElementPtr)

data Text = Text { value :: String, attributes :: [Attribute] }

instance IntoNative Text where
  toNative details = do
    valuePtr <- newCString details.value
    selfPtr <- new_text valuePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    text_into_element updatedSelf

text :: Show label => [Attribute] -> label -> Element
text attributes label = pack Text { value = show label, attributes = attributes }
