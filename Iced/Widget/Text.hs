{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Text (text) where

import Foreign
import Foreign.C.String

import Iced.Element

data NativeText
type TextPtr = Ptr NativeText

foreign import ccall safe "new_text"
  new_text :: CString -> IO (TextPtr)

foreign import ccall safe "text_into_element"
  text_into_element :: TextPtr -> IO (ElementPtr)

data Text = Text { value :: String, attributes :: [TextPtr -> IO TextPtr] }

instance IntoNative Text where
  toNative details = do
    valuePtr <- newCString details.value
    textPtr <- new_text valuePtr
    updatedText <- applyAttributes textPtr details.attributes
    text_into_element updatedText

text :: Show label => [TextPtr -> IO TextPtr] -> label -> Element
text attributes label = pack Text { value = show label, attributes = attributes }
