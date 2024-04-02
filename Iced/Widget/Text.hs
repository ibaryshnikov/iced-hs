{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Text (
  text,
  height,
  size,
  width,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Element
import Iced.Length
import Iced.LengthFFI

data NativeText
type SelfPtr = Ptr NativeText
type Attribute = SelfPtr -> IO SelfPtr

foreign import ccall safe "new_text"
  new_text :: CString -> IO (SelfPtr)

foreign import ccall safe "text_into_element"
  text_into_element :: SelfPtr -> IO (ElementPtr)

foreign import ccall safe "text_height"
  text_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "text_size"
  text_size :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "text_width"
  text_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

data Text = Text { attributes :: [Attribute], value :: String }

instance IntoNative Text where
  toNative details = do
    valuePtr <- newCString details.value
    selfPtr <- new_text valuePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    text_into_element updatedSelf

text :: [Attribute] -> String -> Element
text attributes value = pack Text { .. }

height :: Length -> Attribute
height len selfPtr = do
  let nativeLen = lengthToNative len
  text_height selfPtr nativeLen

size :: Float -> Attribute
size value selfPtr = do
  text_size selfPtr (CFloat value)

width :: Length -> Attribute
width len selfPtr = do
  let nativeLen = lengthToNative len
  text_width selfPtr nativeLen
