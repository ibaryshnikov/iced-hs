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
type SelfPtr = Ptr NativeText
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute = Size Float | Width Length | Height Length

foreign import ccall safe "text_new"
  text_new :: CString -> IO (SelfPtr)

foreign import ccall safe "text_size"
  text_size :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "text_width"
  text_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "text_height"
  text_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "text_into_element"
  text_into_element :: SelfPtr -> IO (ElementPtr)

data Text = Text { attributes :: [Attribute], value :: String }

instance IntoNative Text where
  toNative details = do
    valuePtr <- newCString details.value
    selfPtr <- text_new valuePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    text_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      Size value -> useSize value selfPtr
      Width len -> useWidth len selfPtr
      Height len -> useHeight len selfPtr

instance UseSize Attribute where
  size value = Size value

instance UseWidth Length Attribute where
  width len = Width len

instance UseHeight Length Attribute where
  height len = Height len

text :: [Attribute] -> String -> Element
text attributes value = pack Text { .. }

useSize :: Float -> AttributeFn
useSize value selfPtr = do
  text_size selfPtr (CFloat value)

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  text_width selfPtr nativeLen

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  text_height selfPtr nativeLen
