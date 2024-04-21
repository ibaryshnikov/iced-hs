{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Scrollable (
  scrollable,
) where

import Foreign

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeScrollable
type Self = Ptr NativeScrollable
type AttributeFn = Self -> IO Self

data Attribute = Width Length | Height Length

foreign import ccall safe "scrollable_new"
  scrollable_new :: ElementPtr -> IO Self

foreign import ccall safe "scrollable_width"
  scrollable_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "scrollable_height"
  scrollable_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "scrollable_into_element"
  into_element :: Self -> IO ElementPtr

data Scrollable = Scrollable { attributes :: [Attribute], content :: Element }

instance IntoNative Scrollable where
  toNative details = do
    contentPtr <- elementToNative details.content
    self <- scrollable_new contentPtr
    into_element =<< applyAttributes self details.attributes

instance UseAttribute Self Attribute where
  useAttribute self attribute = do
    case attribute of
      Width len -> useWidth len self
      Height len -> useHeight len self

instance UseWidth Length Attribute where
  width len = Width len

instance UseHeight Length Attribute where
  height len = Height len

scrollable :: [Attribute] -> Element -> Element
scrollable attributes content = pack Scrollable { .. }

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  scrollable_width self nativeLen

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  scrollable_height self nativeLen
