{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Scrollable (
  scrollable,
) where

import Foreign

import Iced.Element
import Iced.Length
import Iced.LengthFFI

data NativeScrollable
type SelfPtr = Ptr NativeScrollable
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute = Width Length | Height Length

foreign import ccall safe "scrollable_new"
  scrollable_new :: ElementPtr -> IO (SelfPtr)

foreign import ccall safe "scrollable_width"
  scrollable_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "scrollable_height"
  scrollable_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "scrollable_into_element"
  scrollable_into_element :: SelfPtr -> IO (ElementPtr)

data Scrollable = Scrollable { attributes :: [Attribute], content :: Element }

instance IntoNative Scrollable where
  toNative details = do
    contentPtr <- elementToNative details.content
    selfPtr <- scrollable_new contentPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    scrollable_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      Width len -> useWidth len selfPtr
      Height len -> useHeight len selfPtr

instance UseLength Attribute where
  width len = Width len
  height len = Height len

scrollable :: [Attribute] -> Element -> Element
scrollable attributes content = pack Scrollable { .. }

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  scrollable_width selfPtr nativeLen

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  scrollable_height selfPtr nativeLen
