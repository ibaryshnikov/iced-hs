{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Scrollable (
  scrollable,
) where

import Foreign

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeScrollable
type Self = Ptr NativeScrollable

data Attribute = Width Length | Height Length

foreign import ccall "scrollable_new"
  scrollable_new :: ElementPtr -> IO Self

foreign import ccall "scrollable_width"
  scrollable_width :: Self -> LengthPtr -> IO Self

foreign import ccall "scrollable_height"
  scrollable_height :: Self -> LengthPtr -> IO Self

foreign import ccall "scrollable_into_element"
  into_element :: Self -> IO ElementPtr

data Scrollable = Scrollable { content :: Element }

instance Builder Self where
  build = into_element

instance IntoNative Scrollable Self where
  toNative details = do
    content <- elementToNative details.content
    scrollable_new content

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Width  len -> useFn scrollable_width  len
    Height len -> useFn scrollable_height len

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

scrollable :: [Attribute] -> Element -> Element
scrollable attributes content = pack Scrollable { .. } attributes
