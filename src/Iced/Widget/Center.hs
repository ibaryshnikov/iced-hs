{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Center (
  center,
) where

import Foreign

import Iced.Attribute.Internal
import Iced.Attribute.Padding
import Iced.Attribute.PaddingFFI
import Iced.Element

data NativeContainer
type Self = Ptr NativeContainer

data Attribute
  = AddPadding Padding

foreign import ccall "container_new_centered"
  container_new_centered :: ElementPtr -> IO Self

-- container padding
foreign import ccall "container_padding"
  container_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "container_into_element"
  into_element :: Self -> IO ElementPtr

data Center = Center { content :: Element }

instance Builder Self where
  build = into_element

instance IntoNative Center Self where
  toNative details = do
    content <- elementToNative details.content
    container_new_centered content

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    AddPadding value -> useFnIO container_padding value

instance UsePadding Attribute where
  padding = AddPadding . paddingFromOne

instance UsePadding2 Attribute where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 Attribute where
  padding4 top right bottom left = AddPadding Padding { .. }

center :: [Attribute] -> Element -> Element
center attributes content = pack Center { .. } attributes
