{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Center (
  center,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.Padding
import Iced.Element

data NativeContainer
type Self = Ptr NativeContainer
type AttributeFn = Self -> IO Self

data Attribute
  = AddPadding Padding

foreign import ccall "container_new_centered"
  container_new_centered :: ElementPtr -> IO Self

-- container top right bottom left
foreign import ccall "container_padding"
  container_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

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
    AddPadding value -> usePadding value

instance PaddingToAttribute Padding Attribute where
  paddingToAttribute = AddPadding

center :: [Attribute] -> Element -> Element
center attributes content = pack Center { .. } attributes

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  container_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
