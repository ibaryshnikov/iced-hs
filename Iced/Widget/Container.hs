{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Container (
  container,
  centerX,
  centerY,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Element

data NativeContainer
type Self = Ptr NativeContainer
type AttributeFn = Self -> IO Self

data Attribute = AddPadding Padding | CenterX | CenterY | Width Length | Height Length

foreign import ccall "container_new"
  container_new :: ElementPtr -> IO Self

foreign import ccall "container_center_x"
  container_center_x :: Self -> IO Self

foreign import ccall "container_center_y"
  container_center_y :: Self -> IO Self

-- container top right bottom left
foreign import ccall "container_padding"
  container_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall "container_width"
  container_width :: Self -> LengthPtr -> IO Self

foreign import ccall "container_height"
  container_height :: Self -> LengthPtr -> IO Self

foreign import ccall "container_into_element"
  into_element :: Self -> IO ElementPtr

data Container = Container { content :: Element }

instance Builder Self where
  build = into_element

instance IntoNative Container Self where
  toNative details = do
    content <- elementToNative details.content
    container_new content

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    AddPadding value -> usePadding value
    CenterX -> container_center_x
    CenterY -> container_center_y
    Width  len -> useFnIO container_width  len
    Height len -> useFnIO container_height len

instance PaddingToAttribute Padding Attribute where
  paddingToAttribute = AddPadding

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

container :: [Attribute] -> Element -> Element
container attributes content = pack Container { .. } attributes

centerX :: Attribute
centerX = CenterX

centerY :: Attribute
centerY = CenterY

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  container_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
