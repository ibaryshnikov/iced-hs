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

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Element

data NativeContainer
type Self = Ptr NativeContainer
type AttributeFn = Self -> IO Self

data Attribute = AddPadding Padding | CenterX | CenterY | Width Length | Height Length

foreign import ccall safe "container_new"
  container_new :: ElementPtr -> IO Self

foreign import ccall safe "container_center_x"
  container_center_x :: Self -> IO Self

foreign import ccall safe "container_center_y"
  container_center_y :: Self -> IO Self

-- container top right bottom left
foreign import ccall safe "container_padding"
  container_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall safe "container_width"
  container_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "container_height"
  container_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "container_into_element"
  into_element :: Self -> IO ElementPtr

data Container = Container { attributes :: [Attribute], content :: Element }

instance IntoNative Container where
  toNative details = do
    content <- elementToNative details.content
    container_new content
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    AddPadding value -> usePadding value
    CenterX -> useCenterX
    CenterY -> useCenterY
    Width len -> useWidth len
    Height len -> useHeight len

instance UsePadding Attribute where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute Attribute where
  paddingToAttribute = AddPadding

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

container :: [Attribute] -> Element -> Element
container attributes content = pack Container { .. }

centerX :: Attribute
centerX = CenterX

useCenterX :: AttributeFn
useCenterX = container_center_x

centerY :: Attribute
centerY = CenterY

useCenterY :: AttributeFn
useCenterY = container_center_y

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  container_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useWidth :: Length -> AttributeFn
useWidth len self = container_width self $ lengthToNative len

useHeight :: Length -> AttributeFn
useHeight len self = container_height self $ lengthToNative len
