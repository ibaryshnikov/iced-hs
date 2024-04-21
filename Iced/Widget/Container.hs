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
  container_into_element :: Self -> IO ElementPtr

data Container = Container { attributes :: [Attribute], content :: Element }

instance IntoNative Container where
  toNative details = do
    contentPtr <- elementToNative details.content
    self <- container_new contentPtr
    updatedSelf <- applyAttributes self details.attributes
    container_into_element updatedSelf

instance UseAttribute Self Attribute where
  useAttribute self attribute = do
    case attribute of
      AddPadding value -> usePadding value self
      CenterX -> useCenterX self
      CenterY -> useCenterY self
      Width len -> useWidth len self
      Height len -> useHeight len self

instance UsePadding Attribute where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute Attribute where
  paddingToAttribute value = AddPadding value

instance UseWidth Length Attribute where
  width len = Width len

instance UseHeight Length Attribute where
  height len = Height len

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
usePadding Padding { .. } self = do
  container_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  container_width self nativeLen

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  container_height self nativeLen
