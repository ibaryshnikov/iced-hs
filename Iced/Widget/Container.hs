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
type SelfPtr = Ptr NativeContainer
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute = AddPadding Padding | CenterX | CenterY | Width Length | Height Length

foreign import ccall safe "container_new"
  container_new :: ElementPtr -> IO (SelfPtr)

foreign import ccall safe "container_center_x"
  container_center_x :: SelfPtr -> IO (SelfPtr)

foreign import ccall safe "container_center_y"
  container_center_y :: SelfPtr -> IO (SelfPtr)

-- container top right bottom left
foreign import ccall safe "container_padding"
  container_padding :: SelfPtr -> CFloat -> CFloat -> CFloat -> CFloat -> IO (SelfPtr)

foreign import ccall safe "container_width"
  container_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "container_height"
  container_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "container_into_element"
  container_into_element :: SelfPtr -> IO (ElementPtr)

data Container = Container { attributes :: [Attribute], content :: Element }

instance IntoNative Container where
  toNative details = do
    contentPtr <- elementToNative details.content
    selfPtr <- container_new contentPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    container_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      AddPadding value -> usePadding value selfPtr
      CenterX -> useCenterX selfPtr
      CenterY -> useCenterY selfPtr
      Width len -> useWidth len selfPtr
      Height len -> useHeight len selfPtr

instance UsePadding Attribute where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute Attribute where
  paddingToAttribute value = AddPadding value

instance UseWidth Attribute where
  width len = Width len

instance UseHeight Attribute where
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
usePadding Padding { .. } selfPtr = do
  container_padding selfPtr (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  container_width selfPtr nativeLen

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  container_height selfPtr nativeLen
