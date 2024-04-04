{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Container (
  container,
  centerX,
  centerY,
  height,
  width,
) where

import Foreign

import Iced.Element
import Iced.Length
import Iced.LengthFFI

data NativeText
type SelfPtr = Ptr NativeText
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute = CenterX | CenterY | Height Length | Width Length

foreign import ccall safe "new_container"
  new_container :: ElementPtr -> IO (SelfPtr)

foreign import ccall safe "container_center_x"
  container_center_x :: SelfPtr -> IO (SelfPtr)

foreign import ccall safe "container_center_y"
  container_center_y :: SelfPtr -> IO (SelfPtr)

foreign import ccall safe "container_into_element"
  container_into_element :: SelfPtr -> IO (ElementPtr)

foreign import ccall safe "container_height"
  container_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "container_width"
  container_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

data Container = Container { attributes :: [Attribute], content :: Element }

instance IntoNative Container where
  toNative details = do
    contentPtr <- elementToNative details.content
    selfPtr <- new_container contentPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    container_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      CenterX -> useCenterX selfPtr
      CenterY -> useCenterY selfPtr
      Height len -> useHeight len selfPtr
      Width len -> useWidth len selfPtr

instance UseLength Attribute where
  height len = Height len
  width len = Width len

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

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  container_height selfPtr nativeLen

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  container_width selfPtr nativeLen
