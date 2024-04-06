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

foreign import ccall safe "container_new"
  container_new :: ElementPtr -> IO (SelfPtr)

foreign import ccall safe "container_center_x"
  container_center_x :: SelfPtr -> IO (SelfPtr)

foreign import ccall safe "container_center_y"
  container_center_y :: SelfPtr -> IO (SelfPtr)

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
      CenterX -> useCenterX selfPtr
      CenterY -> useCenterY selfPtr
      Width len -> useWidth len selfPtr
      Height len -> useHeight len selfPtr

instance UseLength Attribute where
  width len = Width len
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

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  container_width selfPtr nativeLen

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  container_height selfPtr nativeLen
