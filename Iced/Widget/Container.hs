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
type Attribute = SelfPtr -> IO SelfPtr

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

container :: [Attribute] -> Element -> Element
container attributes content = pack Container { .. }

centerX :: Attribute
centerX = container_center_x

centerY :: Attribute
centerY = container_center_y

height :: Length -> Attribute
height len selfPtr = do
  let nativeLen = lengthToNative len
  container_height selfPtr nativeLen

width :: Length -> Attribute
width len selfPtr = do
  let nativeLen = lengthToNative len
  container_width selfPtr nativeLen
