module Iced.Advanced.Image (
  ImagePtr,
  imageNew,
  imageOpacity,
  imageRotation,
  imageSnap,
) where

import Foreign
import Foreign.C.Types

import Iced.Advanced.Image.Handle

data NativeImage
type ImagePtr = Ptr NativeImage

foreign import ccall "advanced_image_new"
  image_new :: Handle -> IO ImagePtr

foreign import ccall "advanced_image_opacity"
  image_opacity :: ImagePtr -> CFloat -> IO ImagePtr

foreign import ccall "advanced_image_rotation"
  image_rotation :: ImagePtr -> CFloat -> IO ImagePtr

foreign import ccall "advanced_image_snap"
  image_snap :: ImagePtr -> CBool -> IO ImagePtr

imageNew :: Handle -> IO ImagePtr
imageNew = image_new

imageOpacity :: ImagePtr -> Float -> IO ImagePtr
imageOpacity image = image_opacity image . CFloat

imageRotation :: ImagePtr -> Float -> IO ImagePtr
imageRotation image = image_rotation image . CFloat

imageSnap :: ImagePtr -> Bool -> IO ImagePtr
imageSnap image = image_snap image . fromBool
