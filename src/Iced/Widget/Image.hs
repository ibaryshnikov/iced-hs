{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Image (
  image,
  fromRgba,
) where

import Data.Word
import Foreign

import Iced.Advanced.Image.Handle
import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeImage
type Self = Ptr NativeImage

data Attribute
  = Width Length
  | Height Length

foreign import ccall "image_new"
  image_new :: Handle -> IO Self

foreign import ccall "image_width"
  image_width :: Self -> LengthPtr -> IO Self

foreign import ccall "image_height"
  image_height :: Self -> LengthPtr -> IO Self

foreign import ccall "image_into_element"
  into_element :: Self -> IO ElementPtr

data Image = Image {handle :: IO Handle}

instance Builder Self where
  build = into_element

instance IntoNative Image Self where
  toNative details = do
    image_new =<< details.handle

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Width len -> useFnIO image_width len
    Height len -> useFnIO image_height len

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

image :: IntoHandle a => [Attribute] -> a -> Element
image attributes input = pack Image{..} attributes
 where
  handle = intoHandle input

fromRgba :: [Attribute] -> Int -> Int -> [Word8] -> Element
fromRgba attributes w h pixels = pack Image{..} attributes
 where
  handle = handleFromRgba w h pixels
