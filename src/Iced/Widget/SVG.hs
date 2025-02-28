{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Image (
  image,
  fromRgba,
) where

import Control.Monad
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteStringInternal
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeImage
type Self = Ptr NativeImage
data NativeHandle
type Handle = Ptr NativeHandle

data Attribute
  = Width Length
  | Height Length

foreign import ccall "image_handle_from_path"
  handle_from_path :: CString -> IO Handle

handleFromPath :: String -> IO Handle
handleFromPath = handle_from_path <=< newCString

-- len bytes
foreign import ccall "image_handle_from_bytes"
  handle_from_bytes :: CUInt -> Ptr Word8 -> IO Handle

-- width height len pixels
foreign import ccall "image_handle_from_rgba"
  handle_from_rgba :: CUInt -> CUInt -> CUInt -> Ptr Word8 -> IO Handle

handleFromRgba :: Int -> Int -> [Word8] -> IO Handle
handleFromRgba w h pixels = do
  let len = fromIntegral $ length pixels
  pixelsPtr <- newArray pixels
  handle <- handle_from_rgba (fromIntegral w) (fromIntegral h) len pixelsPtr
  free pixelsPtr
  pure handle

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

class IntoHandle a where
  intoHandle :: a -> IO Handle

instance IntoHandle String where
  intoHandle = handleFromPath

instance IntoHandle ByteString.ByteString where
  intoHandle byteString = withForeignPtr ptr $ handle_from_bytes (fromIntegral len)
   where
    (ptr, len) = ByteStringInternal.toForeignPtr0 byteString

image :: IntoHandle a => [Attribute] -> a -> Element
image attributes input = pack Image{..} attributes
 where
  handle = intoHandle input

fromRgba :: [Attribute] -> Int -> Int -> [Word8] -> Element
fromRgba attributes w h pixels = pack Image{..} attributes
 where
  handle = handleFromRgba w h pixels
