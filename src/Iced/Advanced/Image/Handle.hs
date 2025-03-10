module Iced.Advanced.Image.Handle (
  Handle,
  IntoHandle,
  intoHandle,
  handleFromPath,
  handleFromBytes,
  handleFromRgba,
) where

import Control.Monad
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteStringInternal
import Foreign
import Foreign.C.String
import Foreign.C.Types

data NativeHandle
type Handle = Ptr NativeHandle

foreign import ccall "image_handle_from_path"
  handle_from_path :: CString -> IO Handle

handleFromPath :: String -> IO Handle
handleFromPath = handle_from_path <=< newCString

-- len bytes
foreign import ccall "image_handle_from_bytes"
  handle_from_bytes :: CUInt -> Ptr Word8 -> IO Handle

handleFromBytes :: Int -> Ptr Word8 -> IO Handle
handleFromBytes len = handle_from_bytes (fromIntegral len)

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

class IntoHandle a where
  intoHandle :: a -> IO Handle

instance IntoHandle String where
  intoHandle = handleFromPath

instance IntoHandle ByteString.ByteString where
  intoHandle byteString = withForeignPtr ptr $ handleFromBytes len
   where
    (ptr, len) = ByteStringInternal.toForeignPtr0 byteString
