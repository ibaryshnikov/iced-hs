module Iced.Advanced.Svg.Handle (
  Handle,
  IntoHandle,
  intoHandle,
  handleFromPath,
  handleFromMemory,
) where

import Control.Monad
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteStringInternal
import Foreign
import Foreign.C.String
import Foreign.C.Types

data NativeHandle
type Handle = Ptr NativeHandle

foreign import ccall "svg_handle_from_path"
  handle_from_path :: CString -> IO Handle

handleFromPath :: String -> IO Handle
handleFromPath = handle_from_path <=< newCString

-- len bytes
foreign import ccall "svg_handle_from_memory"
  handle_from_memory :: CUInt -> Ptr Word8 -> IO Handle

handleFromMemory :: Int -> Ptr Word8 -> IO Handle
handleFromMemory len = handle_from_memory (fromIntegral len)

class IntoHandle a where
  intoHandle :: a -> IO Handle

instance IntoHandle String where
  intoHandle = handleFromPath

instance IntoHandle ByteString.ByteString where
  intoHandle byteString = withForeignPtr ptr $ handleFromMemory len
   where
    (ptr, len) = ByteStringInternal.toForeignPtr0 byteString
