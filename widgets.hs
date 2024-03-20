module Widgets where

import Foreign
import Foreign.C.Types
import Foreign.C.String

type Element = Ptr NativeElement
data NativeElement

foreign import ccall safe "create_text"
  create_text :: CString -> IO (Element)
foreign import ccall safe "create_button"
  create_button :: CString -> StablePtr a -> IO (Element)
foreign import ccall safe "create_column"
  create_column:: CInt -> Ptr Element -> IO (Element)

button:: String -> a -> IO(Element)
button label message = do
  label_ptr <- newCString label
  message_ptr <- newStablePtr message
  create_button label_ptr message_ptr

text:: String -> IO(Element)
text label = do
  label_ptr <- newCString label
  create_text label_ptr

column:: [Element] -> IO(Element)
column elements = let len = length elements in do
  elements_ptr <- newArray elements
  create_column (fromIntegral len) elements_ptr
