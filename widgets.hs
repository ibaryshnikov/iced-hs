module Widgets where

import Foreign
import Foreign.C.String
import Foreign.C.Types

data NativeElement

type Element = Ptr NativeElement

foreign import ccall safe "create_text"
  create_text :: CString -> IO (Element)
foreign import ccall safe "create_text_input"
  create_text_input :: CString -> CString -> FunPtr (NativeOnInput a) -> StablePtr a -> IO (Element)
foreign import ccall safe "create_button"
  create_button :: CString -> StablePtr a -> IO (Element)
foreign import ccall safe "create_column"
  create_column :: CInt -> Ptr Element -> IO (Element)

button :: String -> a -> IO (Element)
button label message = do
  label_ptr <- newCString label
  message_ptr <- newStablePtr message
  create_button label_ptr message_ptr

text :: String -> IO (Element)
text label = do
  label_ptr <- newCString label
  create_text label_ptr

type NativeOnInput message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeOnInputCallback :: NativeOnInput message -> IO (FunPtr (NativeOnInput message))

on_input_hs:: OnInput message -> CString -> IO (StablePtr message)
on_input_hs on_input c_string = do
  string <- peekCString c_string
  newStablePtr $ on_input string

type OnInput message = String -> message

text_input :: String -> String -> OnInput message -> message -> IO (Element)
text_input placeholder value on_input on_submit = do
  placeholder_ptr <- newCString placeholder
  value_ptr <- newCString value
  on_input_ptr <- makeOnInputCallback $ on_input_hs on_input
  on_submit_ptr <- newStablePtr on_submit
  create_text_input placeholder_ptr value_ptr on_input_ptr on_submit_ptr

column :: [Element] -> IO (Element)
column elements = let len = length elements in do
  elements_ptr <- newArray elements
  create_column (fromIntegral len) elements_ptr
