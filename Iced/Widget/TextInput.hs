module Iced.Widget.TextInput (textInput) where

import Foreign
import Foreign.C.String

import Iced.Element

foreign import ccall safe "new_text_input"
  newTextInput :: CString -> CString -> FunPtr (NativeOnInput a) -> StablePtr a -> IO (Element)

type NativeOnInput message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnInput message -> IO (FunPtr (NativeOnInput message))

wrapOnInput :: OnInput message -> NativeOnInput message
wrapOnInput onInput c_string = do
  string <- peekCString c_string
  newStablePtr $ onInput string

type OnInput message = String -> message

textInput :: String -> String -> OnInput message -> message -> IO (Element)
textInput placeholder value onInput onSubmit = do
  placeholderPtr <- newCString placeholder
  valuePtr <- newCString value
  onInputPtr <- makeCallback $ wrapOnInput onInput
  onSubmitPtr <- newStablePtr onSubmit
  newTextInput placeholderPtr valuePtr onInputPtr onSubmitPtr
