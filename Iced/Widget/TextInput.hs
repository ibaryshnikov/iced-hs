{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.TextInput (textInput, onInput, onSubmit) where

import Foreign
import Foreign.C.String

import Iced.Element

data NativeTextInput
type TextInputPtr = Ptr NativeTextInput

foreign import ccall safe "new_text_input"
  new_text_input :: CString -> CString -> IO (TextInputPtr)

foreign import ccall safe "text_input_on_input"
  text_input_on_input :: TextInputPtr -> FunPtr (NativeOnInput a) -> IO (TextInputPtr)

foreign import ccall safe "text_input_on_submit"
  text_input_on_submit :: TextInputPtr -> StablePtr a -> IO (TextInputPtr)

foreign import ccall safe "text_input_into_element"
  text_input_into_element :: TextInputPtr -> IO (ElementPtr)

type NativeOnInput message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnInput message -> IO (FunPtr (NativeOnInput message))

wrapOnInput :: OnInput message -> NativeOnInput message
wrapOnInput callback c_string = do
  string <- peekCString c_string
  newStablePtr $ callback string

type OnInput message = String -> message

data TextInput = TextInput {
  placeholder :: String,
  value :: String,
  attributes :: [TextInputPtr -> IO TextInputPtr]
}

instance IntoNative TextInput where
  toNative details = do
    placeholderPtr <- newCString details.placeholder
    valuePtr <- newCString details.value
    textInputPtr <- new_text_input placeholderPtr valuePtr
    updatedTextInput <- applyAttributes textInputPtr details.attributes
    text_input_into_element updatedTextInput

textInput :: [TextInputPtr -> IO TextInputPtr] -> String -> String -> Element
textInput attributes placeholder value = pack TextInput { placeholder, value, attributes }

onInput :: OnInput message -> TextInputPtr -> IO (TextInputPtr)
onInput callback textInputPtr = do
  onInputPtr <- makeCallback $ wrapOnInput callback
  text_input_on_input textInputPtr onInputPtr

onSubmit :: message -> TextInputPtr -> IO (TextInputPtr)
onSubmit message textInputPtr = do
    onSubmitPtr <- newStablePtr message
    text_input_on_submit textInputPtr onSubmitPtr
