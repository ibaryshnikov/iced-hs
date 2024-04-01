{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextInput (textInput, onInput, onSubmit) where

import Foreign
import Foreign.C.String

import Iced.Element

data NativeTextInput
type SelfPtr = Ptr NativeTextInput
type Attribute = SelfPtr -> IO SelfPtr

foreign import ccall safe "new_text_input"
  new_text_input :: CString -> CString -> IO (SelfPtr)

foreign import ccall safe "text_input_on_input"
  text_input_on_input :: SelfPtr -> FunPtr (NativeOnInput a) -> IO (SelfPtr)

foreign import ccall safe "text_input_on_submit"
  text_input_on_submit :: SelfPtr -> StablePtr a -> IO (SelfPtr)

foreign import ccall safe "text_input_into_element"
  text_input_into_element :: SelfPtr -> IO (ElementPtr)

type NativeOnInput message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnInput message -> IO (FunPtr (NativeOnInput message))

wrapOnInput :: OnInput message -> NativeOnInput message
wrapOnInput callback c_string = do
  string <- peekCString c_string
  newStablePtr $ callback string

type OnInput message = String -> message

data TextInput = TextInput {
  attributes :: [Attribute],
  placeholder :: String,
  value :: String
}

instance IntoNative TextInput where
  toNative details = do
    placeholderPtr <- newCString details.placeholder
    valuePtr <- newCString details.value
    selfPtr <- new_text_input placeholderPtr valuePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    text_input_into_element updatedSelf

textInput :: [Attribute] -> String -> String -> Element
textInput attributes placeholder value = pack TextInput { .. }

onInput :: OnInput message -> Attribute
onInput callback selfPtr = do
  onInputPtr <- makeCallback $ wrapOnInput callback
  text_input_on_input selfPtr onInputPtr

onSubmit :: message -> Attribute
onSubmit message selfPtr = do
    onSubmitPtr <- newStablePtr message
    text_input_on_submit selfPtr onSubmitPtr
