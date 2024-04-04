{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextInput (textInput, onInput, onSubmit) where

import Foreign
import Foreign.C.String

import Iced.Element

data NativeTextInput
type SelfPtr = Ptr NativeTextInput
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute message = AddOnInput (OnInput message) | AddOnSubmit message

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

data TextInput message = TextInput {
  attributes :: [Attribute message],
  placeholder :: String,
  value :: String
}

instance IntoNative (TextInput message) where
  toNative details = do
    placeholderPtr <- newCString details.placeholder
    valuePtr <- newCString details.value
    selfPtr <- new_text_input placeholderPtr valuePtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    text_input_into_element updatedSelf

instance UseAttribute SelfPtr (Attribute message) where
  useAttribute selfPtr attribute = do
    case attribute of
      AddOnInput callback -> useOnInput callback selfPtr
      AddOnSubmit message -> useOnSubmit message selfPtr

textInput :: [Attribute message] -> String -> String -> Element
textInput attributes placeholder value = pack TextInput { .. }

onInput :: OnInput message -> Attribute message
onInput callback = AddOnInput callback

useOnInput :: OnInput message -> AttributeFn
useOnInput callback selfPtr = do
  onInputPtr <- makeCallback $ wrapOnInput callback
  text_input_on_input selfPtr onInputPtr

onSubmit :: message -> Attribute message
onSubmit message = AddOnSubmit message

useOnSubmit :: message -> AttributeFn
useOnSubmit message selfPtr = do
    onSubmitPtr <- newStablePtr message
    text_input_on_submit selfPtr onSubmitPtr
