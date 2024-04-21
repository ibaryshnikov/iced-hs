{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextInput (textInput, onInput, onSubmit) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.OnInput
import Iced.Attribute.Padding
import Iced.Element

data NativeTextInput
type Self = Ptr NativeTextInput
type AttributeFn = Self -> IO Self

data Attribute message
  = AddOnInput (OnInput message)
  | AddOnSubmit message
  | AddPadding Padding

foreign import ccall safe "text_input_new"
  text_input_new :: CString -> CString -> IO Self

foreign import ccall safe "text_input_on_input"
  text_input_on_input :: Self -> FunPtr (NativeOnInput a) -> IO Self

foreign import ccall safe "text_input_on_submit"
  text_input_on_submit :: Self -> StablePtr a -> IO Self

-- text_input top right bottom left
foreign import ccall safe "text_input_padding"
  text_input_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall safe "text_input_into_element"
  text_input_into_element :: Self -> IO ElementPtr

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
    self <- text_input_new placeholderPtr valuePtr
    updatedSelf <- applyAttributes self details.attributes
    text_input_into_element updatedSelf

instance UseAttribute Self (Attribute message) where
  useAttribute self attribute = do
    case attribute of
      AddOnInput callback -> useOnInput callback self
      AddOnSubmit message -> useOnSubmit message self
      AddPadding value -> usePadding value self

instance UseOnInput (OnInput message) (Attribute message) where
  onInput = AddOnInput

instance UsePadding (Attribute message) where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute (Attribute message) where
  paddingToAttribute value = AddPadding value

textInput :: [Attribute message] -> String -> String -> Element
textInput attributes placeholder value = pack TextInput { .. }

useOnInput :: OnInput message -> AttributeFn
useOnInput callback self = do
  onInputPtr <- makeCallback $ wrapOnInput callback
  text_input_on_input self onInputPtr

onSubmit :: message -> Attribute message
onSubmit message = AddOnSubmit message

useOnSubmit :: message -> AttributeFn
useOnSubmit message self = do
    onSubmitPtr <- newStablePtr message
    text_input_on_submit self onSubmitPtr

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self = do
  text_input_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
