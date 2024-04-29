{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextInput (textInput, onInput, onSubmit) where

import Control.Monad
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

foreign import ccall "text_input_new"
  text_input_new :: CString -> CString -> IO Self

foreign import ccall "text_input_on_input"
  text_input_on_input :: Self -> FunPtr (NativeOnInput a) -> IO Self

foreign import ccall "text_input_on_submit"
  text_input_on_submit :: Self -> StablePtr a -> IO Self

-- text_input top right bottom left
foreign import ccall "text_input_padding"
  text_input_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall "text_input_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnInput message = CString -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnInput message -> IO (FunPtr (NativeOnInput message))

wrapOnInput :: OnInput message -> NativeOnInput message
wrapOnInput callback = newStablePtr . callback <=< peekCString

type OnInput message = String -> message

data TextInput = TextInput {
  placeholder :: String,
  value :: String
}

instance Builder Self where
  build = into_element

instance IntoNative TextInput Self where
  toNative details = do
    placeholder <- newCString details.placeholder
    value <- newCString details.value
    text_input_new placeholder value

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddOnInput callback -> useOnInput callback
    AddOnSubmit message -> useOnSubmit message
    AddPadding value -> usePadding value

instance UseOnInput (OnInput message) (Attribute message) where
  onInput = AddOnInput

instance PaddingToAttribute Padding (Attribute message) where
  paddingToAttribute = AddPadding

textInput :: [Attribute message] -> String -> String -> Element
textInput attributes placeholder value = pack TextInput { .. } attributes

useOnInput :: OnInput message -> AttributeFn
useOnInput callback self =
  makeCallback (wrapOnInput callback)
    >>= text_input_on_input self

onSubmit :: message -> Attribute message
onSubmit = AddOnSubmit

useOnSubmit :: message -> AttributeFn
useOnSubmit message self =
    newStablePtr message
      >>= text_input_on_submit self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  text_input_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
