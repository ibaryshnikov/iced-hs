{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextEditor (
  textEditor,
  newContent,
  contentWithText,
  applyAction,
  freeContent,
  Action,
  Content,
  onAction,
) where

import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Element

data NativeTextEditor
type Self = Ptr NativeTextEditor
type AttributeFn = Self -> IO Self
data NativeContent
type Content = Ptr NativeContent
data NativeAction
type Action = Ptr NativeAction

data Attribute message
  = AddOnAction (OnAction message)
  | AddPadding Padding
  | Height Length

foreign import ccall safe "text_editor_content_new"
  text_editor_content_new :: IO Content

newContent :: IO Content
newContent = text_editor_content_new

foreign import ccall safe "text_editor_content_with_text"
  text_editor_content_with_text :: CString -> IO Content

contentWithText :: String -> IO Content
contentWithText = text_editor_content_with_text <=< newCString

foreign import ccall safe "text_editor_content_perform"
  text_editor_content_perform :: Content -> Action -> IO ()

applyAction :: Content -> Action -> IO ()
applyAction = text_editor_content_perform

foreign import ccall safe "text_editor_content_free"
  text_editor_content_free :: Content -> IO ()

freeContent :: Content -> IO ()
freeContent = text_editor_content_free

foreign import ccall safe "text_editor_new"
  text_editor_new :: Content -> IO Self

foreign import ccall safe "text_editor_on_action"
  text_editor_on_action :: Self -> FunPtr (NativeOnAction message) -> IO Self

-- text_editor top right bottom left
foreign import ccall safe "text_editor_padding"
  text_editor_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall safe "text_editor_height"
  text_editor_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "text_editor_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnAction message = Action -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnAction message -> IO (FunPtr (NativeOnAction message))

wrapOnAction :: OnAction message -> NativeOnAction message
wrapOnAction callback = newStablePtr . callback

type OnAction message = Action -> message

data TextEditor message = TextEditor {
  content :: Content
}

instance Builder Self where
  build = into_element

instance IntoNative (TextEditor message) Self where
  toNative details = do
    text_editor_new details.content

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddOnAction callback -> useOnAction callback
    AddPadding value -> usePadding value
    Height len -> useFn text_editor_height len

instance PaddingToAttribute Padding (Attribute message) where
  paddingToAttribute = AddPadding

instance UseHeight Length (Attribute message) where
  height = Height

textEditor :: [Attribute message] -> Content -> Element
textEditor attributes content = pack TextEditor { .. } attributes

onAction :: OnAction message -> Attribute message
onAction = AddOnAction

useOnAction :: OnAction message -> AttributeFn
useOnAction callback self =
  makeCallback (wrapOnAction callback)
    >>= text_editor_on_action self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  text_editor_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
