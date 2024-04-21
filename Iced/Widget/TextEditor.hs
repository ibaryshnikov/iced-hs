{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextEditor (
  textEditor,
  newContent,
  contentWithText,
  applyAction,
  Action,
  Content,
  onAction,
) where

-- required to be able to run IO in update function
-- gonna be removed after the addition of Command api
import System.IO.Unsafe -- hopefully temporally
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Iced.Attribute.Length
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
  text_editor_content_new :: Content

foreign import ccall safe "text_editor_content_with_text"
  text_editor_content_with_text :: CString -> IO (Content)

foreign import ccall safe "text_editor_content_perform"
  text_editor_content_perform :: Content -> Action -> IO ()

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "text_editor_content_free"
--  text_editor_content_free :: Content -> ()

applyAction :: Content -> Action -> Content
applyAction content action = unsafePerformIO $ do -- todo: make it a Command
  text_editor_content_perform content action
  return content

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
  text_editor_into_element :: Self -> IO ElementPtr

type NativeOnAction message = Action -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnAction message -> IO (FunPtr (NativeOnAction message))

wrapOnAction :: OnAction message -> NativeOnAction message
wrapOnAction callback action = do
  newStablePtr $ callback action

type OnAction message = Action -> message

data TextEditor message = TextEditor {
  attributes :: [Attribute message],
  content :: Content
}

instance IntoNative (TextEditor message) where
  toNative details = do
    self <- text_editor_new details.content
    updatedSelf <- applyAttributes self details.attributes
    text_editor_into_element updatedSelf

instance UseAttribute Self (Attribute message) where
  useAttribute self attribute = do
    case attribute of
      AddOnAction callback -> useOnAction callback self
      AddPadding value -> usePadding value self
      Height len -> useHeight len self

instance UsePadding (Attribute message) where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute (Attribute message) where
  paddingToAttribute value = AddPadding value

instance UseHeight Length (Attribute message) where
  height len = Height len

textEditor :: [Attribute message] -> Content -> Element
textEditor attributes content = pack TextEditor { .. }

onAction :: OnAction message -> Attribute message
onAction callback = AddOnAction callback

useOnAction :: OnAction message -> AttributeFn
useOnAction callback self = do
  onActionPtr <- makeCallback $ wrapOnAction callback
  text_editor_on_action self onActionPtr

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self = do
  text_editor_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  text_editor_height self nativeLen

newContent :: Content
newContent = text_editor_content_new

contentWithText :: String -> IO (Content)
contentWithText value = do
  stringPtr <- newCString value
  text_editor_content_with_text stringPtr
