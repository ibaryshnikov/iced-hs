{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.TextEditor (
  textEditor,
  newContent,
  applyAction,
  Action,
  Content,
  onAction,
) where

-- required to be able to run IO in update function
-- gonna be removed after the addition of Command api
import System.IO.Unsafe -- hopefully temporally
import Foreign

import Iced.Element

data NativeTextEditor
type TextEditorPtr = Ptr NativeTextEditor
data NativeContent
type Content = Ptr NativeContent
data NativeAction
type Action = Ptr NativeAction

foreign import ccall safe "new_content"
  newContent :: Content

foreign import ccall safe "content_perform"
  content_perform :: Content -> Action -> IO ()

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "free_content"
--  free_content :: Content -> ()

applyAction :: Content -> Action -> Content
applyAction content action = unsafePerformIO $ do -- todo: make it a Command
  content_perform content action
  return content

foreign import ccall safe "new_text_editor"
  new_text_editor :: Content -> IO (TextEditorPtr)

foreign import ccall safe "text_editor_on_action"
  text_editor_on_action :: TextEditorPtr -> FunPtr (NativeOnAction message) -> IO (TextEditorPtr)

foreign import ccall safe "text_editor_into_element"
  text_editor_into_element :: TextEditorPtr -> IO (ElementPtr)

type NativeOnAction message = Action -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnAction message -> IO (FunPtr (NativeOnAction message))

wrapOnAction :: OnAction message -> NativeOnAction message
wrapOnAction callback action = do
  newStablePtr $ callback action

type OnAction message = Action -> message

data TextEditor = TextEditor {
  content :: Content,
  attributes :: [TextEditorPtr -> IO TextEditorPtr]
}

instance IntoNative TextEditor where
  toNative details = do
    textEditorPtr <- new_text_editor details.content
    updatedTextEditor <- applyAttributes textEditorPtr details.attributes
    text_editor_into_element updatedTextEditor

textEditor :: [TextEditorPtr -> IO TextEditorPtr] -> Content -> Element
textEditor attributes content = pack TextEditor { content = content, attributes = attributes }

onAction :: OnAction message -> TextEditorPtr -> IO (TextEditorPtr)
onAction callback textEditorPtr = do
  onActionPtr <- makeCallback $ wrapOnAction callback
  text_editor_on_action textEditorPtr onActionPtr
