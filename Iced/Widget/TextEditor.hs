{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

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
type SelfPtr = Ptr NativeTextEditor
type AttributeFn = SelfPtr -> IO SelfPtr
data NativeContent
type Content = Ptr NativeContent
data NativeAction
type Action = Ptr NativeAction

data Attribute message = AddOnAction (OnAction message)

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
  new_text_editor :: Content -> IO (SelfPtr)

foreign import ccall safe "text_editor_on_action"
  text_editor_on_action :: SelfPtr -> FunPtr (NativeOnAction message) -> IO (SelfPtr)

foreign import ccall safe "text_editor_into_element"
  text_editor_into_element :: SelfPtr -> IO (ElementPtr)

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
    selfPtr <- new_text_editor details.content
    updatedSelf <- applyAttributes selfPtr details.attributes
    text_editor_into_element updatedSelf

instance UseAttribute SelfPtr (Attribute message) where
  useAttribute selfPtr attribute = do
    case attribute of
      AddOnAction callback -> useOnAction callback selfPtr

textEditor :: [Attribute message] -> Content -> Element
textEditor attributes content = pack TextEditor { .. }

onAction :: OnAction message -> Attribute message
onAction callback = AddOnAction callback

useOnAction :: OnAction message -> AttributeFn
useOnAction callback selfPtr = do
  onActionPtr <- makeCallback $ wrapOnAction callback
  text_editor_on_action selfPtr onActionPtr
