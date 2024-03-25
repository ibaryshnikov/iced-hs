module Iced.Widget.TextEditor (
  textEditor,
  newContent,
  applyAction,
  Action,
  Content,
) where

import Foreign

import Iced.Element

data NativeContent
data NativeAction

type Content = Ptr NativeContent
type Action = Ptr NativeAction

foreign import ccall safe "new_content"
  newContent :: Content

foreign import ccall safe "apply_action"
  applyAction :: Content -> Action -> IO ()

foreign import ccall safe "new_text_editor"
  new_text_editor :: Content -> FunPtr (NativeOnAction message) -> IO (Element)

type NativeOnAction message = Action -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnAction message -> IO (FunPtr (NativeOnAction message))

wrapOnAction :: OnAction message -> NativeOnAction message
wrapOnAction onAction action = do
  newStablePtr $ onAction action

type OnAction message = Action -> message

textEditor :: Content -> OnAction message -> IO (Element)
textEditor content onAction = do
  onActionPtr <- makeCallback $ wrapOnAction onAction
  new_text_editor content onActionPtr
