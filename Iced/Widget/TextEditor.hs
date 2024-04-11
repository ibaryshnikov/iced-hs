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
import Foreign.C.Types

import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Element

data NativeTextEditor
type SelfPtr = Ptr NativeTextEditor
type AttributeFn = SelfPtr -> IO SelfPtr
data NativeContent
type Content = Ptr NativeContent
data NativeAction
type Action = Ptr NativeAction

data Attribute message
  = AddOnAction (OnAction message)
  | AddPadding Padding
  | Height Length

foreign import ccall safe "content_new"
  content_new :: Content

foreign import ccall safe "content_perform"
  content_perform :: Content -> Action -> IO ()

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "free_content"
--  free_content :: Content -> ()

applyAction :: Content -> Action -> Content
applyAction content action = unsafePerformIO $ do -- todo: make it a Command
  content_perform content action
  return content

foreign import ccall safe "text_editor_new"
  text_editor_new :: Content -> IO (SelfPtr)

foreign import ccall safe "text_editor_on_action"
  text_editor_on_action :: SelfPtr -> FunPtr (NativeOnAction message) -> IO (SelfPtr)

-- text_editor top right bottom left
foreign import ccall safe "text_editor_padding"
  text_editor_padding :: SelfPtr -> CFloat -> CFloat -> CFloat -> CFloat -> IO (SelfPtr)

foreign import ccall safe "text_editor_height"
  text_editor_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

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
    selfPtr <- text_editor_new details.content
    updatedSelf <- applyAttributes selfPtr details.attributes
    text_editor_into_element updatedSelf

instance UseAttribute SelfPtr (Attribute message) where
  useAttribute selfPtr attribute = do
    case attribute of
      AddOnAction callback -> useOnAction callback selfPtr
      AddPadding value -> usePadding value selfPtr
      Height len -> useHeight len selfPtr

instance UsePadding (Attribute message) where
  paddingToAttribute value = AddPadding value

instance UseHeight (Attribute message) where
  height len = Height len

textEditor :: [Attribute message] -> Content -> Element
textEditor attributes content = pack TextEditor { .. }

onAction :: OnAction message -> Attribute message
onAction callback = AddOnAction callback

useOnAction :: OnAction message -> AttributeFn
useOnAction callback selfPtr = do
  onActionPtr <- makeCallback $ wrapOnAction callback
  text_editor_on_action selfPtr onActionPtr

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } selfPtr = do
  text_editor_padding selfPtr (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  text_editor_height selfPtr nativeLen

newContent :: Content
newContent = content_new
