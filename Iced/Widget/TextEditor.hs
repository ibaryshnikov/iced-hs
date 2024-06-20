{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.TextEditor (
  textEditor,
  newContent,
  contentWithText,
  perform,
  Action,
  Content,
  onAction,
) where

import Control.Monad
import Foreign
import Foreign.C.String

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Attribute.PaddingFFI
import Iced.Element

data NativeTextEditor
type Self = Ptr NativeTextEditor
type AttributeFn = Self -> IO Self
data NativeContent
type Content = ForeignPtr NativeContent
data NativeAction
type Action = Ptr NativeAction

data Attribute message
  = AddOnAction (OnAction message)
  | AddPadding Padding
  | Height Length

foreign import ccall "text_editor_content_new"
  content_new :: IO (Ptr NativeContent)

makeContent :: Ptr NativeContent -> IO Content
makeContent = newForeignPtr content_free

newContent :: IO Content
newContent = makeContent =<< content_new

foreign import ccall "text_editor_content_with_text"
  content_with_text :: CString -> IO (Ptr NativeContent)

contentWithText :: String -> IO Content
contentWithText = makeContent <=< content_with_text <=< newCString

foreign import ccall "text_editor_content_perform"
  content_perform :: Ptr NativeContent -> Action -> IO ()

perform :: Content -> Action -> IO ()
perform content action = withForeignPtr content $ \self ->
  content_perform self action

foreign import ccall "&text_editor_content_free"
  content_free :: FinalizerPtr NativeContent

foreign import ccall "text_editor_new"
  text_editor_new :: Ptr NativeContent -> IO Self

foreign import ccall "text_editor_on_action"
  text_editor_on_action :: Self -> FunPtr (NativeOnAction message) -> IO Self

-- text_editor padding
foreign import ccall "text_editor_padding"
  text_editor_padding :: Self -> PaddingPtr -> IO Self

foreign import ccall "text_editor_height"
  text_editor_height :: Self -> LengthPtr -> IO Self

foreign import ccall "text_editor_into_element"
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
    withForeignPtr details.content text_editor_new

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddOnAction callback -> useOnAction callback
    AddPadding value -> useFnIO text_editor_padding value
    Height len -> useFnIO text_editor_height len

instance UsePadding (Attribute message) where
  padding = AddPadding . paddingFromOne

instance UsePadding2 (Attribute message) where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 (Attribute message) where
  padding4 top right bottom left = AddPadding Padding { .. }

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
