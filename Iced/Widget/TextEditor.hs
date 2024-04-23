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
import Control.Monad
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
  pure content

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
  attributes :: [Attribute message],
  content :: Content
}

instance IntoNative (TextEditor message) where
  toNative details = do
    text_editor_new details.content
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddOnAction callback -> useOnAction callback
    AddPadding value -> usePadding value
    Height len -> useHeight len

instance UsePadding (Attribute message) where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute (Attribute message) where
  paddingToAttribute = AddPadding

instance UseHeight Length (Attribute message) where
  height = Height

textEditor :: [Attribute message] -> Content -> Element
textEditor attributes content = pack TextEditor { .. }

onAction :: OnAction message -> Attribute message
onAction = AddOnAction

useOnAction :: OnAction message -> AttributeFn
useOnAction callback self =
  makeCallback (wrapOnAction callback)
    >>= text_editor_on_action self

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  text_editor_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useHeight :: Length -> AttributeFn
useHeight len self = text_editor_height self $ lengthToNative len

newContent :: Content
newContent = text_editor_content_new

contentWithText :: String -> IO (Content)
contentWithText = text_editor_content_with_text <=< newCString
