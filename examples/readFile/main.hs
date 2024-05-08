{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Command
import Iced.Widget
import Iced.Widget.TextEditor qualified as TextEditor

data Model = Model { content :: TextEditor.Content }

data Message
  = EditorAction TextEditor.Action
  | ReadFile
  | FileContents TextEditor.Content

commandFn :: IO (Message)
commandFn = do
  string <- readFile "main.hs"
  content <- TextEditor.contentWithText string
  return $ FileContents content

update :: Model -> Message -> IO (Model, Command Message)
update model message = case message of
  EditorAction action -> do
    TextEditor.perform model.content action
    pure (model, None)
  FileContents content -> pure (model { content = content }, None)
  ReadFile -> pure (model, PerformIO commandFn)

view :: Model -> Element
view model = column [] [
    textEditor [height (Fixed 500), onAction EditorAction] model.content,
    button [onPress ReadFile] "Read file"
  ]

main :: IO ()
main = do
  content <- TextEditor.newContent
  let model = Model { content = content }
  Iced.run [] "ReadFile" model update view
