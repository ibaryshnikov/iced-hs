{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import System.Directory (doesFileExist)

import Iced
import Iced.Attribute
import Iced.Task qualified as Task
import Iced.Widget
import Iced.Widget.TextEditor qualified as TextEditor

data Model = Model {content :: TextEditor.Content}

data Message
  = EditorAction TextEditor.Action
  | ReadFile
  | FileContents TextEditor.Content

getFilePath :: IO String
getFilePath = do
  exists <- doesFileExist "Main.hs"
  pure $
    if exists
      then "Main.hs"
      else "examples/readFile/Main.hs"

commandFn :: IO Message
commandFn = do
  string <- readFile =<< getFilePath
  content <- TextEditor.contentWithText string
  return $ FileContents content

update :: Message -> Model -> IO (Model, Task Message)
update message model = case message of
  EditorAction action -> do
    TextEditor.perform model.content action
    pure (model, Task.none)
  FileContents content -> pure (model{content = content}, Task.none)
  ReadFile -> pure (model, Task.performBlocking commandFn)

view :: Model -> Element
view model =
  column
    []
    [ textEditor [height (Fixed 500), onAction EditorAction] model.content
    , button [onPress ReadFile] "Read file"
    ]

main :: IO ()
main = do
  content <- TextEditor.newContent
  let model = Model{content = content}
  Iced.run [] "ReadFile" model update view
