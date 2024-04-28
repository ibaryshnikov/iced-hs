{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Command
import Iced.Widget

data Model = Model { content :: Content }

data Message = EditorAction Action | ReadFile | FileContents Content

commandFn :: IO (Message)
commandFn = do
  string <- readFile "main.hs"
  content <- contentWithText string
  return $ FileContents content

update :: Model -> Message -> IO (Model, Command Message)
update model message = case message of
  EditorAction action -> do
    contentPerform model.content action
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
  content <- newContent
  let model = Model { content = content }
  Iced.run [] "ReadFile" model update view
