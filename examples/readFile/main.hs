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

update :: Model -> Message -> (Model, Command Message)
update model message = case message of
  EditorAction action -> (model { content = applyAction model.content action }, None)
  FileContents content -> (model { content = content }, None)
  ReadFile -> (model, Perform commandFn)

view :: Model -> Element
view model = column [] [
    textEditor [height (Fixed 500), onAction EditorAction] model.content,
    button [onPress ReadFile] "Read file"
  ]

main :: IO ()
main = Iced.run [] "ReadFile" model update view
  where model = Model { content = newContent }
