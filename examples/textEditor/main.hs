{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget
import Iced.Widget.TextEditor qualified as TextEditor

data Model = Model { content :: TextEditor.Content }

data Message = EditorAction TextEditor.Action

update :: Model -> Message -> IO Model
update model message = case message of
  EditorAction action -> do
    TextEditor.perform model.content action
    pure model

view :: Model -> Element
view model = textEditor [height (Fixed 500), onAction EditorAction] model.content

main :: IO ()
main = do
  content <- TextEditor.newContent
  let model = Model { content = content }
  Iced.run [] "TextEditor" model update view
