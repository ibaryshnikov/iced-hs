{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Model = Model { content :: Content }

data Message = EditorAction Action

update :: Model -> Message -> IO Model
update model message = case message of
  EditorAction action -> do
    applyAction model.content action
    pure model

view :: Model -> Element
view model = textEditor [height (Fixed 500), onAction EditorAction] model.content

main :: IO ()
main = do
  content <- newContent
  let model = Model { content = content }
  Iced.run [] "TextEditor" model update view
  freeContent content
