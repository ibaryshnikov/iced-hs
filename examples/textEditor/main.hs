{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Widget

data Model = Model { content :: Content }

data Message = EditorAction Action

update :: Model -> Message -> Model
update model message = case message of
  EditorAction action -> model { content = applyAction model.content action }

view :: Model -> Element
view model = textEditor [onAction EditorAction] model.content

main :: IO ()
main = Iced.run [] "TextEditor" model update view
  where model = Model { content = newContent }
