{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Widget

data Model = Model {
  content :: Content
}

data Message = EditorAction Action

update :: Model -> Message -> IO (Model)
update model message = case message of
  EditorAction action -> do
    applyAction model.content action
    return model

view :: Model -> IO (Element)
view model = do
  textEditor model.content EditorAction

initModel :: Model
initModel = Model { content = newContent }

main :: IO ()
main = do Iced.run "TextEditor" initModel update view
