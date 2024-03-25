{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Widget

data Model = Model {
  content :: Content
}

data Message = NewAction Action

update :: Model -> Message -> IO (Model)
update model message = case message of
  NewAction action -> do
    applyAction model.content action
    return model

view :: Model -> IO (Element)
view model = do
  textEditor model.content (\action -> NewAction action)

main :: IO ()
main = do
  let model = Model { content = newContent }
  Iced.run "TextEditor" model update view
