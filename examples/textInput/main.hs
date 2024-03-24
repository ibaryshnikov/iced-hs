{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Widget

data Model = Model {
    inputValue :: String,
    items :: [String]
}

data Message = Input String | Submit

initModel :: Model
initModel = Model {
  inputValue = "",
  items = []
}

update :: Model -> Message -> IO (Model)
update model message = do
  return $ case message of
    Input s -> model { inputValue = s }
    Submit -> let items = model.items ++ [model.inputValue]
              in model { items = items, inputValue = "" }

makeLabels :: [String] -> [Element] -> IO ([Element])
makeLabels [] elements = pure elements
makeLabels (first:remaining) elements = do
  label <- text first
  makeLabels remaining ([label] ++ elements)

view :: Model -> IO (Element)
view model = do
  input <- textInput "Placeholder" model.inputValue Input Submit
  labels <- makeLabels model.items []
  column $ [input] ++ labels

main :: IO ()
main = Iced.run "TextInput" initModel update view
