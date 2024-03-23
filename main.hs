{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Widget

data Model = Model {
    value :: Int,
    inputValue :: String,
    items :: [String]
}

data Message = Inc | Dec | Input String | Submit

initModel :: Model
initModel = Model {
  value = 0,
  inputValue = "",
  items = []
}

update :: Model -> Message -> IO (Model)
update model message = do
  return $ case message of
    Inc -> model { value = model.value + 1 }
    Dec -> model { value = model.value - 1 }
    Input s -> model { inputValue = s }
    Submit -> let items = model.items ++ [model.inputValue]
              in model { items = items, inputValue = "" }

makeLabels :: [String] -> [Element] -> IO ([Element])
makeLabels [] elements = pure elements
makeLabels (first:remaining) elements = do
  label <- text first
  makeLabels remaining ([label] ++ elements)

view :: Model -> IO(Element)
view model = do
  buttonInc <- button "Inc" Inc
  label <- text $ "Counter value " ++ show model.value
  buttonDec <- button "Dec" Dec
  input <- textInput "Placeholder" model.inputValue Input Submit
  labels <- makeLabels model.items []
  column $ [buttonInc, label, buttonDec, input] ++ labels

main :: IO ()
main = Iced.run "Iced in Haskell" initModel update view
