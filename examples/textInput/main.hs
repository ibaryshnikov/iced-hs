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

update :: Model -> Message -> Model
update model message = case message of
  Input s -> model { inputValue = s }
  Submit -> case model.inputValue of
    "" -> model -- filter empty strings
    value -> let items = model.items ++ [value]
      in model { items = items, inputValue = "" }

view :: Model -> Element
view model =
  let input = textInput [onInput Input, onSubmit Submit] "Placeholder" model.inputValue
      labels = map (text []) model.items
  in column [] ([input] ++ labels)

main :: IO ()
main = Iced.run [] "TextInput" initModel update view
