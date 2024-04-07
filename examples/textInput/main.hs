{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Widget

data Model = Model {
    input :: String,
    items :: [String]
}

data Message = Input String | Submit

update :: Model -> Message -> Model
update model message = case message of
  Input value -> model { input = value }
  Submit -> case model.input of
    "" -> model -- filter empty strings
    value -> model { items = items, input = "" }
      where items = [value] ++ model.items

view :: Model -> Element
view model = column [] ([input] ++ labels)
  where input = textInput [onInput Input, onSubmit Submit] "Placeholder" model.input
        labels = map (text []) model.items

main :: IO ()
main = Iced.run [] "TextInput" model update view
  where model = Model { input = "", items = [] }
