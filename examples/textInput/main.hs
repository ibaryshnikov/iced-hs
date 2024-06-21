{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget
import Iced.Theme

data Model = Model {
    input :: String,
    items :: [String]
}

data Message = Input String | Submit

update :: Model -> Message -> Model
update model message = case message of
  Input value_ -> model { input = value_ }
  Submit -> case model.input of
    "" -> model -- filter empty strings
    value_ -> model { items = items, input = "" }
      where items = [value_] ++ model.items

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  column [] ([input] ++ labels)
    where
      input = textInput [width (Fixed 300), onInput Input, onSubmit Submit]
        "Placeholder" model.input
      labels = map (text []) model.items

main :: IO ()
main = Iced.run [theme GruvboxLight] "TextInput" model update view
  where model = Model { input = "", items = [] }
