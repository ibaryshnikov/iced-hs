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

update :: Message -> Model -> Model
update message model = case message of
  Input value -> model { input = value }
  Submit -> case model.input of
    "" -> model -- filter empty strings
    value -> model { items = items, input = "" }
      where items = [value] ++ model.items

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
