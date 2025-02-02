{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget

data Option = First | Second | Third deriving Enum

data Model = Model { selected :: Maybe Option }

data Message = Selected Option | Clear

update :: Message -> Model -> Model
update message model = case message of
  Selected option -> model { selected = Just option }
  Clear -> model { selected = Nothing }

view :: Model -> Element
view model =
  center [] $
  column [spacing 50, alignX Center] [
    row [spacing 20] [
      radio [] "First" First model.selected Selected,
      radio [] "Second" Second model.selected Selected,
      radio [] "Third" Third model.selected Selected
    ],
    button [onPress Clear] "Clear"
  ]

main :: IO ()
main = Iced.run [theme TokyoNightLight] "Radio" model update view
  where model = Model { selected = Nothing }
