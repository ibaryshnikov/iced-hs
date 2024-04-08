{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Option = First | Second | Third deriving Enum

data Model = Model { selected :: Maybe Option }

data Message = Selected Option | Clear

update :: Model -> Message -> Model
update model message = case message of
  Selected option -> model { selected = Just option }
  Clear -> model { selected = Nothing }

view :: Model -> Element
view model =
  container [centerX, centerY, width Fill, height Fill] $
  column [spacing 50, alignItems Center] [
    row [spacing 20] [
      radio [] "First" First model.selected Selected,
      radio [] "Second" Second model.selected Selected,
      radio [] "Third" Third model.selected Selected
    ],
    button [onPress Clear] "Clear"
  ]

main :: IO ()
main = Iced.run [] "Radio" model update view
  where model = Model { selected = Nothing }
