{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget

data Language = Rust | Elm | Ruby | Haskell | C | Javascript | Other deriving (Read, Show)

data Model = Model {selected :: Maybe Language}

data Message = Selected Language

update :: Message -> Model -> Model
update message model = case message of
  Selected language -> model{selected = Just language}

options :: [Language]
options = [C, Elm, Ruby, Haskell, Rust, Javascript, Other]

view :: Model -> Element
view model =
  scrollable [] $
    column
      [width Fill, alignX Center, spacing 10]
      [ spaceHeight (Fixed 600)
      , text [] "Which is your favorite language?"
      , widget
      , spaceHeight (Fixed 600)
      ]
 where
  widget =
    pickList
      [placeholder "Choose a language..."]
      options
      model.selected
      Selected

main :: IO ()
main = Iced.run [theme Moonfly] "PickList" model update view
 where
  model = Model{selected = Nothing}
