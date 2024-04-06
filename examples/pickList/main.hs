{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Language = Rust | Elm | Ruby | Haskell | C | Javascript | Other deriving (Show, Read)

data Model = Model {
  selectedLanguage :: Maybe Language
}

data Message = LanguageSelected Language

update :: Model -> Message -> Model
update model message = case message of
  LanguageSelected language -> model { selectedLanguage = Just language }

options :: [Language]
options = [C, Elm, Ruby, Haskell, Rust, Javascript, Other]

view :: Model -> Element
view model = scrollable [] $
  column [width Fill, alignItems Center, spacing 10] [
    spaceHeight (Fixed 600),
    text [] "Which is your favorite language?",
    list,
    spaceHeight (Fixed 600)
  ]
  where
    list = pickList [placeholder "Choose a language..."]
      options model.selectedLanguage LanguageSelected

initModel :: Model
initModel = Model { selectedLanguage = Nothing }

main :: IO ()
main = do Iced.run [] "Pick List - Iced" initModel update view
