{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Language
  = Danish
  | English
  | French
  | German
  | Italian
  | Portuguese
  | Spanish
  | Other deriving (Show, Read)

data Model = Model {
  languages :: ComboBoxState,
  selected :: Maybe Language,
  text :: String
}

data Message = Selected Language | OptionHovered Language | Closed

update :: Model -> Message -> Model
update model message = case message of
  Selected language -> model {
    selected = Just language,
    text = hello language
  }
  OptionHovered language -> model { text = hello language }
  Closed -> model { text = string } where
    string = case model.selected of
      Just language -> hello language
      Nothing -> ""

view :: Model -> Element
view model =
  container [width Fill, height Fill, centerX, centerY] $
  scrollable [] $
  column [width Shrink, alignItems Center, spacing 10] [
    text [] model.text,
    text [] "What is your language?",
    combo,
    spaceHeight (Fixed 150)
  ] where
    combo = comboBox [onOptionHovered OptionHovered, onClose Closed]
      model.languages "Type a language..." model.selected Selected

hello :: Language -> String
hello language = case language of
  Danish -> "Halloy!"
  English -> "Hello!"
  French -> "Salut!"
  German -> "Hallo!"
  Italian -> "Ciao!"
  Portuguese -> "Olá!"
  Spanish -> "¡Hola!"
  Other -> "... hello?"

options :: [Language]
options = [Danish, English, French, German, Italian, Portuguese, Spanish, Other]

initModel :: ComboBoxState -> Model
initModel state = Model { languages = state, selected = Nothing, text = "" }

main :: IO ()
main = do
  state <- newComboBoxState options
  let model = initModel state
  Iced.run [] "Combo Box - Iced" model update view
  freeComboBoxState state
