{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Widget
import Iced.Widget.ComboBox qualified as ComboBox

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
  languages :: ComboBox.State,
  selected :: Maybe Language,
  input :: String,
  text :: String
}

data Message
  = Selected Language
  | Input String
  | OptionHovered Language
  | Closed

update :: Model -> Message -> Model
update model message = case message of
  Selected language -> model {
    selected = Just language,
    text = hello language
  }
  Input value -> model { input = value }
  OptionHovered language -> model { text = hello language }
  Closed -> model { text = string } where
    string = case model.selected of
      Just language -> hello language
      Nothing -> ""

view :: Model -> Element
view model =
  center [] $
  column [width Fill, alignItems Center, spacing 10] [
    text [] $ "Input value: " ++ model.input,
    text [] model.text,
    text [] "What is your language?",
    widget,
    spaceHeight (Fixed 150)
  ] where
    widget = comboBox [
        width (Fixed 250),
        onInput Input,
        onOptionHovered OptionHovered,
        onClose Closed
      ] model.languages "Type a language..." model.selected Selected

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

main :: IO ()
main = do
  state <- ComboBox.newState options
  let model = Model { languages = state, selected = Nothing, input = "", text = "" }
  Iced.run [] "ComboBox" model update view
