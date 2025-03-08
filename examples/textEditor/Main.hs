{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Color
import Iced.Style
import Iced.Theme
import Iced.Widget
import Iced.Widget.TextEditor qualified as TextEditor

data Model = Model {content :: TextEditor.Content}

data Message = EditorAction TextEditor.Action

update :: Message -> Model -> IO Model
update message model = case message of
  EditorAction action -> do
    TextEditor.perform model.content action
    pure model

editorStyle :: [TextEditor.StyleAttribute]
editorStyle =
  [ background (rgb8 50 50 50)
  , border (rgb8 0 0 0) 2 10
  ]

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
    container [width (Fixed 400)] $
      textEditor [height (Fixed 300), onAction EditorAction, style editorStyle] model.content

main :: IO ()
main = do
  content <- TextEditor.newContent
  let model = Model{content = content}
  Iced.run [theme TokyoNightStorm] "TextEditor" model update view
