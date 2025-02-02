{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.List
import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget
import Iced.Widget.Markdown qualified as Markdown

data Message = Click String

data Model = Model {
  state :: Markdown.State
}

update :: Message -> Model -> IO Model
update (Click url) model = do
  putStrLn $ "Url clicked: " ++ url
  pure model

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  column [alignX Center, spacing 10, width (Fixed 600)] [
    markdown model.state Oxocarbon Click
  ]

pageContents :: String
pageContents = intercalate "\n\r" [
    "### CommonMark Markdown example",
    "Find more at [https://commonmark.org/](https://commonmark.org/)",
    "",
    "* One",
    "  * **Two**",
    "    * Three",
    "",
    "1. One",
    "2. Two"
  ]

main :: IO ()
main = do
    state <- Markdown.newState pageContents
    let model = Model { state = state }
    Iced.run [theme GruvboxLight] "Markdown" model update view
