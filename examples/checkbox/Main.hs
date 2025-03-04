{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Data.ByteString qualified as ByteString
import System.Directory (doesFileExist)

import Iced
import Iced.Attribute
import Iced.Theme
import Iced.Widget
import Iced.Widget.Checkbox (BasicStyle (..))

data Model = Model
  { def :: Bool
  , styled :: Bool
  , custom :: Bool
  }

data Message = Default Bool | Custom Bool | Styled Bool

update :: Message -> Model -> Model
update message model = case message of
  Default value -> model{def = value}
  Styled value -> model{styled = value}
  Custom value -> model{custom = value}

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
    column
      [spacing 20]
      [ checkbox [onToggle Default] "Default" model.def
      , row [spacing 20] (map styled pairs)
      , checkbox [icon 59649, onToggle Custom] "Custom" model.custom -- use icon id
      ]
 where
  pairs =
    [(Primary, "Primary"), (Secondary, "Secondary"), (Success, "Success"), (Danger, "Danger")]
  styled (value, label) = checkbox attributes label model.styled
   where
    attributes = [style value, onToggleIf model.def Styled]

getFontPath :: IO String
getFontPath = do
  exists <- doesFileExist "fonts/icons.ttf"
  pure $
    if exists
      then "fonts/icons.ttf"
      else "examples/checkbox/fonts/icons.ttf"

main :: IO ()
main = do
  bytes <- fmap ByteString.unpack $ ByteString.readFile =<< getFontPath
  let model = Model{def = False, styled = False, custom = False}
  Iced.run [addFont bytes, theme SolarizedDark] "Checkbox" model update view
