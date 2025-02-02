{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Font
import Iced.Theme
import Iced.Widget
import Iced.Widget.Checkbox (BasicStyle(..))

data Model = Model {
  def :: Bool,
  styled :: Bool,
  custom :: Bool
}

data Message = Default Bool | Custom Bool | Styled Bool

update :: Message -> Model -> Model
update message model = case message of
  Default value -> model { def = value }
  Styled value -> model { styled = value }
  Custom value -> model { custom = value }

view :: Model -> Element
view model =
  center [] $
  column [spacing 20] [
    checkbox [onToggle Default] "Default" model.def,
    row [spacing 20] (map styled pairs),
    checkbox [icon 59649, onToggle Custom] "Custom" model.custom -- use icon id
  ]
  where
    pairs = [(Primary, "Primary"), (Secondary, "Secondary"), (Success, "Success"), (Danger, "Danger")]
    styled (value, label) = checkbox attributes label model.styled
      where attributes = [style value, onToggleIf model.def Styled]

main :: IO ()
main = Iced.run [addFont bytes, theme SolarizedDark] "Checkbox" model update view
  where bytes = $(includeBytes "fonts/icons.ttf")
        model = Model { def = False, styled = False, custom = False }
