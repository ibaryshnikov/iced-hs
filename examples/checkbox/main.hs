{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Font
import Iced.Widget

data Model = Model {
  def :: Bool,
  styled :: Bool,
  custom :: Bool
}

data Message = DefaultToggled Bool | CustomToggled Bool | StyledToggled Bool

update :: Model -> Message -> Model
update model message = case message of
  DefaultToggled value -> model { def = value }
  StyledToggled value -> model { styled = value }
  CustomToggled value -> model { custom = value }

view :: Model -> Element
view model = container [centerX, centerY, width Fill, height Fill] $
  column [spacing 20] [
    checkbox [onToggle DefaultToggled] "Default" model.def,
    row [spacing 20] (map styledCheckbox pairs),
    checkbox [onToggle CustomToggled] "Custom" model.custom
  ]
  where
    pairs = [(Primary, "Primary"), (Secondary, "Secondary"), (Success, "Success"), (Danger, "Danger")]
    styledCheckbox (value, label) = checkbox attributes label model.styled
      where attributes = [style value, onToggleIf model.def StyledToggled]

initModel :: Model
initModel = Model { def = False, styled = False, custom = False }

main :: IO ()
main = do
  let bytes = $(includeBytes "fonts/icons.ttf")
  Iced.run [Fonts bytes] "Checkbox" initModel update view
