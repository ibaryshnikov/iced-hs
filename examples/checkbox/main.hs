{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Widget

data Model = Model {
  default_ :: Bool,
  styled :: Bool,
  custom :: Bool
}

data Message = DefaultToggled Bool | CustomToggled Bool | StyledToggled Bool

update :: Model -> Message -> IO (Model)
update model message = do
  return $ case message of
    DefaultToggled default_ -> model { default_ = default_ }
    StyledToggled styled -> model { styled = styled }
    CustomToggled custom -> model { custom = custom }

view :: Model -> IO (Element)
view model = do
  defaultCheckbox <- checkbox "Default" model.default_ DefaultToggled

  let styledCheckbox label = checkbox label model.styled StyledToggled
  primary <- styledCheckbox "Primary"
  secondary <- styledCheckbox "Secondary"
  success <- styledCheckbox "Success"
  danger <- styledCheckbox "Danger"
  checkboxes <- row [primary, secondary, success, danger]

  customCheckbox <- checkbox "Custom" model.custom CustomToggled

  column [defaultCheckbox, checkboxes, customCheckbox]

initModel :: Model
initModel = Model { default_ = False, styled = False, custom = False }

main :: IO ()
main = do
  Iced.run "Checkbox - Iced" initModel update view
