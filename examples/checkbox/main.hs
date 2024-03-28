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

update :: Model -> Message -> Model
update model message = case message of
  DefaultToggled default_ -> model { default_ = default_ }
  StyledToggled styled -> model { styled = styled }
  CustomToggled custom -> model { custom = custom }

view :: Model -> Element
view model =
  let defaultCheckbox = checkbox [onToggle DefaultToggled] "Default" model.default_

      styledCheckbox label = checkbox [onToggle StyledToggled] label model.styled

      primary = styledCheckbox "Primary"
      secondary = styledCheckbox "Secondary"
      success = styledCheckbox "Success"
      danger = styledCheckbox "Danger"

      checkboxes = row [] [primary, secondary, success, danger]

      customCheckbox = checkbox [onToggle CustomToggled] "Custom" model.custom
  in column [] [defaultCheckbox, checkboxes, customCheckbox]

initModel :: Model
initModel = Model { default_ = False, styled = False, custom = False }

main :: IO ()
main = do Iced.run "Checkbox" initModel update view
