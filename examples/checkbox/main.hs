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
view model = column [] [
    checkbox [onToggle DefaultToggled] "Default" model.default_,
    row [] (map styled ["Primary", "Secondary", "Success", "Danger"]),
    checkbox [onToggle CustomToggled] "Custom" model.custom
  ]
  where styled label = checkbox [onToggle StyledToggled] label model.styled

initModel :: Model
initModel = Model { default_ = False, styled = False, custom = False }

main :: IO ()
main = do Iced.run "Checkbox" initModel update view
