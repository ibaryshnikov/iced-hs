{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Iced
import Iced.Font
import Iced.Length
import Iced.Widget
import qualified Iced.Widget.Column as Column
import qualified Iced.Widget.Container as Container
import qualified Iced.Widget.Row as Row

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

makeContainer :: Element -> Element
makeContainer content =
  let attributes = [
         Container.centerX,
         Container.centerY,
         Container.width Fill,
         Container.height Fill
       ]
  in container attributes content

view :: Model -> Element
view model = makeContainer $ column [Column.spacing 20] [
    checkbox [onToggle DefaultToggled] "Default" model.def,
    row [Row.spacing 20] (map styledCheckbox pairs),
    checkbox [onToggle CustomToggled] "Custom" model.custom
  ]
  where
    pairs = [(Primary, "Primary"), (Secondary, "Secondary"), (Success, "Success"), (Danger, "Danger")]
    styledCheckbox (styleValue, label) =
      let base = [style styleValue]
          attributes = if model.def then base ++ [onToggle StyledToggled] else base
      in checkbox attributes label model.styled

initModel :: Model
initModel = Model { def = False, styled = False, custom = False }

main :: IO ()
main = do
  let bytes = $(includeBytes "fonts/icons.ttf")
  Iced.run [Fonts bytes] "Checkbox" initModel update view
