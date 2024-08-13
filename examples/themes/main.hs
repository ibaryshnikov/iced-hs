{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget
import Iced.Widget.Checkbox (BasicStyle(..))

data Model = Model {
  selected :: Maybe Theme,
  option :: Maybe Option,
  value :: Bool,
  checkBoxValue :: Bool
}

data Message
  = Selected Theme
  | ChangeOption Option
  | ClearOption
  | Toggle Bool
  | ToggleCheckBox Bool

data Option = First | Second | Third deriving Enum

update :: Message -> Model -> Model
update (Selected value) model = model { selected = Just value }
update (ChangeOption value) model = model { option = Just value }
update ClearOption model = model { option = Nothing }
update (Toggle value) model = model { value = value }
update (ToggleCheckBox value) model = model { checkBoxValue = value }

view :: Model -> Element
view model = center [] $
  column [alignItems Center, spacing 20] [
    pickList [placeholder "Choose a theme..."] options model.selected Selected,
    text [] "Examples:",
    row [spacing 20] [
      radio [] "First" First model.option ChangeOption,
      radio [] "Second" Second model.option ChangeOption,
      radio [] "Third" Third model.option ChangeOption
    ],
    button [onPress ClearOption] "Clear",
    toggler [width Shrink] (lights model.value) model.value Toggle,
    row [spacing 20] (map styled pairs)
  ]
  where
    pairs = [(Primary, "Primary"), (Secondary, "Secondary"), (Success, "Success"), (Danger, "Danger")]
    styled (value, label) = checkbox attributes label model.checkBoxValue
      where attributes = [style value, onToggle ToggleCheckBox]

options :: [Theme]
options = [
    Light, Dark, Dracula, Nord, SolarizedLight, SolarizedDark, GruvboxLight, GruvboxDark,
    CatppuccinLatte, CatppuccinFrappe, CatppuccinMacchiato, CatppuccinMocha,
    TokyoNight, TokyoNightStorm, TokyoNightLight, KanagawaWave, KanagawaDragon, KanagawaLotus,
    Moonfly, Nightfly, Oxocarbon, Ferra
  ]

lights :: Bool -> String
lights True = "Lights on"
lights False = "Lights off"

themeFn :: Model -> Theme
themeFn model = case model.selected of
  Just value -> value
  Nothing -> Ferra

main :: IO ()
main = Iced.run [theme themeFn] "Themes" model update view
  where model = Model { selected = Nothing, option = Nothing, value = False, checkBoxValue = False }
