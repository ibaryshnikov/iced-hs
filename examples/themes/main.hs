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
  value_ :: Bool,
  checkBoxValue :: Bool
}

data Message
  = Selected Theme
  | ChangeOption Option
  | ClearOption
  | Toggle Bool
  | ToggleCheckBox Bool

data Option = First | Second | Third deriving Enum

update :: Model -> Message -> Model
update model (Selected value_) = model { selected = Just value_ }
update model (ChangeOption value_) = model { option = Just value_ }
update model ClearOption = model { option = Nothing }
update model (Toggle value_) = model { value_ = value_ }
update model (ToggleCheckBox value_) = model { checkBoxValue = value_ }

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
    toggler [width Shrink] (lights model.value_) model.value_ Toggle,
    row [spacing 20] (map styled pairs)
  ]
  where
    pairs = [(Primary, "Primary"), (Secondary, "Secondary"), (Success, "Success"), (Danger, "Danger")]
    styled (value_, label) = checkbox attributes label model.checkBoxValue
      where attributes = [style value_, onToggle ToggleCheckBox]

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
  Just value_ -> value_
  Nothing -> Ferra

main :: IO ()
main = Iced.run [theme themeFn] "Themes" model update view
  where model = Model { selected = Nothing, option = Nothing, value_ = False, checkBoxValue = False }
