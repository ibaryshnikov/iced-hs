# Themes

There's a number of built-in themes in iced. Check the
complete [example](../examples/themes) for more details

```haskell
-- example
import Iced
import Iced.Theme

-- theme can be defined as a value
main :: IO ()
main = Iced.run [theme Oxocarbon] "App" model update view

-- or as a function
themeFn :: Model -> Theme
themeFn model = case model.selected of
  Just value -> value
  Nothing -> Oxocarbon

-- assuming the Model is
data Model = Model { selected :: Maybe Theme }

main :: IO ()
main = Iced.run [theme themeFn] "App" model update view
```

List of all themes:

```haskell
data Theme
  = Light
  | Dark
  | Dracula
  | Nord
  | SolarizedLight
  | SolarizedDark
  | GruvboxLight
  | GruvboxDark
  | CatppuccinLatte
  | CatppuccinFrappe
  | CatppuccinMacchiato
  | CatppuccinMocha
  | TokyoNight
  | TokyoNightStorm
  | TokyoNightLight
  | KanagawaWave
  | KanagawaDragon
  | KanagawaLotus
  | Moonfly
  | Nightfly
  | Oxocarbon
  | Ferra
  deriving (Enum, Show, Read)
```
