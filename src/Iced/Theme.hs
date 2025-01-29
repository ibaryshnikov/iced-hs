module Iced.Theme where

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
