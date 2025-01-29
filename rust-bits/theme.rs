use std::ffi::c_uchar;

use iced::Theme::{self, *};

use crate::Model;

pub type ThemeFn = extern "C" fn(model: Model) -> c_uchar;

pub fn theme_from_raw(raw: c_uchar) -> Theme {
    match raw {
        0 => Light,
        1 => Dark,
        2 => Dracula,
        3 => Nord,
        4 => SolarizedLight,
        5 => SolarizedDark,
        6 => GruvboxLight,
        7 => GruvboxDark,
        8 => CatppuccinLatte,
        9 => CatppuccinFrappe,
        10 => CatppuccinMacchiato,
        11 => CatppuccinMocha,
        12 => TokyoNight,
        13 => TokyoNightStorm,
        14 => TokyoNightLight,
        15 => KanagawaWave,
        16 => KanagawaDragon,
        17 => KanagawaLotus,
        18 => Moonfly,
        19 => Nightfly,
        20 => Oxocarbon,
        21 => Ferra,
        other => panic!("Unexpected value in theme_from_raw: {other}"),
    }
}

pub fn theme_to_raw(theme: &Theme) -> c_uchar {
    match theme {
        Light => 0,
        Dark => 1,
        Dracula => 2,
        Nord => 3,
        SolarizedLight => 4,
        SolarizedDark => 5,
        GruvboxLight => 6,
        GruvboxDark => 7,
        CatppuccinLatte => 8,
        CatppuccinFrappe => 9,
        CatppuccinMacchiato => 10,
        CatppuccinMocha => 11,
        TokyoNight => 12,
        TokyoNightStorm => 13,
        TokyoNightLight => 14,
        KanagawaWave => 15,
        KanagawaDragon => 16,
        KanagawaLotus => 17,
        Moonfly => 18,
        Nightfly => 19,
        Oxocarbon => 20,
        Ferra => 21,
        Custom(_) => panic!("Custom themes are not supported yet"),
    }
}
