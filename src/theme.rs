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
