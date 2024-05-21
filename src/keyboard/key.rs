use iced::keyboard::key::Named;

use Named::*;

pub fn named_to_int(key: Named) -> i32 {
    match key {
        Alt => 0,
        AltGraph => 1,
        CapsLock => 2,
        Control => 3,
        Fn => 4,
        FnLock => 5,
        NumLock => 6,
        ScrollLock => 7,
        Shift => 8,
        Symbol => 9,
        SymbolLock => 10,
        Meta => 11,
        Hyper => 12,
        Super => 13,
        Enter => 14,
        Tab => 15,
        Space => 16,
        ArrowDown => 17,
        ArrowLeft => 18,
        ArrowRight => 19,
        ArrowUp => 20,
        End => 21,
        Home => 22,
        PageDown => 23,
        PageUp => 24,
        Backspace => 25,
        Clear => 26,
        Copy => 27,
        CrSel => 28,
        Cut => 29,
        Delete => 30,
        EraseEof => 31,
        ExSel => 32,
        Insert => 33,
        Paste => 34,
        Redo => 35,
        Undo => 36,
        Accept => 37,
        Again => 38,
        Attn => 39,
        Cancel => 40,
        ContextMenu => 41,
        Escape => 42,
        Execute => 43,
        Find => 44,
        Help => 45,
        Pause => 46,
        Play => 47,
        Props => 48,
        Select => 49,
        ZoomIn => 50,
        ZoomOut => 51,
        BrightnessDown => 52,
        BrightnessUp => 53,
        Eject => 54,
        LogOff => 55,
        Power => 56,
        PowerOff => 57,
        PrintScreen => 58,
        Hibernate => 59,
        Standby => 60,
        WakeUp => 61,
        AllCandidates => 62,
        Alphanumeric => 63,
        CodeInput => 64,
        Compose => 65,
        Convert => 66,
        FinalMode => 67,
        GroupFirst => 68,
        GroupLast => 69,
        GroupNext => 70,
        GroupPrevious => 71,
        ModeChange => 72,
        NextCandidate => 73,
        NonConvert => 74,
        PreviousCandidate => 75,
        Process => 76,
        SingleCandidate => 77,
        HangulMode => 78,
        HanjaMode => 79,
        JunjaMode => 80,
        Eisu => 81,
        Hankaku => 82,
        Hiragana => 83,
        HiraganaKatakana => 84,
        KanaMode => 85,
        KanjiMode => 86,
        Katakana => 87,
        Romaji => 88,
        Zenkaku => 89,
        ZenkakuHankaku => 90,
        Soft1 => 91,
        Soft2 => 92,
        Soft3 => 93,
        Soft4 => 94,
        ChannelDown => 95,
        ChannelUp => 96,
        Close => 97,
        MailForward => 98,
        MailReply => 99,
        MailSend => 100,
        MediaClose => 101,
        MediaFastForward => 102,
        MediaPause => 103,
        MediaPlay => 104,
        MediaPlayPause => 105,
        MediaRecord => 106,
        MediaRewind => 107,
        MediaStop => 108,
        MediaTrackNext => 109,
        MediaTrackPrevious => 110,
        New => 111,
        Open => 112,
        Print => 113,
        Save => 114,
        SpellCheck => 115,
        Key11 => 116,
        Key12 => 117,
        AudioBalanceLeft => 118,
        AudioBalanceRight => 119,
        AudioBassBoostDown => 120,
        AudioBassBoostToggle => 121,
        AudioBassBoostUp => 122,
        AudioFaderFront => 123,
        AudioFaderRear => 124,
        AudioSurroundModeNext => 125,
        AudioTrebleDown => 126,
        AudioTrebleUp => 127,
        AudioVolumeDown => 128,
        AudioVolumeUp => 129,
        AudioVolumeMute => 130,
        MicrophoneToggle => 131,
        MicrophoneVolumeDown => 132,
        MicrophoneVolumeUp => 133,
        MicrophoneVolumeMute => 134,
        SpeechCorrectionList => 135,
        SpeechInputToggle => 136,
        LaunchApplication1 => 137,
        LaunchApplication2 => 138,
        LaunchCalendar => 139,
        LaunchContacts => 140,
        LaunchMail => 141,
        LaunchMediaPlayer => 142,
        LaunchMusicPlayer => 143,
        LaunchPhone => 144,
        LaunchScreenSaver => 145,
        LaunchSpreadsheet => 146,
        LaunchWebBrowser => 147,
        LaunchWebCam => 148,
        LaunchWordProcessor => 149,
        BrowserBack => 150,
        BrowserFavorites => 151,
        BrowserForward => 152,
        BrowserHome => 153,
        BrowserRefresh => 154,
        BrowserSearch => 155,
        BrowserStop => 156,
        AppSwitch => 157,
        Call => 158,
        Camera => 159,
        CameraFocus => 160,
        EndCall => 161,
        GoBack => 162,
        GoHome => 163,
        HeadsetHook => 164,
        LastNumberRedial => 165,
        Notification => 166,
        MannerMode => 167,
        VoiceDial => 168,
        TV => 169,
        TV3DMode => 170,
        TVAntennaCable => 171,
        TVAudioDescription => 172,
        TVAudioDescriptionMixDown => 173,
        TVAudioDescriptionMixUp => 174,
        TVContentsMenu => 175,
        TVDataService => 176,
        TVInput => 177,
        TVInputComponent1 => 178,
        TVInputComponent2 => 179,
        TVInputComposite1 => 180,
        TVInputComposite2 => 181,
        TVInputHDMI1 => 182,
        TVInputHDMI2 => 183,
        TVInputHDMI3 => 184,
        TVInputHDMI4 => 185,
        TVInputVGA1 => 186,
        TVMediaContext => 187,
        TVNetwork => 188,
        TVNumberEntry => 189,
        TVPower => 190,
        TVRadioService => 191,
        TVSatellite => 192,
        TVSatelliteBS => 193,
        TVSatelliteCS => 194,
        TVSatelliteToggle => 195,
        TVTerrestrialAnalog => 196,
        TVTerrestrialDigital => 197,
        TVTimer => 198,
        AVRInput => 199,
        AVRPower => 200,
        ColorF0Red => 201,
        ColorF1Green => 202,
        ColorF2Yellow => 203,
        ColorF3Blue => 204,
        ColorF4Grey => 205,
        ColorF5Brown => 206,
        ClosedCaptionToggle => 207,
        Dimmer => 208,
        DisplaySwap => 209,
        DVR => 210,
        Exit => 211,
        FavoriteClear0 => 212,
        FavoriteClear1 => 213,
        FavoriteClear2 => 214,
        FavoriteClear3 => 215,
        FavoriteRecall0 => 216,
        FavoriteRecall1 => 217,
        FavoriteRecall2 => 218,
        FavoriteRecall3 => 219,
        FavoriteStore0 => 220,
        FavoriteStore1 => 221,
        FavoriteStore2 => 222,
        FavoriteStore3 => 223,
        Guide => 224,
        GuideNextDay => 225,
        GuidePreviousDay => 226,
        Info => 227,
        InstantReplay => 228,
        Link => 229,
        ListProgram => 230,
        LiveContent => 231,
        Lock => 232,
        MediaApps => 233,
        MediaAudioTrack => 234,
        MediaLast => 235,
        MediaSkipBackward => 236,
        MediaSkipForward => 237,
        MediaStepBackward => 238,
        MediaStepForward => 239,
        MediaTopMenu => 240,
        NavigateIn => 241,
        NavigateNext => 242,
        NavigateOut => 243,
        NavigatePrevious => 244,
        NextFavoriteChannel => 245,
        NextUserProfile => 246,
        OnDemand => 247,
        Pairing => 248,
        PinPDown => 249,
        PinPMove => 250,
        PinPToggle => 251,
        PinPUp => 252,
        PlaySpeedDown => 253,
        PlaySpeedReset => 254,
        PlaySpeedUp => 255,
        RandomToggle => 256,
        RcLowBattery => 257,
        RecordSpeedNext => 258,
        RfBypass => 259,
        ScanChannelsToggle => 260,
        ScreenModeNext => 261,
        Settings => 262,
        SplitScreenToggle => 263,
        STBInput => 264,
        STBPower => 265,
        Subtitle => 266,
        Teletext => 267,
        VideoModeNext => 268,
        Wink => 269,
        ZoomToggle => 270,
        F1 => 271,
        F2 => 272,
        F3 => 273,
        F4 => 274,
        F5 => 275,
        F6 => 276,
        F7 => 277,
        F8 => 278,
        F9 => 279,
        F10 => 280,
        F11 => 281,
        F12 => 282,
        F13 => 283,
        F14 => 284,
        F15 => 285,
        F16 => 286,
        F17 => 287,
        F18 => 288,
        F19 => 289,
        F20 => 290,
        F21 => 291,
        F22 => 292,
        F23 => 293,
        F24 => 294,
        F25 => 295,
        F26 => 296,
        F27 => 297,
        F28 => 298,
        F29 => 299,
        F30 => 300,
        F31 => 301,
        F32 => 302,
        F33 => 303,
        F34 => 304,
        F35 => 305,
    }
}
