module Iced.Settings (newSettings, addFonts, SettingsPtr) where

import Data.Word
import Foreign
import Foreign.C.Types

data NativeSettings
type SettingsPtr = Ptr NativeSettings

foreign import ccall safe "new_settings"
  new_settings :: IO (SettingsPtr)

foreign import ccall safe "settings_add_fonts"
  settings_add_fonts :: SettingsPtr -> Ptr Word8 -> CInt -> IO ()

newSettings :: IO (SettingsPtr)
newSettings = new_settings

addFonts :: SettingsPtr -> [Word8] -> IO ()
addFonts settingsPtr fonts = do
  let len = length fonts
  fontsPtr <- newArray fonts
  settings_add_fonts settingsPtr fontsPtr (fromIntegral len)