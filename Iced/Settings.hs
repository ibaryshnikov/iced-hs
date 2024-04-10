module Iced.Settings (newSettings, useFont, SettingsPtr) where

import Data.Word
import Foreign
import Foreign.C.Types

data NativeSettings
type SettingsPtr = Ptr NativeSettings

foreign import ccall safe "settings_new"
  settings_new :: IO (SettingsPtr)

foreign import ccall safe "settings_add_font"
  settings_add_font :: SettingsPtr -> Ptr Word8 -> CUInt -> IO ()

newSettings :: IO (SettingsPtr)
newSettings = settings_new

useFont :: SettingsPtr -> [Word8] -> IO ()
useFont settingsPtr bytes = do
  let len = length bytes
  fontPtr <- newArray bytes
  settings_add_font settingsPtr fontPtr (fromIntegral len)
