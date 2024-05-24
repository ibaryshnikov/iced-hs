module Iced.Keyboard (
  onKeyPress,
  onKeyRelease,
) where

import Control.Monad
import Foreign
import Foreign.C.Types

import Iced.Keyboard.LogicalKey
import Iced.Subscription

-- Enum is 0-indexed
keyFromInt :: Int -> Named
keyFromInt = toEnum

type OnKey message = Named -> message
type NativeOnKey message = CInt -> CInt -> IO (StablePtr message)

foreign import ccall "wrapper"
  makeOnKeyCallback :: NativeOnKey message -> IO (FunPtr (NativeOnKey message))

wrapOnKey :: OnKey message -> NativeOnKey message
wrapOnKey callback rawKey _modifiers = do
  let key = keyFromInt $ fromIntegral rawKey
  newStablePtr $ callback key

foreign import ccall "keyboard_on_key_press"
  on_key_press :: FunPtr (NativeOnKey message) -> IO (Subscription message)

onKeyPress :: OnKey message -> IO (Subscription message)
onKeyPress = on_key_press <=< makeOnKeyCallback . wrapOnKey

foreign import ccall "keyboard_on_key_release"
  on_key_release :: FunPtr (NativeOnKey message) -> IO (Subscription message)

onKeyRelease :: OnKey message -> IO (Subscription message)
onKeyRelease = on_key_release <=< makeOnKeyCallback . wrapOnKey
