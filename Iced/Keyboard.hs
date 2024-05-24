{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Keyboard (
  onKeyPress,
  onKeyRelease,
) where

import Control.Monad
import Foreign
import Foreign.C.Types

import Iced.Keyboard.Key
import Iced.Keyboard.PhysicalKey
import Iced.Keyboard.LogicalKey
import Iced.Subscription

-- Enum is 0-indexed
namedFromFFI :: CInt -> LogicalKey
namedFromFFI = Named . toEnum . fromIntegral

-- Enum is 0-indexed
keyCodeFomFFI :: CInt -> KeyCode
keyCodeFomFFI = toEnum . fromIntegral

type OnKey message = KeyCode -> message
type NativeOnKey message = CInt -> CInt -> IO (StablePtr message)

foreign import ccall "wrapper"
  makeOnKeyCallback :: NativeOnKey message -> IO (FunPtr (NativeOnKey message))

wrapOnKey :: OnKey message -> NativeOnKey message
wrapOnKey callback rawKey _modifiers = do
  -- let key = namedFromFFI rawKey
  let key = keyCodeFomFFI rawKey
  newStablePtr $ callback key

foreign import ccall "keyboard_on_key_press"
  on_key_press :: FunPtr (NativeOnKey message) -> IO (Subscription message)

onKeyPress :: OnKey message -> IO (Subscription message)
onKeyPress = on_key_press <=< makeOnKeyCallback . wrapOnKey

foreign import ccall "keyboard_on_key_release"
  on_key_release :: FunPtr (NativeOnKey message) -> IO (Subscription message)

onKeyRelease :: OnKey message -> IO (Subscription message)
onKeyRelease = on_key_release <=< makeOnKeyCallback . wrapOnKey
