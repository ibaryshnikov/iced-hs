module Iced.Application (run, addFont) where

import Data.Word
import Foreign
import Foreign.C.String

import Iced.Element (Element, ElementPtr, elementToNative)
import Iced.Settings

data Attribute = Font [Word8]

foreign import ccall safe "run_app"
  run_app :: SettingsPtr
          -> CString
          -> StablePtr model
          -> FunPtr (NativeUpdate model message)
          -> FunPtr (NativeView model)
          -> IO ()

foreign import ccall "wrapper"
  makeUpdateCallback :: NativeUpdate model message -> IO (FunPtr (NativeUpdate model message))

type NativeUpdate model message = StablePtr model -> StablePtr message -> IO (StablePtr model)

wrapUpdate :: Update model message -> StablePtr model -> StablePtr message -> IO (StablePtr model)
wrapUpdate update model_ptr message_ptr = do
  model <- deRefStablePtr model_ptr
  message <- deRefStablePtr message_ptr
  let newModel = update model message
  -- better call it from Rust after we change the pointer to a new one
  freeStablePtr model_ptr
  -- do something with message_ptr too
  -- some messages are just Inc | Dec, and persist
  -- others are Input String, and must be deallocated
  newStablePtr newModel

foreign import ccall "wrapper"
  makeViewCallback :: NativeView model -> IO (FunPtr (NativeView model))

type NativeView model = StablePtr model -> IO (ElementPtr)

wrapView :: View model -> StablePtr model -> IO (ElementPtr)
wrapView view modelPtr = do
  model <- deRefStablePtr modelPtr
  elementToNative $ view model

useAttribute :: SettingsPtr -> Attribute -> IO ()
useAttribute settingsPtr attribute = do
  case attribute of
    Font bytes -> useFont settingsPtr bytes

applyAttributes :: SettingsPtr -> [Attribute] -> IO ()
applyAttributes _settingsPtr [] = pure ()
applyAttributes settingsPtr (attribute:remaining) = do
  useAttribute settingsPtr attribute
  applyAttributes settingsPtr remaining

type Update model message = model -> message -> model
type View model = model -> Element

run :: [Attribute] -> String -> model -> Update model message -> View model -> IO ()
run attributes title model update view = do
  titlePtr <- newCString title
  modelPtr <- newStablePtr model
  updatePtr <- makeUpdateCallback $ wrapUpdate update
  viewPtr <- makeViewCallback $ wrapView view
  settingsPtr <- newSettings
  applyAttributes settingsPtr attributes
  run_app settingsPtr titlePtr modelPtr updatePtr viewPtr

addFont :: [Word8] -> Attribute
addFont bytes = Font bytes
