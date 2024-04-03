module Iced.Application (run, Attribute(..)) where

import Data.Word
import Foreign
import Foreign.C.String

import Iced.Element (Element, ElementPtr, elementToNative)
import Iced.Settings

data Attribute = Fonts [Word8]

type Update model message = StablePtr model -> StablePtr message -> IO (StablePtr model)

type View model = StablePtr model -> IO (ElementPtr)

foreign import ccall "wrapper"
  makeUpdateCallback :: Update model message -> IO (FunPtr (Update model message))

foreign import ccall "wrapper"
  makeViewCallback :: View model -> IO (FunPtr (View model))

foreign import ccall safe "run_app" run_app_ffi ::
  SettingsPtr -> CString -> StablePtr model
  -> FunPtr (Update model message) -> FunPtr (View model) -> IO ()

type UpdateCallback model message = model -> message -> model

update_hs :: UpdateCallback model message -> StablePtr model -> StablePtr message -> IO (StablePtr model)
update_hs update model_ptr message_ptr = do
  model <- deRefStablePtr model_ptr
  message <- deRefStablePtr message_ptr
  let newModel = update model message
  -- better call it from Rust after we change the pointer to a new one
  freeStablePtr model_ptr
  -- do something with message_ptr too
  -- some messages are just Inc | Dec, and persist
  -- others are Input String, and must be deallocated
  newStablePtr newModel

type ViewCallback model = model -> Element

view_hs :: ViewCallback model -> StablePtr model -> IO (ElementPtr)
view_hs view modelPtr = do
  model <- deRefStablePtr modelPtr
  elementToNative $ view model

useAttribute :: SettingsPtr -> Attribute -> IO ()
useAttribute settingsPtr attribute = do
  case attribute of
    Fonts bytes -> addFonts settingsPtr bytes

applyAttributes :: SettingsPtr -> [Attribute] -> IO ()
applyAttributes _settingsPtr [] = pure ()
applyAttributes settingsPtr (attribute:remaining) = do
  useAttribute settingsPtr attribute
  applyAttributes settingsPtr remaining

run :: [Attribute] -> String -> model -> UpdateCallback model message -> ViewCallback model -> IO ()
run attributes title model update view = do
  titlePtr <- newCString title
  modelPtr <- newStablePtr model
  updatePtr <- makeUpdateCallback $ update_hs update
  viewPtr <- makeViewCallback $ view_hs view
  settingsPtr <- newSettings
  applyAttributes settingsPtr attributes
  run_app_ffi settingsPtr titlePtr modelPtr updatePtr viewPtr
