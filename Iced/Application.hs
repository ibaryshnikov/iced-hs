module Iced.Application (run) where

import Foreign
import Foreign.C.String

import Iced.Element

type Update model message = StablePtr model -> StablePtr message -> IO (StablePtr model)

type View model = StablePtr model -> IO (ElementPtr)

foreign import ccall "wrapper"
  makeUpdateCallback :: Update model message -> IO (FunPtr (Update model message))

foreign import ccall "wrapper"
  makeViewCallback :: View model -> IO (FunPtr (View model))

foreign import ccall safe "run_app" run_app_ffi ::
  CString -> StablePtr model -> FunPtr (Update model message) -> FunPtr (View model) -> IO ()

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

run :: String -> model -> UpdateCallback model message -> ViewCallback model -> IO ()
run title model update view = do
  titlePtr <- newCString title
  modelPtr <- newStablePtr model
  updatePtr <- makeUpdateCallback $ update_hs update
  viewPtr <- makeViewCallback $ view_hs view
  run_app_ffi titlePtr modelPtr updatePtr viewPtr
