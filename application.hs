module Application where

import Foreign
import Foreign.StablePtr (newStablePtr, deRefStablePtr)

import Widgets

foreign import ccall "wrapper"
  makeUpdateCallback :: Update model message -> IO (FunPtr (Update model message))
foreign import ccall "wrapper"
  makeViewCallback :: View model -> IO (FunPtr (View model))
foreign import ccall safe "run_app"
  runAppFfi :: StablePtr model -> FunPtr (Update model message) -> FunPtr (View model) -> IO ()
type Update model message = StablePtr model -> StablePtr message -> IO (StablePtr model)
type View model = StablePtr model -> IO (Element)

type UpdateCallback model message = model -> message -> model
update_hs :: UpdateCallback model message -> StablePtr model -> StablePtr message -> IO (StablePtr model)
update_hs update model_ptr message_ptr = do
  model <- deRefStablePtr model_ptr
  message <- deRefStablePtr message_ptr
  newStablePtr $ update model message

type ViewCallback model = model -> IO (Element)
view_hs:: ViewCallback model -> StablePtr model -> IO (Element)
view_hs view model_ptr = do
  model <- deRefStablePtr model_ptr
  view model

run :: model -> UpdateCallback model message -> ViewCallback model -> IO ()
run model update_fn view_fn = do
  modelPtr <- newStablePtr model
  updatePtr <- makeUpdateCallback (\model message -> update_hs update_fn model message)
  viewPtr <- makeViewCallback (\model -> view_hs view_fn model)
  runAppFfi modelPtr updatePtr viewPtr
