module Application where

import Foreign
import Foreign.StablePtr

import Widgets

type Update model message = StablePtr model -> StablePtr message -> IO (StablePtr model)

type View model = StablePtr model -> IO (Element)

foreign import ccall "wrapper"
  makeUpdateCallback :: Update model message -> IO (FunPtr (Update model message))

foreign import ccall "wrapper"
  makeViewCallback :: View model -> IO (FunPtr (View model))

foreign import ccall safe "run_app"
  runAppFfi :: StablePtr model -> FunPtr (Update model message) -> FunPtr (View model) -> IO ()

type UpdateCallback model message = model -> message -> model

update_hs :: UpdateCallback model message -> StablePtr model -> StablePtr message -> IO (StablePtr model)
update_hs update model_ptr message_ptr = do
  model <- deRefStablePtr model_ptr
  message <- deRefStablePtr message_ptr
  newStablePtr $ update model message

type ViewCallback model = model -> IO (Element)

view_hs :: ViewCallback model -> StablePtr model -> IO (Element)
view_hs view model_ptr = do
  model <- deRefStablePtr model_ptr
  view model

run :: model -> UpdateCallback model message -> ViewCallback model -> IO ()
run model update_fn view_fn = do
  modelPtr <- newStablePtr model
  updatePtr <- makeUpdateCallback $ update_hs update_fn
  viewPtr <- makeViewCallback $ view_hs view_fn
  runAppFfi modelPtr updatePtr viewPtr
