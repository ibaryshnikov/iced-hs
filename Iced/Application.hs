module Iced.Application (
  run,
  addFont,
) where

import Data.Word
import Foreign
import Foreign.C.String

import Iced.Command
import Iced.Element (Element, ElementPtr, elementToNative)
import Iced.Future.Internal
import Iced.Settings

data Attribute = Font [Word8]

foreign import ccall safe "run_app"
  run_app :: SettingsPtr
          -> FunPtr (NativeTitle model)
          -> StablePtr model
          -> FunPtr (NativeUpdate model message)
          -> FunPtr (NativeView model)
          -> IO ()

type NativeTitle model = StablePtr model -> IO (CString)
foreign import ccall "wrapper"
  makeTitleCallback :: NativeTitle model -> IO (FunPtr (NativeTitle model))

wrapTitle :: IntoTitle title model
          => Title title
          -> NativeTitle model
wrapTitle title modelPtr = do
  model <- deRefStablePtr modelPtr
  newCString $ intoTitle title model

type NativeUpdate model message = StablePtr model -> StablePtr message -> IO (UpdateResultPtr)
foreign import ccall "wrapper"
  makeUpdateCallback :: NativeUpdate model message -> IO (FunPtr (NativeUpdate model message))

wrapUpdate :: IntoCommand model message result
           => Update model message result
           -> NativeUpdate model message
wrapUpdate update modelPtr messagePtr = do
  -- modelPtr is freed by Rust when it changes
  model <- deRefStablePtr modelPtr
  -- messagePtr is freed by Rust when
  -- the message is no longer in use.
  -- still need to take care of closures like Input String
  message <- deRefStablePtr messagePtr
  (newModel, command) <- intoCommand update model message
  packResult newModel command

packResult :: model -> Command message -> IO (UpdateResultPtr)
packResult model command = do
  modelPtr <- newStablePtr model
  resultPtr <- update_result_new modelPtr
  packCommand resultPtr command

packCommand :: UpdateResultPtr -> Command message -> IO (UpdateResultPtr)
packCommand resultPtr None = pure resultPtr
packCommand resultPtr (PerformIO callback) = do
  callbackPtr <- makeCommandPerformCallback $ wrapCommandPerform callback
  update_result_add_command_io resultPtr callbackPtr
  return resultPtr
packCommand resultPtr (Perform (Future ma)) = do
  update_result_add_command_future resultPtr =<< ma
  return resultPtr

data NativeUpdateResult
type UpdateResultPtr = Ptr NativeUpdateResult

foreign import ccall safe "update_result_new"
  update_result_new :: StablePtr model -> IO (UpdateResultPtr)

-- update_result future
foreign import ccall safe "update_result_add_command_future"
  update_result_add_command_future :: UpdateResultPtr -> FuturePtr message -> IO ()

-- update_result callback
foreign import ccall safe "update_result_add_command_io"
  update_result_add_command_io :: UpdateResultPtr-> FunPtr (NativeCommandPerform message) -> IO ()

type NativeCommandPerform message = IO (StablePtr message)
foreign import ccall "wrapper"
  makeCommandPerformCallback :: NativeCommandPerform message -> IO (FunPtr (NativeCommandPerform message))

wrapCommandPerform :: IO (message) -> NativeCommandPerform message
wrapCommandPerform callback = do
  message <- callback
  newStablePtr message

type NativeView model = StablePtr model -> IO (ElementPtr)
foreign import ccall "wrapper"
  makeViewCallback :: NativeView model -> IO (FunPtr (NativeView model))

wrapView :: View model -> NativeView model
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

type Title title = title
type Update model message result = model -> message -> result
type View model = model -> Element

--
-- Provides the following signatures of update function:
--
-- update :: Model -> Message -> Model
-- update :: Model -> Message -> IO Model
-- update :: Model -> Message -> (Model, Command)
-- update :: Model -> Message -> IO (Model, Command)
--
class IntoCommand model message result where
  intoCommand :: (model -> message -> result) -> model -> message -> IO (model, Command message)

instance IntoCommand model message model where
  intoCommand update model message = pure (update model message, None)

instance IntoCommand model message (IO model) where
  intoCommand update = (fmap (, None) .) . update

instance IntoCommand model message (model, Command message) where
  intoCommand update = (pure .) . update

instance IntoCommand model message (IO (model, Command message)) where
  intoCommand update = update

--
-- this class will allow two signatures
-- for title function:
-- title :: String
-- title :: Model -> String
--
class IntoTitle title model where
  intoTitle :: title -> model -> String

instance IntoTitle String model where
  intoTitle title _model = title

instance IntoTitle (model -> String) model where
  intoTitle title = title

run :: (IntoCommand model message result, IntoTitle title model)
    => [Attribute]
    -> Title title
    -> model
    -> Update model message result
    -> View model
    -> IO ()

run attributes title model update view = do
  titlePtr <- makeTitleCallback $ wrapTitle title
  modelPtr <- newStablePtr model
  updatePtr <- makeUpdateCallback $ wrapUpdate update
  viewPtr <- makeViewCallback $ wrapView view
  settingsPtr <- newSettings
  applyAttributes settingsPtr attributes
  run_app settingsPtr titlePtr modelPtr updatePtr viewPtr
  freeHaskellFunPtr titlePtr
  freeHaskellFunPtr updatePtr
  freeHaskellFunPtr viewPtr

addFont :: [Word8] -> Attribute
addFont bytes = Font bytes
