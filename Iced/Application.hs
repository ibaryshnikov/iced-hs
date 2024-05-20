module Iced.Application (
  run,
  addFont,
  subscription,
) where

import Data.Word
import Foreign
import Foreign.C.String

import Iced.Command
import Iced.Element (Element, ElementPtr, elementToNative)
import Iced.Future.Internal
import Iced.Settings
import Iced.Subscription

data Attribute model message
  = Font [Word8]
  | AddSubscription (SubscriptionFn model message)

data NativeFlags
type Flags model message = Ptr NativeFlags

-- title model update view
foreign import ccall "app_flags_new"
  app_flags_new
    :: FunPtr (NativeTitle model)
    -> StablePtr model
    -> FunPtr (NativeUpdate model message)
    -> FunPtr (NativeView model)
    -> IO (Flags model message)

foreign import ccall "app_flags_set_subscription"
  app_flags_set_subscription
    :: Flags model message
    -> FunPtr (NativeSubscriptionFn model message)
    -> IO ()

foreign import ccall "app_run"
  app_run :: Flags model message -> SettingsPtr -> IO ()

type NativeTitle model = StablePtr model -> IO CString
foreign import ccall "wrapper"
  makeTitleCallback :: NativeTitle model -> IO (FunPtr (NativeTitle model))

wrapTitle :: IntoTitle title model
          => Title title
          -> NativeTitle model
wrapTitle title modelPtr = do
  model <- deRefStablePtr modelPtr
  newCString $ intoTitle title model

type NativeUpdate model message = StablePtr model -> StablePtr message -> IO UpdateResultPtr
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

packResult :: model -> Command message -> IO UpdateResultPtr
packResult model command = do
  modelPtr <- newStablePtr model
  resultPtr <- update_result_new modelPtr
  packCommand resultPtr command

packCommand :: UpdateResultPtr -> Command message -> IO UpdateResultPtr
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

foreign import ccall "update_result_new"
  update_result_new :: StablePtr model -> IO UpdateResultPtr

-- update_result future
foreign import ccall "update_result_add_command_future"
  update_result_add_command_future :: UpdateResultPtr -> FuturePtr message -> IO ()

-- update_result callback
foreign import ccall "update_result_add_command_io"
  update_result_add_command_io :: UpdateResultPtr-> FunPtr (NativeCommandPerform message) -> IO ()

type NativeCommandPerform message = IO (StablePtr message)
foreign import ccall "wrapper"
  makeCommandPerformCallback :: NativeCommandPerform message -> IO (FunPtr (NativeCommandPerform message))

wrapCommandPerform :: IO message -> NativeCommandPerform message
wrapCommandPerform callback = do
  message <- callback
  newStablePtr message

type NativeView model = StablePtr model -> IO ElementPtr
foreign import ccall "wrapper"
  makeViewCallback :: NativeView model -> IO (FunPtr (NativeView model))

wrapView :: View model -> NativeView model
wrapView view modelPtr = do
  model <- deRefStablePtr modelPtr
  elementToNative $ view model

type NativeSubscriptionFn model message = StablePtr model -> IO (Subscription message)

foreign import ccall "wrapper" makeSubscriptionCallback
  :: NativeSubscriptionFn model message
  -> IO (FunPtr (NativeSubscriptionFn model message))

wrapSubscriptionFn :: SubscriptionFn model message -> NativeSubscriptionFn model message
wrapSubscriptionFn callback modelPtr = do
  model <- deRefStablePtr modelPtr
  callback model

useAttribute :: Flags model message -> SettingsPtr -> Attribute model message -> IO ()
useAttribute flagsPtr settingsPtr attribute = do
  case attribute of
    Font bytes -> useFont settingsPtr bytes
    AddSubscription subscriptionFn -> do
      makeSubscriptionCallback (wrapSubscriptionFn subscriptionFn)
        >>= app_flags_set_subscription flagsPtr

applyAttributes :: Flags model message -> SettingsPtr -> [Attribute model message] -> IO ()
applyAttributes _flagsPtr _settingsPtr [] = pure ()
applyAttributes flagsPtr settingsPtr (attribute:remaining) = do
  useAttribute flagsPtr settingsPtr attribute
  applyAttributes flagsPtr settingsPtr remaining

type Title title = title
type Update model message result = model -> message -> result
type View model = model -> Element

--
-- Provides the following signatures:
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
-- Provides the following signatures:
--
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
    => [Attribute model message]
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
  flagsPtr <- app_flags_new titlePtr modelPtr updatePtr viewPtr
  settingsPtr <- newSettings
  applyAttributes flagsPtr settingsPtr attributes
  app_run flagsPtr settingsPtr
  freeHaskellFunPtr titlePtr
  freeHaskellFunPtr updatePtr
  freeHaskellFunPtr viewPtr

addFont :: [Word8] -> Attribute model message
addFont bytes = Font bytes

type SubscriptionFn model message = model -> IO (Subscription message)

subscription :: SubscriptionFn model message -> Attribute model message
subscription = AddSubscription
