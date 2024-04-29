module Iced.Time (
  Duration,
  durationFromSecs,
  durationFromMillis,
  delay,
  sleep,
) where

import Control.Monad
import Data.Word
import Foreign
import Foreign.C.Types

import Iced.Future
import Iced.Future.Internal

data NativeDuration
type Duration = Ptr NativeDuration

foreign import ccall "duration_from_secs"
  duration_from_secs :: CULong -> IO Duration

durationFromSecs :: Word64 -> IO Duration
durationFromSecs = duration_from_secs . CULong

foreign import ccall "duration_from_millis"
  duration_from_millis :: CULong -> IO Duration

durationFromMillis :: Word64 -> IO Duration
durationFromMillis = duration_from_millis . CULong

foreign import ccall "tokio_time_sleep"
  tokio_time_sleep :: Duration -> IO (FuturePtr ())

makeDelay :: Word64 -> IO (FuturePtr ())
makeDelay = tokio_time_sleep <=< duration_from_secs . CULong

delay :: Word64 -> Future ()
delay n = Future (makeDelay n)

sleep :: IO Duration -> Future ()
sleep duration = Future $ tokio_time_sleep =<< duration
