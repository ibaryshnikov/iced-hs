module Iced.Time (
  Duration,
  durationFromSecs,
  durationFromMillis,
  delay,
  sleep,
) where

import Control.Monad
import Foreign
import Foreign.C.Types

import Iced.Future
import Iced.Future.Internal

data NativeDuration
type Duration = Ptr NativeDuration

foreign import ccall "duration_from_secs"
  duration_from_secs :: CULong -> IO (Duration)

durationFromSecs :: Int -> IO (Duration)
durationFromSecs = duration_from_secs . fromIntegral

foreign import ccall "duration_from_millis"
  duration_from_millis :: CULong -> IO (Duration)

durationFromMillis :: Int -> IO (Duration)
durationFromMillis = duration_from_millis . fromIntegral

foreign import ccall "tokio_time_sleep"
  tokio_time_sleep :: Duration -> IO (FuturePtr ())

makeDelay :: Int -> IO (FuturePtr ())
makeDelay = tokio_time_sleep <=< duration_from_secs . fromIntegral

delay :: Int -> Future ()
delay n = Future (makeDelay n)

sleep :: Duration -> Future ()
sleep = Future . tokio_time_sleep
