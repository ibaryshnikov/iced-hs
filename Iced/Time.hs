module Iced.Time (
  Duration,
  durationFromSecs,
  durationFromMillis,
  delay,
  sleep,
  every,
  microsSinceStart,
) where

import Control.Monad
import Data.Word
import Foreign
import Foreign.C.Types

import Iced.Future
import Iced.Future.Internal
import Iced.Subscription

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

foreign import ccall "iced_time_every"
  iced_time_every :: Duration -> FunPtr (NativeOnEvery message) -> IO (Subscription message)

type NativeOnEvery message = CULong -> IO (StablePtr message)

foreign import ccall "wrapper"
  makeOnEveryCallback :: NativeOnEvery message -> IO (FunPtr (NativeOnEvery message))

wrapOnEvery :: OnEvery message -> NativeOnEvery message
wrapOnEvery callback = newStablePtr . callback . fromIntegral

type OnEvery message = Integer -> message

-- Every duration returns the number of microseconds since STARTED_AT static variable in Rust
-- STARTED_AT is initialised when first accessed
every :: IO Duration -> OnEvery message -> IO (Subscription message)
every ioDuration callback = do
  duration <- ioDuration
  callbackPtr <- makeOnEveryCallback $ wrapOnEvery callback
  iced_time_every duration callbackPtr

foreign import ccall "time_micros_since_start"
  micros_since_start :: IO CULong

-- Number of microseconds since STARTED_AT static variable in Rust
-- STARTED_AT is initialised when first accessed
microsSinceStart :: IO Integer
microsSinceStart = fmap fromIntegral micros_since_start
