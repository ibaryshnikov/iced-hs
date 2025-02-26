module Iced.Time (
  delaySecs,
  delayMillis,
  delayMicros,
  delayNanos,
  everySecs,
  everyMillis,
  everyMicros,
  everyNanos,
  microsSinceStart,
) where

import Data.Word
import Foreign
import Foreign.C.Types

import Iced.Future
import Iced.Future.Internal
import Iced.Subscription
import Iced.Time.Duration

foreign import ccall "tokio_time_sleep"
  sleep :: Duration -> IO (FuturePtr ())

delay :: IO Duration -> Future ()
delay duration = Future $ sleep =<< duration

delaySecs :: Word64 -> Future ()
delaySecs = delay . fromSecs

delayMillis :: Word64 -> Future ()
delayMillis = delay . fromMillis

delayMicros :: Word64 -> Future ()
delayMicros = delay . fromMicros

delayNanos :: Word64 -> Future ()
delayNanos = delay . fromNanos

foreign import ccall "iced_time_every"
  time_every :: Duration -> FunPtr (NativeOnEvery message) -> IO (Subscription message)

type NativeOnEvery message = CULLong -> IO (StablePtr message)

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
  time_every duration callbackPtr

everySecs :: Word64 -> OnEvery message -> IO (Subscription message)
everySecs n = every (fromSecs n)

everyMillis :: Word64 -> OnEvery message -> IO (Subscription message)
everyMillis n = every (fromMillis n)

everyMicros :: Word64 -> OnEvery message -> IO (Subscription message)
everyMicros n = every (fromMicros n)

everyNanos :: Word64 -> OnEvery message -> IO (Subscription message)
everyNanos n = every (fromNanos n)

foreign import ccall "time_micros_since_start"
  micros_since_start :: IO CULLong

-- Number of microseconds since STARTED_AT static variable in Rust
-- STARTED_AT is initialised when first accessed
microsSinceStart :: IO Integer
microsSinceStart = fmap fromIntegral micros_since_start
