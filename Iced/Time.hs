module Iced.Time (
  Duration,
  Future,
  durationFromSecs,
  durationFromMillis,
  sleep,
  Sleep(..),
  IntoFuture,
  intoFuture,
) where

import Data.Word
import Foreign
import Foreign.C.Types

data NativeDuration
type DurationPtr = Ptr NativeDuration
type Duration = DurationPtr -- for export

data NativeFuture
type FuturePtr = Ptr NativeFuture
type Future = FuturePtr -- for export

-- currently the only Future here, but
-- will be a packed type later
data Sleep message = Sleep Duration message

class IntoFuture a where
  intoFuture :: a -> IO (Future)

instance IntoFuture (Sleep message) where
  intoFuture (Sleep duration message) = do
    messagePtr <- newStablePtr message
    tokio_time_sleep duration messagePtr

foreign import ccall safe "duration_from_secs"
  duration_from_secs :: CULong -> DurationPtr

durationFromSecs :: Word64 -> DurationPtr
durationFromSecs value = duration_from_secs (CULong value)

foreign import ccall safe "duration_from_millis"
  duration_from_millis :: CULong -> DurationPtr

durationFromMillis :: Word64 -> DurationPtr
durationFromMillis value = duration_from_millis (CULong value)

foreign import ccall safe "tokio_time_sleep"
  tokio_time_sleep :: DurationPtr -> StablePtr a -> IO (FuturePtr)

sleep :: DurationPtr -> message -> Sleep message
sleep duration message = Sleep duration message
