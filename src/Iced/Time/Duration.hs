module Iced.Time.Duration (
  fromSecs,
  fromMillis,
  fromMicros,
  fromNanos,
  Duration,
) where

import Data.Word
import Foreign
import Foreign.C.Types

-- A wrapper around Rust std::time::Duration
data NativeDuration
type Duration = Ptr NativeDuration

foreign import ccall "duration_from_secs"
  from_secs :: CULLong -> IO Duration

fromSecs :: Word64 -> IO Duration
fromSecs = from_secs . CULLong

foreign import ccall "duration_from_millis"
  from_millis :: CULLong -> IO Duration

fromMillis :: Word64 -> IO Duration
fromMillis = from_millis . CULLong

foreign import ccall "duration_from_micros"
  from_micros :: CULLong -> IO Duration

fromMicros :: Word64 -> IO Duration
fromMicros = from_micros . CULLong

foreign import ccall "duration_from_nanos"
  from_nanos :: CULLong -> IO Duration

fromNanos :: Word64 -> IO Duration
fromNanos = from_nanos . CULLong
