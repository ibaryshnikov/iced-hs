module Iced.Attribute.Internal where

import Foreign.C.Types

class UseAttribute widget attribute where
  useAttribute :: attribute -> widget -> IO widget

class ValueToNative value native where
  valueToNative :: value -> native

instance ValueToNative Int CInt where
  valueToNative = fromIntegral

instance ValueToNative Float CFloat where
  valueToNative = CFloat

useFn :: ValueToNative value native
      => (self -> native -> IO self)
      -> value
      -> self
      -> IO self
useFn fn value self = fn self $ valueToNative value

class ValueToNativeIO value native where
  valueToNativeIO :: value -> IO native

useFnIO :: ValueToNativeIO value native
      => (self -> native -> IO self)
      -> value
      -> self
      -> IO self
useFnIO fn value self = fn self =<< valueToNativeIO value
