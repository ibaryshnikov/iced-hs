module Iced.Attribute.Internal where

import Foreign.C.Types

class UseAttribute widget attribute where
  useAttribute :: attribute -> widget -> IO widget

class UseStyleAttribute appearance attribute where
  useStyleAttribute :: attribute -> appearance -> IO ()

applyStyles
  :: UseStyleAttribute appearance attribute => [attribute] -> appearance -> IO ()
applyStyles [] _ = pure ()
applyStyles (attribute : remaining) appearance = do
  useStyleAttribute attribute appearance
  applyStyles remaining appearance

class ValueToNative value native where
  valueToNative :: value -> native

instance ValueToNative Int CInt where
  valueToNative = fromIntegral

instance ValueToNative Float CFloat where
  valueToNative = CFloat

useFn
  :: ValueToNative value native
  => (self -> native -> IO result)
  -> value
  -> self
  -> IO result
useFn fn value self = fn self $ valueToNative value

class ValueToNativeIO value native where
  valueToNativeIO :: value -> IO native

useFnIO
  :: ValueToNativeIO value native
  => (self -> native -> IO result)
  -> value
  -> self
  -> IO result
useFnIO fn value self = fn self =<< valueToNativeIO value
