module Iced.Attribute.AlignmentFFI where

import Foreign

import Iced.Attribute.Alignment
import Iced.Attribute.Internal

data NativeAlignment
type AlignmentPtr = Ptr NativeAlignment

foreign import ccall "alignment_start"
  alignment_start :: IO AlignmentPtr

foreign import ccall "alignment_center"
  alignment_center :: IO AlignmentPtr

foreign import ccall "alignment_end"
  alignment_end :: IO AlignmentPtr

instance ValueToNativeIO Alignment AlignmentPtr where
  valueToNativeIO value = case value of
    Start -> alignment_start
    Center -> alignment_center
    End -> alignment_end
