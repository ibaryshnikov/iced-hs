module Iced.Attribute.AlignmentFFI where

import Foreign

import Iced.Attribute.Alignment

data NativeAlignment
type AlignmentPtr = Ptr NativeAlignment

foreign import ccall safe "alignment_start"
  alignment_start :: AlignmentPtr

foreign import ccall safe "alignment_center"
  alignment_center :: AlignmentPtr

foreign import ccall safe "alignment_end"
  alignment_end :: AlignmentPtr

alignmentToNative :: Alignment -> AlignmentPtr
alignmentToNative value = case value of
  Start -> alignment_start
  Center -> alignment_center
  End -> alignment_end
