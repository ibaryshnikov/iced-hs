module Iced.Widget.Canvas.Fill (
  FillPtr,
  Rule(..),
  newCanvasFill,
) where

import Data.Word
import Foreign
import Foreign.C.Types

import Iced.Widget.Canvas.Style

data NativeFill
type FillPtr = Ptr NativeFill

data Rule
  = NonZero -- 0
  | EvenOdd -- 1

ruleToNative :: Rule -> Word8
ruleToNative rule = case rule of
  NonZero -> 0
  EvenOdd -> 1

foreign import ccall "canvas_fill_new"
  fill_new :: Style -> CUChar -> IO FillPtr

newCanvasFill :: Style -> Rule -> IO FillPtr
newCanvasFill style = fill_new style . CUChar . ruleToNative
