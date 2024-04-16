{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas.Fill (
  FillPtr,
  StylePtr,
  Rule(..),
  newCanvasFill,
  canvasStyleSolid,
) where

import Data.Word
import Foreign
import Foreign.C.Types

import Iced.Color

data NativeFill
type FillPtr = Ptr NativeFill
data NativeStyle
type StylePtr = Ptr NativeStyle

data Rule
  = NonZero -- 0
  | EvenOdd -- 1

foreign import ccall safe "canvas_fill_new"
  canvas_fill_new :: StylePtr -> CUChar -> IO (FillPtr)

-- color values are in range 0.0 - 1.0
-- r g b a
foreign import ccall safe "canvas_style_solid"
  canvas_style_solid :: CFloat -> CFloat -> CFloat -> CFloat -> IO (StylePtr)

ruleToNative :: Rule -> Word8
ruleToNative rule = case rule of
  NonZero -> 0
  EvenOdd -> 1

newCanvasFill :: StylePtr -> Rule -> IO (FillPtr)
newCanvasFill stylePtr rule = canvas_fill_new stylePtr ruleRaw
  where ruleRaw = CUChar $ ruleToNative rule

canvasStyleSolid :: Color -> IO (StylePtr)
canvasStyleSolid Color { .. } = canvas_style_solid (CFloat r) (CFloat g) (CFloat b) (CFloat a)
