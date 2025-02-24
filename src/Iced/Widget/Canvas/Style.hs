{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas.Style (
  Style,
  solid,
) where

import Foreign
import Foreign.C.Types

import Iced.Color

data NativeStyle
type Style = Ptr NativeStyle

-- color values are in range 0.0 - 1.0
-- r g b a
foreign import ccall "canvas_style_solid"
  style_solid :: CFloat -> CFloat -> CFloat -> CFloat -> IO Style

solid :: Color -> IO Style
solid Color{..} = style_solid (CFloat r) (CFloat g) (CFloat b) (CFloat a)
