module Iced.Advanced.Svg (
  SvgPtr,
  svgNew,
  svgColor,
  svgOpacity,
  svgRotation,
) where

import Control.Monad
import Foreign
import Foreign.C.Types

import Iced.Advanced.Svg.Handle
import Iced.Attribute.Internal
import Iced.ColorFFI

data NativeSvg
type SvgPtr = Ptr NativeSvg

foreign import ccall "advanced_svg_new"
  svg_new :: Handle -> IO SvgPtr

foreign import ccall "advanced_svg_color"
  svg_color :: SvgPtr -> ColorPtr -> IO SvgPtr

foreign import ccall "advanced_svg_opacity"
  svg_opacity :: SvgPtr -> CFloat -> IO SvgPtr

foreign import ccall "advanced_svg_rotation"
  svg_rotation :: SvgPtr -> CFloat -> IO SvgPtr

svgNew :: Handle -> IO SvgPtr
svgNew = svg_new

svgColor :: SvgPtr -> Color -> IO SvgPtr
svgColor svg = svg_color svg <=< valueToNativeIO

svgOpacity :: SvgPtr -> Float -> IO SvgPtr
svgOpacity svg = svg_opacity svg . CFloat

svgRotation :: SvgPtr -> Float -> IO SvgPtr
svgRotation svg = svg_rotation svg . CFloat
