module Iced.Widget.Canvas.Stroke where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Widget.Canvas.Style

data LineCap
  = Butt -- 0
  | Square -- 1
  | RoundLineCap -- 2

instance ValueToNative LineCap CUChar where
  valueToNative lineCap = case lineCap of
    Butt -> 0
    Square -> 1
    RoundLineCap -> 2

data LineJoin
  = Miter -- 0
  | RoundLineJoin -- 1
  | Bevel -- 2

instance ValueToNative LineJoin CUChar where
  valueToNative lineJoin = case lineJoin of
    Miter -> 0
    RoundLineJoin -> 1
    Bevel -> 2

data NativeStroke
type StrokePtr = Ptr NativeStroke

-- style width line_cap line_join
foreign import ccall "canvas_stroke_new"
  stroke_new :: Style -> CFloat -> CUChar -> CUChar -> IO StrokePtr

newCanvasStroke :: Style -> Float -> LineCap -> LineJoin -> IO StrokePtr
newCanvasStroke style width lineCap lineJoin =
  stroke_new style (CFloat width) (valueToNative lineCap) (valueToNative lineJoin)
