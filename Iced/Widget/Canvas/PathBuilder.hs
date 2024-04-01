module Iced.Widget.Canvas.PathBuilder (
    PathBuilderPtr,
    circle,
    lineTo,
    moveTo,
    rectangle,
) where

import Foreign
import Foreign.C.Types

data NativePathBuilder
type SelfPtr = Ptr NativePathBuilder
type PathBuilderPtr = SelfPtr

-- builder x y radius
foreign import ccall safe "path_builder_circle"
  path_builder_circle :: SelfPtr -> CFloat -> CFloat -> CFloat -> IO ()

circle :: SelfPtr -> Float -> Float -> Float -> IO ()
circle builder x y radius =
  path_builder_circle builder (CFloat x) (CFloat y) (CFloat radius)

-- builder x y
foreign import ccall safe "path_builder_line_to"
  path_builder_line_to :: SelfPtr -> CFloat -> CFloat -> IO ()

lineTo :: SelfPtr -> Float -> Float -> IO ()
lineTo builder x y = path_builder_line_to builder (CFloat x) (CFloat y)

-- builder x y
foreign import ccall safe "path_builder_move_to"
  path_builder_move_to :: SelfPtr -> CFloat -> CFloat -> IO ()

moveTo :: SelfPtr -> Float -> Float -> IO ()
moveTo builder x y = path_builder_move_to builder (CFloat x) (CFloat y)

-- builder top_left_x top_left_y width height
foreign import ccall safe "path_builder_rectangle"
  path_builder_rectangle :: SelfPtr -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

rectangle :: SelfPtr -> Float -> Float -> Float -> Float -> IO ()
rectangle builder top_left_x top_left_y width height =
  path_builder_rectangle builder (CFloat top_left_x) (CFloat top_left_y) (CFloat width) (CFloat height)
