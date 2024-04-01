{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Canvas (canvas) where

import Foreign

import Iced.Element
import Iced.Widget.Canvas.Path
import Iced.Widget.Canvas.Shape

-- need to store it in the model somehow to use draw cache
--data NativeCanvasState
--type CanvasStatePtr = Ptr NativeCanvasState
data NativeCanvas
type SelfPtr = Ptr NativeCanvas
type Attribute = SelfPtr -> IO SelfPtr
data NativeFrame
type FramePtr = Ptr NativeFrame

foreign import ccall safe "new_canvas_state"
  new_canvas_state :: FunPtr (NativeDraw) -> IO (SelfPtr)

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "free_canvas_state"
--  free_canvas_state :: SelfPtr -> ()

foreign import ccall safe "canvas_view"
  canvas_view :: SelfPtr -> IO (ElementPtr)

foreign import ccall safe "frame_fill"
  frame_fill :: FramePtr -> PathPtr -> IO ()

type NativeDraw = FramePtr -> IO ()
foreign import ccall "wrapper"
  makeDrawCallback :: NativeDraw -> IO (FunPtr (NativeDraw))

data Canvas = Canvas { attributes :: [Attribute], shapes :: [Shape] }

drawCallback :: [Shape] -> NativeDraw
drawCallback shapes framePtr = do
  pathPtr <- newPath shapes
  frame_fill framePtr pathPtr

instance IntoNative Canvas where
  toNative details = do
    drawCallbackPtr <- makeDrawCallback $ drawCallback details.shapes
    -- need to store it in the model somehow to use draw cache
    selfPtr <- new_canvas_state drawCallbackPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    canvas_view updatedSelf

canvas :: [Attribute] -> [Shape] -> Element
canvas attributes shapes = pack Canvas { attributes = attributes, shapes = shapes }
