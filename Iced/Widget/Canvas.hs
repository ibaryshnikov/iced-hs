{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas (
  canvas,
  newCache,
  clearCache,
  CanvasCache,
) where

import Foreign
import System.IO.Unsafe -- to run clearCache without IO

import Iced.Element
import Iced.Widget.Canvas.Path
import Iced.Widget.Canvas.Shape

-- need to store it in the model somehow to use draw cache
--data NativeCanvasState
--type CanvasStatePtr = Ptr NativeCanvasState
data NativeCanvas
type SelfPtr = Ptr NativeCanvas
type CanvasCache = SelfPtr;
type Attribute = SelfPtr -> IO SelfPtr
data NativeFrame
type FramePtr = Ptr NativeFrame

foreign import ccall safe "new_canvas_state"
  new_canvas_state :: SelfPtr

foreign import ccall safe "canvas_set_draw"
  canvas_set_draw :: SelfPtr -> FunPtr (NativeDraw) -> IO ()

foreign import ccall safe "canvas_remove_draw"
  canvas_remove_draw :: SelfPtr -> IO ()

foreign import ccall safe "canvas_clear_cache"
  canvas_clear_cache :: SelfPtr -> IO ()

foreign import ccall safe "free_canvas_state"
  free_canvas_state :: SelfPtr -> IO ()

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

data Canvas = Canvas {
  attributes :: [Attribute],
  shapes :: [Shape],
  cache :: CanvasCache
}

drawCallback :: [Shape] -> NativeDraw
drawCallback shapes framePtr = do
  pathPtr <- newPath shapes
  frame_fill framePtr pathPtr

instance IntoNative Canvas where
  toNative details = do
    drawCallbackPtr <- makeDrawCallback $ drawCallback details.shapes
    canvas_set_draw details.cache drawCallbackPtr
    updatedSelf <- applyAttributes details.cache details.attributes
    canvas_view updatedSelf

newCache :: CanvasCache
newCache = new_canvas_state

clearCache :: CanvasCache -> CanvasCache
clearCache cache = unsafePerformIO $ do -- todo: make it a Command probably?
  canvas_clear_cache cache
  return cache

canvas :: [Attribute] -> [Shape] -> CanvasCache -> Element
canvas attributes shapes cache = pack Canvas { .. }
