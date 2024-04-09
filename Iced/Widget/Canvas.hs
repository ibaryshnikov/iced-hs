{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas (
  canvas,
  CanvasState,
  newCanvasState,
  clearCanvasCache,
  freeCanvasState,
) where

import Foreign
import System.IO.Unsafe -- to run clearCache without IO

import Iced.Element
import Iced.Length
import Iced.LengthFFI
import Iced.Widget.Canvas.Path
import Iced.Widget.Canvas.Shape

data NativeCanvasState
type CanvasStatePtr = Ptr NativeCanvasState
type CanvasState = CanvasStatePtr; -- to export
data NativeCanvas
type SelfPtr = Ptr NativeCanvas
type AttributeFn = SelfPtr -> IO SelfPtr
data NativeFrame
type FramePtr = Ptr NativeFrame

data Attribute = Width Length | Height Length

foreign import ccall safe "canvas_state_new"
  canvas_state_new :: IO (CanvasStatePtr)

foreign import ccall safe "canvas_set_draw"
  canvas_set_draw :: CanvasStatePtr -> FunPtr (NativeDraw) -> IO ()

foreign import ccall safe "canvas_remove_draw"
  canvas_remove_draw :: CanvasStatePtr -> IO ()

foreign import ccall safe "canvas_clear_cache"
  canvas_clear_cache :: CanvasStatePtr -> IO ()

foreign import ccall safe "canvas_state_free"
  canvas_state_free :: CanvasStatePtr -> IO ()

foreign import ccall safe "canvas_new"
  canvas_new :: CanvasStatePtr -> IO (SelfPtr)

foreign import ccall safe "canvas_width"
  canvas_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "canvas_height"
  canvas_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "canvas_into_element"
  canvas_into_element :: SelfPtr -> IO (ElementPtr)

foreign import ccall safe "frame_fill"
  frame_fill :: FramePtr -> PathPtr -> IO ()

type NativeDraw = FramePtr -> IO ()
foreign import ccall "wrapper"
  makeDrawCallback :: NativeDraw -> IO (FunPtr (NativeDraw))

data Canvas = Canvas {
  attributes :: [Attribute],
  shapes :: [Shape],
  cache :: CanvasStatePtr
}

drawCallback :: [Shape] -> NativeDraw
drawCallback shapes framePtr = do
  pathPtr <- newPath shapes
  frame_fill framePtr pathPtr

instance IntoNative Canvas where
  toNative details = do
    drawCallbackPtr <- makeDrawCallback $ drawCallback details.shapes
    canvas_set_draw details.cache drawCallbackPtr
    selfPtr <- canvas_new details.cache
    updatedSelf <- applyAttributes selfPtr details.attributes
    canvas_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      Width len -> useWidth len selfPtr
      Height len -> useHeight len selfPtr

instance UseWidth Attribute where
  width len = Width len

instance UseHeight Attribute where
  height len = Height len

canvas :: [Attribute] -> [Shape] -> CanvasStatePtr -> Element
canvas attributes shapes cache = pack Canvas { .. }

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  canvas_width selfPtr nativeLen

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  canvas_height selfPtr nativeLen

newCanvasState :: IO (CanvasStatePtr)
newCanvasState = canvas_state_new

clearCanvasCache :: CanvasStatePtr -> CanvasStatePtr
clearCanvasCache state = unsafePerformIO $ do -- todo: make it a Command probably?
  canvas_clear_cache state
  return state

freeCanvasState :: CanvasStatePtr -> IO ()
freeCanvasState state = canvas_state_free state
