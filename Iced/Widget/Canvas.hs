{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas (
  canvas,
  CanvasState,
  newCanvasState,
  clearCanvasCache,
) where

import Foreign

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element
import Iced.Widget.Canvas.Frame
import Iced.Widget.Canvas.FrameAction
import Iced.Widget.Canvas.FramePtr

data NativeCanvasState
type CanvasState = ForeignPtr NativeCanvasState
data NativeCanvas
type Self = Ptr NativeCanvas

data Attribute = Width Length | Height Length

foreign import ccall "canvas_state_new"
  canvas_state_new :: IO (Ptr NativeCanvasState)

newCanvasState :: IO CanvasState
newCanvasState = newForeignPtr canvas_state_free =<< canvas_state_new

foreign import ccall "canvas_set_draw"
  canvas_set_draw :: Ptr NativeCanvasState -> FunPtr NativeDraw -> IO ()

canvasSetDraw :: CanvasState -> FunPtr NativeDraw -> IO ()
canvasSetDraw state callback = withForeignPtr state $ \self ->
  canvas_set_draw self callback

foreign import ccall "canvas_remove_draw"
  canvas_remove_draw :: Ptr NativeCanvasState -> IO ()

foreign import ccall "canvas_clear_cache"
  canvas_clear_cache :: Ptr NativeCanvasState -> IO ()

clearCanvasCache :: CanvasState -> IO ()
clearCanvasCache state = withForeignPtr state canvas_clear_cache

foreign import ccall "&canvas_state_free"
  canvas_state_free :: FinalizerPtr NativeCanvasState

foreign import ccall "canvas_new"
  canvas_new :: Ptr NativeCanvasState -> IO Self

foreign import ccall "canvas_width"
  canvas_width :: Self -> LengthPtr -> IO Self

foreign import ccall "canvas_height"
  canvas_height :: Self -> LengthPtr -> IO Self

foreign import ccall "canvas_into_element"
  into_element :: Self -> IO ElementPtr

type NativeDraw = FramePtr -> IO ()
foreign import ccall "wrapper"
  makeDrawCallback :: NativeDraw -> IO (FunPtr (NativeDraw))

data Canvas = Canvas {
  actions :: [FrameAction],
  cache :: CanvasState
}

drawActions :: [FrameAction] -> FramePtr -> IO ()
drawActions [] _framePtr = pure ()
drawActions (action:remaining) framePtr = do
  case action of
    FrameFill shapes color -> frameFill framePtr shapes color
  drawActions remaining framePtr

drawCallback :: [FrameAction] -> NativeDraw
drawCallback actions framePtr = drawActions actions framePtr

instance Builder Self where
  build = into_element

instance IntoNative Canvas Self where
  toNative details = do
    drawCallbackPtr <- makeDrawCallback $ drawCallback details.actions
    canvasSetDraw details.cache drawCallbackPtr
    withForeignPtr details.cache canvas_new

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Width  len -> useFnIO canvas_width  len
    Height len -> useFnIO canvas_height len

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

canvas :: [Attribute] -> [FrameAction] -> CanvasState -> Element
canvas attributes actions cache = pack Canvas { .. } attributes
