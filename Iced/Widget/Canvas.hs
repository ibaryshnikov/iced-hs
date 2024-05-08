{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Canvas (
  canvas,
  State,
  newState,
  clearCache,
) where

import Foreign

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element
import Iced.Widget.Canvas.Frame
import Iced.Widget.Canvas.FrameAction
import Iced.Widget.Canvas.FramePtr

data NativeState
type State = ForeignPtr NativeState
data NativeCanvas
type Self = Ptr NativeCanvas

data Attribute = Width Length | Height Length

foreign import ccall "canvas_state_new"
  state_new :: IO (Ptr NativeState)

newState :: IO State
newState = newForeignPtr state_free =<< state_new

foreign import ccall "canvas_set_draw"
  canvas_set_draw :: Ptr NativeState -> FunPtr NativeDraw -> IO ()

canvasSetDraw :: State -> FunPtr NativeDraw -> IO ()
canvasSetDraw state callback = withForeignPtr state $ \self ->
  canvas_set_draw self callback

foreign import ccall "canvas_remove_draw"
  canvas_remove_draw :: Ptr NativeState -> IO ()

foreign import ccall "canvas_clear_cache"
  clear_cache :: Ptr NativeState -> IO ()

clearCache :: State -> IO ()
clearCache state = withForeignPtr state clear_cache

foreign import ccall "&canvas_state_free"
  state_free :: FinalizerPtr NativeState

foreign import ccall "canvas_new"
  canvas_new :: Ptr NativeState -> IO Self

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
  cache :: State
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

canvas :: [Attribute] -> [FrameAction] -> State -> Element
canvas attributes actions cache = pack Canvas { .. } attributes
