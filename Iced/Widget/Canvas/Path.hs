module Iced.Widget.Canvas.Path (newPath, PathPtr) where

import Foreign

import Iced.Widget.Canvas.PathBuilder
import Iced.Widget.Canvas.Shape

data NativePath
type PathPtr = Ptr NativePath

foreign import ccall safe "new_path"
  new_path :: FunPtr (NativePathCallback) -> IO (PathPtr)

type NativePathCallback = PathBuilderPtr -> IO ()
foreign import ccall "wrapper"
  makeCallback :: NativePathCallback -> IO (FunPtr (NativePathCallback))

shapesToPath :: [Shape] -> PathBuilderPtr -> IO ()
shapesToPath [] _builder = return ()
shapesToPath (shape:remaining) builder = do
  addToPath shape builder
  shapesToPath remaining builder

pathCallback :: [Shape] -> NativePathCallback
pathCallback shapes = shapesToPath shapes

newPath :: [Shape] -> IO (PathPtr)
newPath shapes = do
  callbackPtr <- makeCallback $ pathCallback shapes
  new_path callbackPtr
