module Iced.Widget.Canvas.Path (newPath, PathPtr) where

import Control.Monad
import Foreign

import Iced.Widget.Canvas.PathBuilder
import Iced.Widget.Canvas.Shape

data NativePath
type PathPtr = Ptr NativePath

foreign import ccall "path_new"
  path_new :: FunPtr (NativePathCallback) -> IO (PathPtr)

type NativePathCallback = PathBuilderPtr -> IO ()
foreign import ccall "wrapper"
  makeCallback :: NativePathCallback -> IO (FunPtr (NativePathCallback))

shapesToPath :: [Shape] -> PathBuilderPtr -> IO ()
shapesToPath [] _builder = pure ()
shapesToPath (shape : remaining) builder = do
  addToPath shape builder
  shapesToPath remaining builder

pathCallback :: [Shape] -> NativePathCallback
pathCallback shapes = shapesToPath shapes

newPath :: [Shape] -> IO (PathPtr)
newPath = path_new <=< makeCallback . pathCallback
