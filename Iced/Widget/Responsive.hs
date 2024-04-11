{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Responsive (
  responsive,
) where

import Foreign
import Foreign.C.Types

import Iced.Element
import Iced.Size

foreign import ccall safe "responsive_new"
  responsive_new :: FunPtr (NativeView) -> IO (ElementPtr)

foreign import ccall "wrapper"
  makeCallback :: NativeView -> IO (FunPtr (NativeView))

type NativeView = CFloat -> CFloat -> IO (ElementPtr)

wrapView :: View -> NativeView
wrapView view (CFloat width) (CFloat height) = do
  elementToNative $ view Size { .. }

type View = Size -> Element

data Responsive = Responsive { view :: View }

instance IntoNative Responsive where
  toNative details = do
    viewPtr <- makeCallback $ wrapView details.view
    responsive_new viewPtr

responsive :: View -> Element
responsive view = pack Responsive { .. }
