{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Responsive (
  responsive,
) where

import Foreign
import Foreign.C.Types

import Iced.Element
import Iced.Size

foreign import ccall "responsive_new"
  responsive_new :: FunPtr (NativeView) -> IO ElementPtr

foreign import ccall "wrapper"
  makeCallback :: NativeView -> IO (FunPtr (NativeView))

type NativeView = CFloat -> CFloat -> IO ElementPtr

wrapView :: View -> NativeView
wrapView view (CFloat width) (CFloat height) = elementToNative $ view Size{..}

type View = Size -> Element

data Responsive = Responsive {view :: View}

instance IntoNative Responsive ElementPtr where
  toNative details =
    makeCallback (wrapView details.view)
      >>= responsive_new

responsive :: View -> Element
responsive view = packSimple Responsive{..}
