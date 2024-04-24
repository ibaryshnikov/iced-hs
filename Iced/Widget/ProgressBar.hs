{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.ProgressBar (progressBar) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeProgressBar
type Self = Ptr NativeProgressBar

data Attribute = Width Length | Height Length

-- range_from range_to value
foreign import ccall safe "progress_bar_new"
  progress_bar_new :: CFloat -> CFloat -> CFloat -> IO Self

foreign import ccall safe "progress_bar_width"
  progress_bar_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "progress_bar_height"
  progress_bar_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "progress_bar_into_element"
  into_element :: Self -> IO ElementPtr

data ProgressBar = ProgressBar {
  rangeFrom :: Float,
  rangeTo :: Float,
  value :: Float
}

instance Builder Self where
  build = into_element

instance IntoNative ProgressBar Self where
  toNative details = progress_bar_new rangeFrom rangeTo value
    where
      rangeFrom = CFloat details.rangeFrom
      rangeTo = CFloat details.rangeTo
      value = CFloat details.value

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Width  len -> useFn progress_bar_width  len
    Height len -> useFn progress_bar_height len

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

progressBar :: [Attribute] -> Float -> Float -> Float -> Element
progressBar attributes rangeFrom rangeTo value = pack ProgressBar { .. } attributes
