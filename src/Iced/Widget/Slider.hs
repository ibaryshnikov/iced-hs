{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Slider (
  slider,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.OnRelease
import Iced.Attribute.SliderCommon
import Iced.Element

data NativeSlider
type Self = Ptr NativeSlider
type AttributeFn = Self -> IO Self

data Attribute message
  = OnRelease message
  | AddDefault Int
  | AddStep Int
  | AddShiftStep Int
  | Width Length
  | Height Float

-- range_from range_to value on_change
foreign import ccall "slider_new"
  slider_new :: CInt -> CInt -> CInt -> FunPtr (NativeOnChange a) -> IO Self

foreign import ccall "slider_default"
  slider_default :: Self -> CInt -> IO Self

foreign import ccall "slider_on_release"
  slider_on_release :: Self -> StablePtr a -> IO Self

foreign import ccall "slider_step"
  slider_step :: Self -> CInt -> IO Self

foreign import ccall "slider_shift_step"
  slider_shift_step :: Self -> CInt -> IO Self

foreign import ccall "slider_width"
  slider_width :: Self -> LengthPtr -> IO Self

foreign import ccall "slider_height"
  slider_height :: Self -> CFloat -> IO Self

foreign import ccall "slider_into_element"
  into_element :: Self -> IO ElementPtr

type NativeOnChange message = CInt -> IO (StablePtr message)
foreign import ccall "wrapper"
  makeCallback :: NativeOnChange message -> IO (FunPtr (NativeOnChange message))

wrapOnChange :: OnChange message -> NativeOnChange message
wrapOnChange callback = newStablePtr . callback . fromIntegral

type OnChange message = Int -> message

data Slider message = Slider
  { rangeFrom :: Int
  , rangeTo :: Int
  , value :: Int
  , onChange :: OnChange message
  }

instance Builder Self where
  build = into_element

instance IntoNative (Slider message) Self where
  toNative details = do
    let rangeFrom = fromIntegral details.rangeFrom
    let rangeTo = fromIntegral details.rangeTo
    let value = fromIntegral details.value
    onChange <- makeCallback $ wrapOnChange details.onChange
    slider_new rangeFrom rangeTo value onChange

instance UseAttribute Self (Attribute message) where
  useAttribute attribute = case attribute of
    AddDefault value -> useFn slider_default value
    OnRelease message -> useOnRelease message
    AddStep value -> useFn slider_step value
    AddShiftStep value -> useFn slider_shift_step value
    Width len -> useFnIO slider_width len
    Height len -> useFn slider_height len

instance SliderCommon (Attribute message) where
  addDefault = AddDefault
  step = AddStep
  shiftStep = AddShiftStep

instance UseOnRelease message (Attribute message) where
  onRelease = OnRelease

instance UseWidth Length (Attribute message) where
  width = Width

instance UseHeight Float (Attribute message) where
  height = Height

slider :: [Attribute message] -> Int -> Int -> Int -> OnChange message -> Element
slider attributes rangeFrom rangeTo value onChange = pack Slider{..} attributes

useOnRelease :: message -> AttributeFn
useOnRelease message self =
  newStablePtr message
    >>= slider_on_release self
